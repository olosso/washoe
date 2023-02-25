mod tests;

use std::fmt;

use crate::ast::*;
use crate::object::*;
use crate::token::{Token, TokenType};
use std::collections::HashMap;
use std::rc::Rc;

pub fn eval(program: &Program, global: &Rc<Environment>) -> Result<Object, EvalError> {
    let mut value = Ok(Object::Null);
    for statement in program.statements.iter() {
        value = match statement {
            Statement::Expr(_, expr) => eval_expression(expr, global),
            Statement::Return(_, expr) => eval_return(expr, global),
            Statement::Let(_, name, expr) => eval_let(name, expr, global), // NOTE Only let is allowed to mutate the environment.
            Statement::Block(_, statements) => eval_block(statements, global),
        };

        if let Ok(Object::Return(value)) = value {
            return Ok(*value);
        };
    }

    value
}

fn eval_statement(statement: &Statement, env: &Rc<Environment>) -> Result<Object, EvalError> {
    let mut value = Ok(Object::Null);
    value = match statement {
        Statement::Expr(_, expr) => eval_expression(expr, env),
        Statement::Return(_, expr) => eval_return(expr, env),
        Statement::Let(_, name, expr) => eval_let(name, expr, env), // NOTE Only let is allowed to mutate the environment.
        Statement::Block(_, statements) => eval_block(statements, env),
    };

    value
}

pub fn eval_block(statements: &[Statement], env: &Rc<Environment>) -> Result<Object, EvalError> {
    let mut result = Object::Null;
    // let block_env = Rc::new(Environment::local(env));
    for statement in statements {
        result = eval_statement(statement, env)?;
        /*
         * Note that the Return is not unwrapped!
         * It is propagated upwards to Program.eval() to
         * short-circuit it.
         */
        if let Object::Return(..) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_expression(expr: &Expression, env: &Rc<Environment>) -> Result<Object, EvalError> {
    match expr {
        Expression::Bool(_, b) => Ok(Object::Boolean(*b)),
        Expression::IntegerLiteral(_, i) => Ok(Object::Integer(*i)),
        Expression::StringLiteral(_, s) => Ok(Object::String(s.to_owned())),
        Expression::Array(_, exprs) => eval_array(exprs, env),
        Expression::HashMap(_, keys, vals) => eval_hashmap(keys, vals, env),
        Expression::Index(_, array, exprs) => eval_indexing(array, exprs, env),
        Expression::Prefix(_, op, expr) => eval_prefix(expr, op, env),
        Expression::Infix(_, left, op, right) => eval_infix(left, op, right, env),
        Expression::Identifier(_, ident) => eval_identifier(ident, env),
        Expression::If(_, cond, cons, alt) => eval_ifelse(cond, cons, alt, env),
        Expression::Func(_, params, body) => eval_function(params, body, env),
        Expression::Call(_, ident, args) => eval_call(ident, args, env),
        _ => Ok(Object::Null),
    }
}

fn eval_expressions(exprs: &[Expression], env: &Rc<Environment>) -> Result<Vec<Object>, EvalError> {
    let mut results = vec![];
    for expr in exprs.iter() {
        results.push(eval_expression(expr, env)?);
    }

    Ok(results)
}

fn eval_array(exprs: &[Expression], env: &Rc<Environment>) -> Result<Object, EvalError> {
    let exprs = eval_expressions(exprs, env)?;
    Ok(Object::Array(exprs))
}

fn eval_hashmap(
    keys: &[Expression],
    values: &[Expression],
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    let mut hm = HashMap::new();
    for (k, v) in keys.iter().zip(values) {
        let key = eval_expression(k, env)?;
        if !matches!(
            key,
            Object::Integer(_) | Object::Boolean(_) | Object::String(_)
        ) {
            return Err(EvalError::new("Bad Hash key.".to_string()));
        }
        let val = eval_expression(v, env)?;
        hm.insert(key, val);
    }

    Ok(Object::HashMap(hm))
}

fn eval_indexing(
    arr: &Expression,
    i: &Expression,
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    let arr = eval_expression(arr, env)?;
    match arr {
        Object::Array(a) => {
            if let Object::Integer(int) = eval_expression(i, env)? {
                return Ok(a.get(int as usize).unwrap().copy());
            } else {
                return Err(EvalError::new("Index must an IntegerLiteral".to_string()));
            };
        }
        Object::HashMap(hm) => {
            let i = eval_expression(i, env)?;
            if (matches!(
                i,
                Object::Integer(_) | Object::Boolean(_) | Object::String(_)
            )) {
                let val = hm.get(&i);
                if let Some(x) = val {
                    return Ok(x.copy());
                } else {
                    return Ok(Object::Null);
                }
            } else {
                return Err(EvalError::new(
                    "Index must an Int, Bool or String.".to_string(),
                ));
            }
        }
        _ => (),
    }

    Err(EvalError::new(
        "This is not an Array or a HashMap 😠".to_string()),
    )
}

fn eval_call(
    ident: &Expression,
    args: &[Expression],
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    let args = eval_expressions(args, env)?;
    if Builtins::names().contains(&ident.token_literal().as_str()) {
        if let Object::Builtin(s, f) = Object::builtins(ident.token_literal().as_str()).unwrap() {
            return f(args);
        }
    }

    let func = eval_expression(ident, env)?;
    assert!(matches!(func, Object::Function(..)));
    apply_function(&func, &args)
}

fn apply_function(func: &Object, args: &[Object]) -> Result<Object, EvalError> {
    if let Object::Function(params, body, env) = func {
        for (param, arg) in params.iter().zip(args) {
            env.add(&param.to_string(), arg);
        }
        let env = Rc::new(env.clone());
        eval_statement(body, &env)
    } else {
        Err(EvalError::new("Couldn't apply function".to_string()))
    }
}

fn eval_function(
    params: &[Expression],
    body: &Statement,
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    let local = Environment::local(env);

    Ok(Object::Function(params.to_vec(), body.clone(), local))
}

fn eval_prefix(expr: &Expression, op: &str, env: &Rc<Environment>) -> Result<Object, EvalError> {
    match op {
        "-" => eval_minus(expr, env),
        "!" => eval_bang(expr, env),
        _ => panic!("Lexer shouldn't allow this to happen."),
    }
}

/*
 * MINUS
 */
pub fn eval_minus(expr: &Expression, env: &Rc<Environment>) -> Result<Object, EvalError> {
    match expr {
        // !int
        Expression::IntegerLiteral(_, i) => Ok(Object::Integer(-(*i))),
        _ => Err(EvalError::new_t(
            "Minus operator not supported for".to_string(),
            expr.token().clone(),
        )),
    }
}

/*
 * BANGBANG
 */
pub fn eval_bang(expr: &Expression, env: &Rc<Environment>) -> Result<Object, EvalError> {
    match eval_expression(expr, env) {
        Ok(Object::Boolean(b)) => Ok(Object::Boolean(!b)),
        Ok(Object::Null) => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn bang_error(expr: &Expression) -> EvalError {
    EvalError::new(format!(
        "Bang not supported for the value {:?}.",
        expr.token().token_type
    ))
}

/*
 * @INFIX
 */
pub fn eval_infix(
    left: &Expression,
    op: &str,
    right: &Expression,
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    let left = eval_expression(left, env)?;
    let right = eval_expression(right, env)?;
    match left {
        Object::Integer(a) => {
            if let Object::Integer(b) = right {
                eval_integer_infix(a, op, b)
            } else {
                Err(EvalError::new(format!(
                    "Arithmetic not supported between types {:?} and {:?}.",
                    left.obtype(),
                    right.obtype()
                )))
            }
        }
        Object::Boolean(a) => {
            if let Object::Boolean(b) = right {
                eval_bool_infix(a, op, b)
            } else {
                Ok(Object::Null)
            }
        }
        Object::String(a) => {
            if let Object::String(b) = right {
                eval_string_infix(a, op, b)
            } else {
                Err(EvalError::new(format!(
                    "Expected right operator to be a String, but got {:?}.",
                    right.obtype()
                )))
            }
        }
        _ => todo!(),
    }
}

fn eval_integer_infix(a: i32, op: &str, b: i32) -> Result<Object, EvalError> {
    Ok(match op {
        "+" => Object::Integer(a + b),
        "-" => Object::Integer(a - b),
        "*" => Object::Integer(a * b),
        "/" => Object::Integer(a / b),

        "==" => Object::Boolean(a == b),
        "!=" => Object::Boolean(a != b),
        "<" => Object::Boolean(a < b),
        ">" => Object::Boolean(a > b),

        _ => {
            return Err(EvalError(
                format!("Operator \"{op}\" not supported for integers"),
                None,
            ))
        }
    })
}

fn eval_string_infix(a: String, op: &str, b: String) -> Result<Object, EvalError> {
    Ok(match op {
        "+" => {
            let mut s = a;
            s.push_str(b.as_str());
            Object::String(s)
        }
        _ => {
            return Err(EvalError(
                format!("Operator \"{op}\" not supported for Strings"),
                None,
            ))
        }
    })
}

fn eval_bool_infix(a: bool, op: &str, b: bool) -> Result<Object, EvalError> {
    Ok(match op {
        "==" => Object::Boolean(a == b),
        "!=" => Object::Boolean(a != b),

        _ => {
            return Err(EvalError(
                format!("Operator '{op}' not supported for booleans."),
                None,
            ))
        }
    })
}

pub fn eval_return(expr: &Expression, env: &Rc<Environment>) -> Result<Object, EvalError> {
    Ok(Object::Return(Box::new(eval_expression(expr, env)?)))
}

/*
 * IfElse
 */
pub fn eval_ifelse(
    cond: &Expression,
    cons: &Statement,
    alt: &Option<Box<Statement>>,
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    if eval_expression(cond, env)?.as_bool() {
        if let Statement::Block(_, statements) = cons {
            eval_block(statements, env)
        } else {
            panic!("Lexer shouldn't allow a non-block statement in If");
        }
    } else {
        match alt {
            None => Ok(Object::Null),
            Some(alt) => {
                if let Statement::Block(_, statements) = &**alt {
                    eval_block(statements, env)
                } else {
                    panic!("Lexer shouldn't allow a non-block statement in If");
                }
            }
        }
    }
}

/*
 * Let
 */
pub fn eval_let(
    ident: &Expression,
    expr: &Expression,
    env: &Rc<Environment>,
) -> Result<Object, EvalError> {
    let value = eval_expression(expr, env)?;
    let name = ident.token_literal();
    env.add(&name, &value);

    Ok(value)
}

pub fn eval_identifier(name: &String, env: &Rc<Environment>) -> Result<Object, EvalError> {
    match env.get(name) {
        Some(value) => Ok(value.copy()),
        None => Err(EvalError::new(format!(
            "The symbol '{}' is not bound to any value.",
            &name
        ))),
    }
}

#[derive(Debug)]
pub struct EvalError(pub String, pub Option<Token>);
impl EvalError {
    fn msg(&self) -> String {
        match &self.1 {
            None => self.0.to_owned(),
            Some(t) => format!(
                "{}. Problem encountered at token {:?}",
                self.0.to_owned(),
                t
            ),
        }
    }

    pub fn new(s: String) -> Self {
        EvalError(s, None)
    }
    pub fn new_t(s: String, t: Token) -> Self {
        EvalError(s, Some(t))
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg())
    }
}
