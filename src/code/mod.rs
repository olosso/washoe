use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;
use std::ops::{Deref, DerefMut};

/*
 * @CODE::INSTRUCTIONS
 */
/// Containerr for insturctions in bytes
/// [0, 0, 1, 0, 0, 2, 0, 1, 0] Correspods to [Constant 1, Constant 2, Constant 256]
#[derive(Debug, Default, PartialEq)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new(v: Vec<u8>) -> Self {
        Instructions(v)
    }
}

/*
 * Whenever &Instructions.method() is called, and method isn't implemented for Instructions,
 * the compiler will automatically dereference it to give a &Vec<u8> to check if that has
 * the method.
 * Also, whenever a function has &Vec<u8> as an argument, we can pass a &Instructions.
 */
impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Display for Instructions {
    /*
     * Take a bunch of bytes, and turn them into
     * a human-readable format, like
     * [0, 0, 1] => "0000 OpConstant 1"
     */
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        let mut i = 0;
        while i < self.len() {
            let opcode = Opcode::try_from(self[i]).unwrap();
            let def = DEFINITIONS.get(&opcode).unwrap();
            let (operands, n) = read_operands(def, &self.0[i + 1..]);

            let operand_count = def.operand_widths.len();

            let msg = match operand_count {
                1 => {
                    format!("{} {}", def.name, operands[0])
                }
                _ => unreachable!(),
            };
            let msg = format!("{i:0>4} {msg}\n");
            s = s + &msg;

            i += 1 + n;
        }
        write!(f, "{s}")
    }
}

/*
 * @CODE::OPCODE
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)] // Forces Rust to store a Opcode in one byte
pub enum Opcode {
    Constant,
}

/*
 * Allow for transformation from u8 to Opcode. Note that it can fail, since not all u8's
 * correspond to some Opcode.
 * This automatically implements TryInto<Opcode> for u8.
 */
impl TryFrom<u8> for Opcode {
    type Error = &'static str;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
            0 => Ok(Self::Constant),
            _ => Err("No Opcode corresponding for byte found."),
        }
    }
}

/*
 * @CODE::DEFINITION
 * Opcode definition. Contains a printable name of the Opcode,
 * and the number and widths of its operands.
 */
pub struct Definition {
    name: &'static str,
    /*
     * An operation can have have 0...n operands.
     * This array contains the width of the i'th operand in bytes.
     * So [2,3] would mean that the first operand is 2 bytes, and the second operand 3 bytes wide.
     */
    operand_widths: &'static [u8],
}

/*
 * This HashMap contains all supported Opcodes and their operand widths.
 */
lazy_static! {
    pub static ref DEFINITIONS: HashMap<Opcode, Definition> = HashMap::from([(
        /*
         * The compiler will keep track of values that can be evaluated at compile time.
         * These are called constants, or statics. The operand of the Opcode isn't the value
         * of the constant, but an index.
         */
        Opcode::Constant,
        Definition {
            name: "OpConstant",
            operand_widths: &[2], // This means that the constant pool is allowed 65536 Objects.
        },
    )]);
}

/*
 * Produces Instructions by collecting bytes from the operands. Number of bytes collected
 * depends on the Opcode.
 * So for example (Opcode::Constant, [257])
 * Opcode::Constant corresponds to 0x0 = 0.
 * 256 = 0x00000101 => select only the 2 last bytes => 0x0101 => [1, 1]
 * Combined we get [0, 1, 1], where [1, 1] should be interpreted as big endian.
 */
pub fn make(op: Opcode, operands: &[u32]) -> Instructions {
    let def = DEFINITIONS.get(&op).expect("Bad Opcode to make.");

    let instruction_len: usize = (1 + def.operand_widths.iter().sum::<u8>()).into();
    let mut instruction = vec![];
    instruction.push(op as u8);

    let mut offset = 1;
    for (o, w) in operands.iter().zip(def.operand_widths) {
        match w {
            2 => {
                let bytes = o.to_be_bytes();
                // Getting the two last bytes of a 4 byte thing.
                let bytes = [bytes[bytes.len() - 2], bytes[bytes.len() - 1]];
                instruction.append(&mut Vec::from(bytes));
            }
            _ => todo!(),
        }
    }

    Instructions::new(instruction)
}

/*
 * Interprets a array of bytes as an given Opcode.
 * Given a definition and bytes, transforms for example
 * (Opcode::Constant, [1, 1]) into (257, 2).
 * The 2 is useful when you need to know how much to advance in an array of multiple instructions.
 */
pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<u16>, usize) {
    let mut operands = vec![];
    let mut offset = 0;

    for ow in def.operand_widths {
        match ow {
            2 => {
                let bytes = [ins[offset], ins[offset + 1]];
                operands.push(u16::from_be_bytes(bytes));
            }
            _ => todo!(),
        }

        offset += *ow as usize;
    }

    (operands, offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        struct Case<'case> {
            op: Opcode,
            operands: &'case [u32],
            expected: &'case [u8],
        };

        let cases = [Case {
            op: Opcode::Constant,
            operands: &[65534],
            expected: &[Opcode::Constant as u8, 255, 254],
        }];

        for case in cases {
            let instruction = make(case.op, case.operands);

            assert_eq!(instruction.len(), case.expected.len());

            for (i, e) in instruction.iter().zip(case.expected) {
                assert_eq!(i, e);
            }
        }
    }

    #[test]
    fn test_instruction_string() {
        use Opcode::*;
        let instructions = [
            &make(Constant, &[1])[..],
            &make(Constant, &[2])[..],
            &make(Constant, &[65535])[..],
        ];

        let expected = "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
";

        let concatted = Instructions::new(instructions.concat());

        assert_eq!(concatted.to_string(), expected);
    }

    #[test]
    /*
     * Test if we can go back and forth from bytes to operands.
     */
    fn test_read_operands() {
        use Opcode::*;
        struct Case<'operand> {
            op: Opcode,
            operands: &'operand [u32],
            bytes_read: usize,
        }
        let cases = [Case {
            op: Constant,
            operands: &[65535],
            bytes_read: 2,
        }];

        for case in cases {
            let instruction = make(case.op, case.operands);
            let definition = DEFINITIONS.get(&case.op).expect("Definition not found");
            let (operands_read, n) = read_operands(definition, &instruction[1..]);

            assert_eq!(case.bytes_read, n);
            for (a, e) in operands_read.iter().zip(case.operands) {
                assert_eq!(*a, *e as u16)
            }
        }
    }
}
