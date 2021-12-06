use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::io::{Error, ErrorKind, Read, Seek, SeekFrom};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::bus::Address;

lazy_static! {
    static ref INSTRUCTION_LOOKUP_TABLE: HashMap<u8, (Mnemonic, Addressing, usize)> = {
        use Mnemonic::*;
        use Addressing::*;

        let mut map = HashMap::new();
        let mut put = |opcode, mnemonic, addressing, cycles| {
            assert_eq!(map.insert(opcode, (mnemonic, addressing, cycles)), None);
        };

        // @formatter:off

        put(0x69, ADC, Immediate, 2);
        put(0x65, ADC, ZeroPage,  3);
        put(0x75, ADC, ZeroPageX, 4);
        put(0x6D, ADC, Absolute,  4);
        put(0x7D, ADC, AbsoluteX, 4);
        put(0x79, ADC, AbsoluteY, 4);
        put(0x61, ADC, IndirectX, 6);
        put(0x71, ADC, IndirectY, 5);

        put(0x29, AND, Immediate, 2);
        put(0x25, AND, ZeroPage,  3);
        put(0x35, AND, ZeroPageX, 4);
        put(0x2D, AND, Absolute,  4);
        put(0x3D, AND, AbsoluteX, 4);
        put(0x39, AND, AbsoluteY, 4);
        put(0x21, AND, IndirectX, 6);
        put(0x31, AND, IndirectY, 5);

        put(0x0A, ASL, Implied,   2);
        put(0x06, ASL, ZeroPage,  5);
        put(0x16, ASL, ZeroPageX, 6);
        put(0x0E, ASL, Absolute,  6);
        put(0x1E, ASL, AbsoluteX, 7);

        put(0x90, BCC, Relative,  2);
        put(0xB0, BCS, Relative,  2);
        put(0xF0, BEQ, Relative,  2);

        put(0x24, BIT, ZeroPage,  3);
        put(0x2C, BIT, Absolute,  4);

        put(0x30, BMI, Relative,  2);
        put(0xD0, BNE, Relative,  2);
        put(0x10, BPL, Relative,  2);

        put(0x00, BRK, Implied,   7);

        put(0x50, BVC, Relative,  2);
        put(0x70, BVS, Relative,  2);

        put(0x18, CLC, Implied,   2);
        put(0xD8, CLD, Implied,   2);
        put(0x58, CLI, Implied,   2);
        put(0xB8, CLV, Implied,   2);

        put(0xC9, CMP, Immediate, 2);
        put(0xC5, CMP, ZeroPage,  3);
        put(0xD5, CMP, ZeroPageX, 4);
        put(0xCD, CMP, Absolute,  4);
        put(0xDD, CMP, AbsoluteX, 4);
        put(0xD9, CMP, AbsoluteY, 4);
        put(0xC1, CMP, IndirectX, 6);
        put(0xD1, CMP, IndirectY, 5);

        put(0xE0, CPX, Immediate, 2);
        put(0xE4, CPX, ZeroPage,  3);
        put(0xEC, CPX, Absolute,  4);

        put(0xC0, CPY, Immediate, 2);
        put(0xC4, CPY, ZeroPage,  3);
        put(0xCC, CPY, Absolute,  4);

        put(0xC6, DEC, ZeroPage,  5);
        put(0xD6, DEC, ZeroPageX, 6);
        put(0xCE, DEC, Absolute,  6);
        put(0xDE, DEC, AbsoluteX, 7);
        put(0xCA, DEX, Implied,   2);
        put(0x88, DEY, Implied,   2);

        put(0x49, EOR, Immediate, 2);
        put(0x45, EOR, ZeroPage,  3);
        put(0x55, EOR, ZeroPageX, 4);
        put(0x4D, EOR, Absolute,  4);
        put(0x5D, EOR, AbsoluteX, 4);
        put(0x59, EOR, AbsoluteY, 4);
        put(0x41, EOR, IndirectX, 6);
        put(0x51, EOR, IndirectY, 5);

        put(0xE6, INC, ZeroPage,  5);
        put(0xF6, INC, ZeroPageX, 6);
        put(0xEE, INC, Absolute,  6);
        put(0xFE, INC, AbsoluteX, 7);
        put(0xE8, INX, Implied,   2);
        put(0xC8, INY, Implied,   2);

        put(0x4C, JMP, Absolute,  3);
        put(0x6C, JMP, Indirect,  5);
        put(0x20, JSR, AbsoluteX, 6);

        put(0xA9, LDA, Immediate, 2);
        put(0xA5, LDA, ZeroPage,  3);
        put(0xB5, LDA, ZeroPageX, 4);
        put(0xAD, LDA, Absolute,  4);
        put(0xBD, LDA, AbsoluteX, 4);
        put(0xB9, LDA, AbsoluteY, 4);
        put(0xA1, LDA, IndirectX, 6);
        put(0xB1, LDA, IndirectY, 5);

        put(0xA2, LDX, Immediate, 2);
        put(0xA6, LDX, ZeroPage,  3);
        put(0xB6, LDX, ZeroPageY, 4);
        put(0xAE, LDX, Absolute,  4);
        put(0xBE, LDX, AbsoluteY, 4);

        put(0xA0, LDY, Immediate, 2);
        put(0xA4, LDY, ZeroPage,  3);
        put(0xB4, LDY, ZeroPageX, 4);
        put(0xAC, LDY, Absolute,  4);
        put(0xBC, LDY, AbsoluteX, 4);

        put(0x4A, SLR, Implied,   2);
        put(0x46, SLR, ZeroPage,  5);
        put(0x56, SLR, ZeroPageX, 6);
        put(0x4E, SLR, Absolute,  6);
        put(0x5E, SLR, Absolute,  7);

        put(0xEA, NOP, Implied,   2);

        put(0x09, ORA, Immediate, 2);
        put(0x05, ORA, ZeroPage,  3);
        put(0x15, ORA, ZeroPageX, 4);
        put(0x0D, ORA, Absolute,  4);
        put(0x1D, ORA, AbsoluteX, 4);
        put(0x19, ORA, AbsoluteY, 4);
        put(0x01, ORA, IndirectX, 6);
        put(0x11, ORA, IndirectY, 5);

        put(0x48, PHA, Implied,   3);
        put(0x08, PHP, Implied,   3);
        put(0x68, PLA, Implied,   4);
        put(0x28, PLP, Implied,   4);

        put(0x2A, ROL, Implied,   4);
        put(0x26, ROL, ZeroPage,  5);
        put(0x36, ROL, ZeroPageX, 6);
        put(0x2E, ROL, Absolute,  6);
        put(0x3E, ROL, AbsoluteX, 7);

        put(0x6A, ROR, Implied,   4);
        put(0x66, ROR, ZeroPage,  5);
        put(0x76, ROR, ZeroPageX, 6);
        put(0x6E, ROR, Absolute,  6);
        put(0x7E, ROR, AbsoluteX, 7);

        put(0x40, RTI, Implied,   6);
        put(0x60, RTS, Implied,   6);

        put(0xE9, SBC, Immediate, 2);
        put(0xE5, SBC, ZeroPage,  3);
        put(0xF5, SBC, ZeroPageX, 4);
        put(0xED, SBC, Absolute,  4);
        put(0xFD, SBC, AbsoluteX, 4);
        put(0xF9, SBC, AbsoluteY, 4);
        put(0xE1, SBC, IndirectX, 6);
        put(0xF1, SBC, IndirectY, 5);

        put(0x38, SEC, Implied,   2);
        put(0xF8, SED, Implied,   2);
        put(0x78, SEI, Implied,   2);

        put(0x85, STA, ZeroPage,  3);
        put(0x95, STA, ZeroPageX, 4);
        put(0x8D, STA, Absolute,  4);
        put(0x9D, STA, AbsoluteX, 5);
        put(0x99, STA, AbsoluteY, 5);
        put(0x81, STA, IndirectX, 6);
        put(0x91, STA, IndirectY, 6);

        put(0x86, STX, ZeroPage,  3);
        put(0x96, STX, ZeroPageY, 4);
        put(0x8E, STX, Absolute,  4);

        put(0x84, STY, ZeroPage,  3);
        put(0x94, STY, ZeroPageX, 4);
        put(0x8C, STY, Absolute,  4);

        put(0xAA, TAX, Implied,   2);
        put(0xA8, TAY, Implied,   2);
        put(0xBA, TSX, Implied,   2);
        put(0x8A, TXA, Implied,   2);
        put(0x9A, TXS, Implied,   2);
        put(0x98, TYA, Implied,   2);

        // @formatter:on

        map
    };
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Operand {
    Immediate(u8),
    Memory(Address),
    Branch(Address),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Mnemonic {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    SLR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Addressing {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Implied,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Instruction {
    mnemonic: Mnemonic,
    operand: Option<Operand>,
    addressing: Addressing,
    pos: u16,
    len: u16,
    cycles: usize,
}

impl Instruction {
    pub fn immediate(&self) -> Option<u8> {
        match self.operand {
            Some(Operand::Immediate(x)) => Some(x),
            _ => None
        }
    }

    pub fn memory(&self) -> Option<Address> {
        match self.operand {
            Some(Operand::Memory(x)) => Some(x),
            _ => None
        }
    }

    pub fn branch(&self, start: Address) -> Option<Address> {
        match self.operand {
            Some(Operand::Branch(x)) => Some((start as i16 + x as i16) as Address),
            _ => None
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut pos = self.pos + self.len;
        if let Some(start) = f.width() {
            pos += start as u16;
        }
        Debug::fmt(&self.mnemonic, f)?;
        match self.addressing {
            Addressing::Immediate => f.write_fmt(format_args!(" #${:02X}", self.immediate().unwrap())),
            Addressing::ZeroPage => f.write_fmt(format_args!(" ${:02X}", self.memory().unwrap())),
            Addressing::ZeroPageX => f.write_fmt(format_args!(" ${:02X},X", self.memory().unwrap())),
            Addressing::ZeroPageY => f.write_fmt(format_args!(" ${:02X},Y", self.memory().unwrap())),
            Addressing::Relative => f.write_fmt(format_args!(" ${:04X}", self.branch(pos as u16).unwrap())),
            Addressing::Absolute => f.write_fmt(format_args!(" ${:04X}", self.memory().unwrap())),
            Addressing::AbsoluteX => f.write_fmt(format_args!(" ${:04X},X", self.memory().unwrap())),
            Addressing::AbsoluteY => f.write_fmt(format_args!(" ${:04X},Y", self.memory().unwrap())),
            Addressing::Indirect => f.write_fmt(format_args!(" (${:04X})", self.memory().unwrap())),
            Addressing::IndirectX => f.write_fmt(format_args!(" (${:04X},X)", self.memory().unwrap())),
            Addressing::IndirectY => f.write_fmt(format_args!(" (${:04X}),Y", self.memory().unwrap())),
            Addressing::Implied => Ok(()),
        }
    }
}

pub struct Decoder<T: Read + Seek>(Box<T>);

impl<T: Read + Seek> Iterator for Decoder<T> {
    type Item = Result<Instruction, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos().ok()? >= self.len().ok()? {
            return None;
        }
        let start = self.pos().ok()?;
        let opcode = self.fetch_next_u8().ok()?;
        let result = match INSTRUCTION_LOOKUP_TABLE.get(&opcode).cloned() {
            Some((mnemonic, addressing, cycles)) => {
                Ok(Instruction {
                    mnemonic,
                    operand: self.fetch_operand(addressing).ok()?,
                    addressing,
                    pos: start,
                    len: self.pos().ok()? - start,
                    cycles,
                })
            }
            None => Err(Error::new(ErrorKind::InvalidData, format!("Can't decode instruction at ${:04x}: ${:02x}", start, opcode))),
        };
        Some(result)
    }
}

impl<T: Read + Seek> Decoder<T> {
    pub fn new(inner: T) -> Self {
        Decoder(Box::new(inner))
    }

    fn fetch_operand(&mut self, addressing: Addressing) -> Result<Option<Operand>, Error> {
        match addressing {
            Addressing::Immediate => Ok(Some(Operand::Immediate(self.fetch_next_u8()?))),
            Addressing::ZeroPage |
            Addressing::ZeroPageX |
            Addressing::ZeroPageY => Ok(Some(Operand::Memory(self.fetch_next_u8()? as u16))),
            Addressing::Absolute |
            Addressing::AbsoluteX |
            Addressing::AbsoluteY |
            Addressing::Indirect => Ok(Some(Operand::Memory(self.fetch_next_u16()?))),
            Addressing::IndirectX |
            Addressing::IndirectY => Ok(Some(Operand::Memory(self.fetch_next_u8()? as u16))),
            Addressing::Relative => Ok(Some(Operand::Branch(self.fetch_next_u8()? as i8 as u16))),
            Addressing::Implied => Ok(None),
        }
    }

    fn fetch_next_u8(&mut self) -> Result<u8, Error> {
        self.0.read_u8()
    }

    fn fetch_next_u16(&mut self) -> Result<u16, Error> {
        self.0.read_u16::<LittleEndian>()
    }

    fn pos(&mut self) -> Result<u16, Error> {
        self.0.stream_position().map(|x| x as u16)
    }

    fn len(&mut self) -> Result<u16, Error> {
        let pos = self.0.stream_position()?;
        let len = self.0.seek(SeekFrom::End(0))?;

        if pos != len {
            self.0.seek(SeekFrom::Start(pos))?;
        }

        Ok(len as u16)
    }
}


#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Cursor, Error, Read, Seek, SeekFrom};

    use crate::decoder::{Addressing, Decoder, Instruction, Mnemonic, Operand};

    #[test]
    fn can_dump_rom() -> Result<(), Error> {
        const REAL_DATA_START: usize = 0x400;
        const VIRT_DATA_START: usize = 0x400;

        let mut file = File::open("roms/6502_functional_test.bin")?;
        let mut buf = Vec::new();
        file.seek(SeekFrom::Start(REAL_DATA_START as u64))?;
        file.read_to_end(&mut buf)?;

        let mut last_decode_succeeded = false;

        for instruction in Decoder::new(Cursor::new(&buf)).into_iter() {
            match instruction {
                Ok(instruction) => {
                    print!("${:04X} |", instruction.pos + VIRT_DATA_START as u16);

                    let bytes = &buf[instruction.pos as usize..(instruction.pos + instruction.len) as usize];
                    for byte in bytes.iter() {
                        print!(" {:02X}", byte);
                    }
                    for _ in 0..(3 - bytes.len()) {
                        print!("   ");
                    }
                    println!(" | {:start$}", instruction, start = VIRT_DATA_START);
                    last_decode_succeeded = true;
                }
                Err(_) => {
                    if last_decode_succeeded {
                        println!("----- | -------- | ---------");
                        last_decode_succeeded = false;
                    }
                }
            };
        }

        Ok(())
    }

    #[test]
    fn can_decode_lda() -> Result<(), Error> {
        let mut decoder = Decoder::new(Cursor::new(&[
            0xA9, 0x01,
            0xA5, 0x02,
            0xB5, 0x03,
            0xAD, 0x04, 0x40,
            0xBD, 0x05, 0x50,
            0xB9, 0x06, 0x60,
            0xA1, 0x07,
            0xB1, 0x08,
        ]));

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Immediate(0x01)),
            addressing: Addressing::Immediate,
            pos: 0,
            len: 2,
            cycles: 2,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x02)),
            addressing: Addressing::ZeroPage,
            pos: 2,
            len: 2,
            cycles: 3,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x03)),
            addressing: Addressing::ZeroPageX,
            pos: 4,
            len: 2,
            cycles: 4,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x4004)),
            addressing: Addressing::Absolute,
            pos: 6,
            len: 3,
            cycles: 4,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x5005)),
            addressing: Addressing::AbsoluteX,
            pos: 9,
            len: 3,
            cycles: 4,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x6006)),
            addressing: Addressing::AbsoluteY,
            pos: 12,
            len: 3,
            cycles: 4,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x07)),
            addressing: Addressing::IndirectX,
            pos: 15,
            len: 2,
            cycles: 6,
        });

        assert_eq!(decoder.next().unwrap()?, Instruction {
            mnemonic: Mnemonic::LDA,
            operand: Some(Operand::Memory(0x08)),
            addressing: Addressing::IndirectY,
            pos: 17,
            len: 2,
            cycles: 5,
        });

        Ok(())
    }
}