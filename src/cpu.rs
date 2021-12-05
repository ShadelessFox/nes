use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::DerefMut;
use std::rc::Rc;

use crate::bus::{Address, Bus, Device};

pub enum Flag {
    Carry,
    Zero,
    Interrupt,
    Decimal,
    Overflow,
    Negative,
}

impl Flag {
    fn get_position(&self) -> u8 {
        match self {
            Flag::Carry => 0,
            Flag::Zero => 1,
            Flag::Interrupt => 2,
            Flag::Decimal => 3,
            Flag::Overflow => 6,
            Flag::Negative => 7
        }
    }
}

#[derive(Copy, Clone)]
enum AddressMode {
    IMM,
    IMP,
    ZPG,
    ZPX,
    ZPY,
    REL,
    ABS,
    ABX,
    ABY,
    IND,
    INX,
    INY,
}

impl AddressMode {
    pub fn fetch(&self, cpu: &mut CPU) -> Address {
        match self {
            AddressMode::IMP => 0,
            AddressMode::IMM => {
                cpu.pc += 1;
                cpu.pc - 1
            }
            AddressMode::ZPG => cpu.read_next_u8() as u16,
            AddressMode::ZPX => cpu.read_next_u8().wrapping_add(cpu.idx) as u16,
            AddressMode::ZPY => cpu.read_next_u8().wrapping_add(cpu.idy) as u16,

            AddressMode::REL => {
                let mut address = cpu.read_next_u8() as u16;
                if address & 0x80 > 0 {
                    address |= 0xff00;
                }
                cpu.pc.wrapping_add(address)
            },

            AddressMode::ABS => cpu.read_next_u16(),
            AddressMode::ABX => cpu.read_next_u16().wrapping_add(cpu.idx as u16),
            AddressMode::ABY => cpu.read_next_u16().wrapping_add(cpu.idy as u16),

            AddressMode::IND => {
                let address = cpu.read_next_u16();
                cpu.bus.borrow_mut().read_u16(address).unwrap()
            }
            AddressMode::INX => {
                let address = cpu.read_next_u8();
                cpu.bus.borrow_mut().read_u16(address as u16 + cpu.idx as u16).unwrap()
            }
            AddressMode::INY => {
                let address = cpu.read_next_u8();
                cpu.bus.borrow_mut().read_u16(address as u16).unwrap() + cpu.idy as u16
            }
        }
    }
}

impl AddressMode {
    fn format(&self, address: Address, bus: &mut Bus, buf: &mut String) {
        match self {
            AddressMode::IMM => buf.push_str(&format!("#${:02X}", bus.read_u8(address).unwrap())),
            AddressMode::ZPG => buf.push_str(&format!("#${:02X} {{zpg}}", address)),
            AddressMode::ZPX => buf.push_str(&format!("#${:02X},X {{zpx}}", address)),
            AddressMode::ZPY => buf.push_str(&format!("#${:02X},Y {{zpy}}", address)),
            AddressMode::REL => buf.push_str(&format!("${:04X} = ${:02X}", address, bus.read_u8(address).unwrap())),
            AddressMode::ABS => buf.push_str(&format!("${:04X} = ${:02X}", address, bus.read_u8(address).unwrap())),
            AddressMode::ABX => buf.push_str(&format!("${:04X},X = ${:02X}", address, bus.read_u8(address).unwrap())),
            AddressMode::ABY => buf.push_str(&format!("${:04X},Y = ${:02X}", address, bus.read_u8(address).unwrap())),
            AddressMode::INX => buf.push_str(&format!("(${:04X}),X", address)),
            AddressMode::INY => buf.push_str(&format!("(${:04X}),Y", address)),
            _ => {}
        }
    }
}

#[derive(Copy, Clone)]
struct Instruction {
    name: &'static str,
    mode: AddressMode,
    inst: &'static dyn Fn(&mut CPU, u16) -> usize,
    cycles: usize,
}

impl Instruction {
    fn format(&self, address: Address, bus: &mut Bus) -> String {
        let mut buf = String::new();

        buf.push_str(self.name);
        buf.push(' ');

        self.mode.format(address, bus, &mut buf);

        buf
    }
}


pub struct CPU {
    pub(crate) bus: Rc<RefCell<Bus>>,
    /// Accumulator register
    pub(crate) acc: u8,
    /// X-Index register
    pub(crate) idx: u8,
    /// Y-Index register
    pub(crate) idy: u8,
    /// Status register
    pub(crate) ps: u8,
    /// Stack pointer register
    pub(crate) sp: u8,
    /// Program counter register
    pub(crate) pc: u16,

    instructions: HashMap<u8, Instruction>,
    cycles: usize,
}

impl CPU {
    pub fn new(bus: Rc<RefCell<Bus>>) -> CPU {
        let mut cpu = CPU {
            bus,
            acc: 0,
            idx: 0,
            idy: 0,
            ps: 0,
            sp: 0,
            pc: 0,
            instructions: Self::build_instruction_lookup_table(),
            cycles: 0,
        };
        cpu.set_flag(Flag::Zero, true);
        cpu.set_flag(Flag::Interrupt, true);
        cpu
    }

    pub fn cycle(&mut self) {
        print!("{:04X} | A = ${:02X}, X = ${:02X}, Y = ${:02X}, PC = ${:04X}, P = ${:02X} [{}{}{}{}{}{}], C: {:10} | ", self.pc, self.acc, self.idx, self.idy, self.pc, self.ps, if self.get_flag(Flag::Negative) { "N" } else { "-" }, if self.get_flag(Flag::Overflow) { "V" } else { "-" }, if self.get_flag(Flag::Decimal) { "D" } else { "-" }, if self.get_flag(Flag::Interrupt) { "I" } else { "-" }, if self.get_flag(Flag::Zero) { "Z" } else { "-" }, if self.get_flag(Flag::Carry) { "C" } else { "-" }, self.cycles);

        let opcode = self.read_next_u8();

        match self.instructions.get(&opcode).cloned() {
            Some(instruction) => {
                let address = instruction.mode.fetch(self);
                println!("{}", instruction.name);
                self.cycles += (instruction.inst)(self, address);
                self.cycles += instruction.cycles as usize;
            }
            None => panic!("Unknown opcode ${:#02X} at pc ${:#02X}", opcode, self.pc - 1)
        }
    }

    pub fn reset(&mut self) {
        self.pc = self.read_u16(0xfffc);
    }

    fn read_next_u8(&mut self) -> u8 {
        let result = self.read_u8(self.pc);
        self.pc += 1;
        result
    }

    fn read_next_u16(&mut self) -> u16 {
        let result = self.read_u16(self.pc);
        self.pc += 2;
        result
    }

    fn read_u8(&mut self, address: Address) -> u8 {
        self.bus.borrow_mut().read_u8(address).unwrap()
    }

    fn read_u16(&mut self, address: Address) -> u16 {
        self.bus.borrow_mut().read_u16(address).unwrap()
    }

    fn write_u8(&mut self, address: Address, value: u8) {
        self.bus.borrow_mut().write_u8(address, value).unwrap()
    }

    fn write_u16(&mut self, address: Address, value: u16) {
        self.bus.borrow_mut().write_u16(address, value).unwrap()
    }

    fn push_u8(&mut self, value: u8) {
        self.write_u8(0x100 - self.sp as u16, value);
        self.sp += 1;
    }

    fn push_u16(&mut self, value: u16) {
        self.write_u16(0x100 - self.sp as u16, value);
        self.sp += 1;
    }

    fn get_flag(&self, flag: Flag) -> bool {
        self.ps >> flag.get_position() & 1 == 1
    }

    fn set_flag(&mut self, flag: Flag, set: bool) {
        if set {
            self.ps |= 1 << flag.get_position();
        } else {
            self.ps &= !(1 << flag.get_position());
        }
    }

    fn ldx(&mut self, address: u16) -> usize {
        let value = self.read_u8(address);
        self.idx = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
        0 // TODO: Take in account page boundary crossing
    }

    fn ldy(&mut self, address: u16) -> usize {
        let value = self.read_u8(address);
        self.idy = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
        0 // TODO: Take in account page boundary crossing
    }

    fn lda(&mut self, address: u16) -> usize {
        let value = self.read_u8(address);
        self.acc = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
        0 // TODO: Take in account page boundary crossing
    }

    fn sta(&mut self, address: u16) -> usize {
        self.write_u8(address, self.acc);
        0
    }

    fn stx(&mut self, address: u16) -> usize {
        self.write_u8(address, self.idx);
        0
    }

    fn sty(&mut self, address: u16) -> usize {
        self.write_u8(address, self.idy);
        0
    }

    fn dex(&mut self, _: u16) -> usize {
        let value = self.idx.overflowing_sub(1).0;
        self.idx = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
        0
    }

    fn dey(&mut self, _: u16) -> usize {
        let value = self.idy.overflowing_sub(1).0;
        self.idy = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
        0
    }

    fn bne(&mut self, address: u16) -> usize {
        if !self.get_flag(Flag::Zero) {
            self.pc = address;
        }
        0 // TODO: Take in account page boundary crossing
    }

    fn bpl(&mut self, address: u16) -> usize {
        if !self.get_flag(Flag::Negative) {
            self.pc = address;
        }
        0 // TODO: Take in account page boundary crossing
    }

    fn jsr(&mut self, address: u16) -> usize {
        self.push_u16(self.pc - 1);
        self.pc = address;
        0
    }

    fn pha(&mut self, _: u16) -> usize {
        self.push_u8(self.acc);
        0
    }

    fn bit(&mut self, address: u16) -> usize {
        let value = self.read_u8(address);
        self.set_flag(Flag::Negative, value >> 6 & 1 == 1);
        self.set_flag(Flag::Overflow, value >> 7 & 1 == 1);
        self.set_flag(Flag::Zero, value & self.acc == 0);
        0
    }

    fn sei(&mut self, _: u16) -> usize {
        self.set_flag(Flag::Interrupt, true);
        0
    }

    fn sec(&mut self, _: u16) -> usize {
        self.set_flag(Flag::Carry, true);
        0
    }

    fn cld(&mut self, _: u16) -> usize {
        self.set_flag(Flag::Decimal, false);
        0
    }

    fn txs(&mut self, _: u16) -> usize {
        self.sp = self.idx;
        0
    }

    fn inx(&mut self, _: u16) -> usize {
        self.idx = self.idx.wrapping_add(1);
        0
    }

    fn iny(&mut self, _: u16) -> usize {
        self.idy = self.idy.wrapping_add(1);
        0
    }

    fn cmp(&mut self, address: u16) -> usize {
        let m = self.read_u8(address);
        let a = self.acc;
        let t = a.wrapping_sub(m);
        self.set_flag(Flag::Negative, t >= 128);
        self.set_flag(Flag::Zero, t == 0);
        self.set_flag(Flag::Carry, a >= m);
        0 // TODO: Take in account page boundary crossing
    }

    fn cpx(&mut self, address: u16) -> usize {
        let m = self.read_u8(address);
        let x = self.idx;
        let t = x.wrapping_sub(m);
        self.set_flag(Flag::Negative, t >= 128);
        self.set_flag(Flag::Zero, t == 0);
        self.set_flag(Flag::Carry, x >= m);
        0
    }

    fn cpy(&mut self, address: u16) -> usize {
        let m = self.read_u8(address);
        let y = self.idy;
        let t = y.wrapping_sub(m);
        self.set_flag(Flag::Negative, t >= 128);
        self.set_flag(Flag::Zero, t == 0);
        self.set_flag(Flag::Carry, y >= m);
        0
    }

    fn build_instruction_lookup_table() -> HashMap<u8, Instruction> {
        use AddressMode::*;

        let make = |name, mode, inst, cycles| Instruction { name, mode, inst, cycles };
        let mut map = HashMap::new();

        map.insert(0xA9, make("LDA", IMM, &CPU::lda, 2));
        map.insert(0xA5, make("LDA", ZPG, &CPU::lda, 3));
        map.insert(0xB5, make("LDA", ZPX, &CPU::lda, 4));
        map.insert(0xAD, make("LDA", ABS, &CPU::lda, 4));
        map.insert(0xBD, make("LDA", ABX, &CPU::lda, 4)); // *
        map.insert(0xB9, make("LDA", ABY, &CPU::lda, 4)); // *
        map.insert(0xA1, make("LDA", INX, &CPU::lda, 6));
        map.insert(0xB1, make("LDA", INY, &CPU::lda, 5)); // *

        map.insert(0xA2, make("LDX", IMM, &CPU::ldx, 2));
        map.insert(0xA6, make("LDX", ZPG, &CPU::ldx, 3));
        map.insert(0xB6, make("LDX", ZPY, &CPU::ldx, 4));
        map.insert(0xAE, make("LDX", ABS, &CPU::ldx, 4));
        map.insert(0xBE, make("LDX", ABY, &CPU::ldx, 4)); // *

        map.insert(0xA0, make("LDY", IMM, &CPU::ldy, 2));
        map.insert(0xA4, make("LDY", ZPG, &CPU::ldy, 3));
        map.insert(0xB4, make("LDY", ZPX, &CPU::ldy, 4));
        map.insert(0xAC, make("LDY", ABS, &CPU::ldy, 4));
        map.insert(0xBC, make("LDY", ABX, &CPU::ldy, 4)); // *

        map.insert(0x85, make("STA", ZPG, &CPU::sta, 3));
        map.insert(0x95, make("STA", ZPX, &CPU::sta, 4));
        map.insert(0x8D, make("STA", ABS, &CPU::sta, 4));
        map.insert(0x9D, make("STA", ABX, &CPU::sta, 5));
        map.insert(0x99, make("STA", ABY, &CPU::sta, 5));
        map.insert(0x81, make("STA", INX, &CPU::sta, 6));
        map.insert(0x91, make("STA", INY, &CPU::sta, 6));

        map.insert(0x86, make("STX", ZPG, &CPU::stx, 3));
        map.insert(0x96, make("STX", ZPY, &CPU::stx, 4));
        map.insert(0x8E, make("STX", ABS, &CPU::stx, 4));

        map.insert(0x84, make("STY", ZPG, &CPU::sty, 3));
        map.insert(0x94, make("STY", ZPX, &CPU::sty, 4));
        map.insert(0x8C, make("STY", ABS, &CPU::sty, 4));

        map.insert(0xCA, make("DEX", IMP, &CPU::dex, 2));
        map.insert(0x88, make("DEY", IMP, &CPU::dey, 2));

        map.insert(0xD0, make("BNE", REL, &CPU::bne, 2)); // **
        map.insert(0x10, make("BPL", REL, &CPU::bpl, 2)); // **

        map.insert(0x24, make("BIT", ZPG, &CPU::bit, 3));
        map.insert(0x2C, make("BIT", ABS, &CPU::bit, 4));

        map.insert(0x20, make("JSR", ABS, &CPU::jsr, 6));
        map.insert(0x48, make("PHA", IMP, &CPU::pha, 3));

        map.insert(0x78, make("SEI", IMP, &CPU::sei, 2));
        map.insert(0x38, make("SEC", IMP, &CPU::sec, 2));
        map.insert(0xD8, make("CLD", IMP, &CPU::cld, 2));
        map.insert(0x9A, make("TXS", IMP, &CPU::txs, 2));
        map.insert(0xE8, make("INX", IMP, &CPU::inx, 2));
        map.insert(0xC8, make("INY", IMP, &CPU::iny, 2));

        map.insert(0xC9, make("CMP", IMM, &CPU::cmp, 2));
        map.insert(0xC5, make("CMP", ZPG, &CPU::cmp, 3));
        map.insert(0xD5, make("CMP", ZPX, &CPU::cmp, 4));
        map.insert(0xCD, make("CMP", ABS, &CPU::cmp, 4));
        map.insert(0xDD, make("CMP", ABX, &CPU::cmp, 4)); // *
        map.insert(0xD9, make("CMP", ABY, &CPU::cmp, 4)); // *
        map.insert(0xC1, make("CMP", INX, &CPU::cmp, 6));
        map.insert(0xD1, make("CMP", INY, &CPU::cmp, 5)); // *

        map.insert(0xE0, make("CPX", IMM, &CPU::cpx, 2));
        map.insert(0xE4, make("CPX", ZPG, &CPU::cpx, 3));
        map.insert(0xEC, make("CPX", ABS, &CPU::cpx, 4));

        map.insert(0xC0, make("CPY", IMM, &CPU::cpy, 2));
        map.insert(0xC4, make("CPY", ZPG, &CPU::cpy, 3));
        map.insert(0xCC, make("CPY", ABS, &CPU::cpy, 4));

        map
    }
}
