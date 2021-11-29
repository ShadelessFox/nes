use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::bus::Bus;

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

    lookup_table: HashMap<u8, Instruction>,
}

#[derive(Copy, Clone)]
struct Instruction {
    name: &'static str,
    addr: &'static dyn Fn(&mut CPU) -> u16,
    inst: &'static dyn Fn(&mut CPU, u16) -> (),
    cycles: u8,
}

impl CPU {
    pub fn new(bus: Rc<RefCell<Bus>>) -> CPU {
        let mut cpu = CPU { bus, acc: 0, idx: 0, idy: 0, ps: 0, sp: 0, pc: 0, lookup_table: Self::build_lookup_table() };
        cpu.set_flag(Flag::Zero, true);
        cpu.set_flag(Flag::Interrupt, true);
        cpu
    }

    pub fn cycle(&mut self) {
        print!("{:04X} | A = ${:02X}, X = ${:02X}, Y = ${:02X}, PC = ${:04X}, P = ${:02X} [{}{}{}{}{}{}] | ", self.pc, self.acc, self.idx, self.idy, self.pc, self.ps, if self.get_flag(Flag::Negative) { "N" } else { "-" }, if self.get_flag(Flag::Overflow) { "V" } else { "-" }, if self.get_flag(Flag::Decimal) { "D" } else { "-" }, if self.get_flag(Flag::Interrupt) { "I" } else { "-" }, if self.get_flag(Flag::Zero) { "Z" } else { "-" }, if self.get_flag(Flag::Carry) { "C" } else { "-" });

        let opcode = self.fetch_u8();

        match self.lookup_table.get(&opcode).cloned() {
            Some(instruction) => {
                println!("{}", instruction.name);
                let addr = (instruction.addr)(self);
                let _cycles = (instruction.inst)(self, addr);
            }
            None => panic!("Unknown opcode ${:#02X} at pc ${:#02X}", opcode, self.pc - 1)
        }
    }

    fn fetch_u8(&mut self) -> u8 {
        let result = self.bus.borrow_mut().read_u8(self.pc);
        self.pc += 1;
        result
    }

    fn fetch_u16(&mut self) -> u16 {
        let result = self.bus.borrow_mut().read_u16(self.pc);
        self.pc += 2;
        result
    }

    fn fetch_imm(&mut self) -> u16 {
        self.fetch_u8() as u16
    }

    fn fetch_imp(&mut self) -> u16 {
        self.acc as u16
    }

    fn fetch_zpg(&mut self) -> u16 {
        self.fetch_u8() as u16
    }

    fn fetch_zpx(&mut self) -> u16 {
        self.fetch_u8().wrapping_add(self.idx) as u16
    }

    fn fetch_zpy(&mut self) -> u16 {
        self.fetch_u8().wrapping_add(self.idy) as u16
    }

    fn fetch_rel(&mut self) -> u16 {
        (self.fetch_u8() as i8 as i16).wrapping_add(self.pc as i16) as u16
    }

    fn fetch_abs(&mut self) -> u16 {
        self.fetch_u16()
    }

    fn fetch_abx(&mut self) -> u16 {
        self.fetch_abs().wrapping_add(self.idx as u16)
    }

    fn fetch_aby(&mut self) -> u16 {
        self.fetch_abs().wrapping_add(self.idy as u16)
    }

    fn fetch_idx(&mut self) -> u16 {
        let ptr = self.fetch_u8();
        self.bus.borrow_mut().read_u16(ptr.wrapping_add(self.idx) as u16)
    }

    fn fetch_idy(&mut self) -> u16 {
        let ptr = self.fetch_u8();
        self.bus.borrow_mut().read_u16(ptr as u16).wrapping_add(self.idy as u16)
    }

    fn push_u8(&mut self, value: u8) {
        self.bus.borrow_mut().write_u8(0x100 - self.sp as u16, value);
        self.sp += 1;
    }

    fn push_u16(&mut self, value: u16) {
        self.bus.borrow_mut().write_u16(0x100 - self.sp as u16, value);
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

    fn ldx(&mut self, value: u16) {
        let value = value as u8;
        self.idx = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
    }

    fn ldy(&mut self, value: u16) {
        let value = value as u8;
        self.idy = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
    }

    fn lda(&mut self, value: u16) {
        let value = value as u8;
        self.acc = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
    }

    fn sta(&mut self, value: u16) {
        self.bus.borrow_mut().write_u8(value, self.acc);
    }

    fn stx(&mut self, value: u16) {
        self.bus.borrow_mut().write_u8(value, self.idx);
    }

    fn sty(&mut self, value: u16) {
        self.bus.borrow_mut().write_u8(value, self.idy);
    }

    fn dex(&mut self, _: u16) {
        let value = self.idx.overflowing_sub(1).0;
        self.idx = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
    }

    fn dey(&mut self, _: u16) {
        let value = self.idy.overflowing_sub(1).0;
        self.idy = value;
        self.set_flag(Flag::Zero, value == 0);
        self.set_flag(Flag::Negative, value >= 128);
    }

    fn bne(&mut self, value: u16) {
        if !self.get_flag(Flag::Zero) {
            self.pc = value;
        }
    }

    fn bpl(&mut self, value: u16) {
        if !self.get_flag(Flag::Negative) {
            self.pc = value;
        }
    }

    fn jsr(&mut self, value: u16) {
        self.push_u16(self.pc - 1);
        self.pc = value;
    }

    fn pha(&mut self, _: u16) {
        self.push_u8(self.acc);
    }

    fn bit(&mut self, value: u16) {
        let value = value as u8;
        self.set_flag(Flag::Negative, value >> 6 & 1 == 1);
        self.set_flag(Flag::Overflow, value >> 7 & 1 == 1);
        self.set_flag(Flag::Zero, value & self.acc == 0);
    }

    fn sei(&mut self, _: u16) {
        self.set_flag(Flag::Interrupt, true);
    }

    fn sec(&mut self, _: u16) {
        self.set_flag(Flag::Carry, true);
    }

    fn cld(&mut self, _: u16) {
        self.set_flag(Flag::Decimal, false);
    }

    fn txs(&mut self, _: u16) {
        self.sp = self.idx
    }

    fn nop(&mut self, _: u16) {}

    fn build_lookup_table() -> HashMap<u8, Instruction> {
        let make = |name, addr, inst, cycles| Instruction { name, addr, inst, cycles };
        let mut map = HashMap::new();

        map.insert(0xA9, make("LDA", &CPU::fetch_imm, &CPU::lda, 2));
        map.insert(0xA5, make("LDA", &CPU::fetch_zpg, &CPU::lda, 3));
        map.insert(0xB5, make("LDA", &CPU::fetch_zpx, &CPU::lda, 4));
        map.insert(0xAD, make("LDA", &CPU::fetch_abs, &CPU::lda, 4));
        map.insert(0xBD, make("LDA", &CPU::fetch_abx, &CPU::lda, 4)); // *
        map.insert(0xB9, make("LDA", &CPU::fetch_aby, &CPU::lda, 4)); // *
        map.insert(0xA1, make("LDA", &CPU::fetch_idx, &CPU::lda, 6));
        map.insert(0xB1, make("LDA", &CPU::fetch_idy, &CPU::lda, 5)); // *

        map.insert(0xA2, make("LDX", &CPU::fetch_imm, &CPU::ldx, 2));
        map.insert(0xA6, make("LDX", &CPU::fetch_zpg, &CPU::ldx, 3));
        map.insert(0xB6, make("LDX", &CPU::fetch_zpy, &CPU::ldx, 4));
        map.insert(0xAE, make("LDX", &CPU::fetch_abs, &CPU::ldx, 4));
        map.insert(0xBE, make("LDX", &CPU::fetch_aby, &CPU::ldx, 4)); // *

        map.insert(0xA0, make("LDY", &CPU::fetch_imm, &CPU::ldy, 2));
        map.insert(0xA4, make("LDY", &CPU::fetch_zpg, &CPU::ldy, 3));
        map.insert(0xB4, make("LDY", &CPU::fetch_zpx, &CPU::ldy, 4));
        map.insert(0xAC, make("LDY", &CPU::fetch_abs, &CPU::ldy, 4));
        map.insert(0xBC, make("LDY", &CPU::fetch_abx, &CPU::ldy, 4)); // *

        map.insert(0x85, make("STA", &CPU::fetch_zpg, &CPU::sta, 3));
        map.insert(0x95, make("STA", &CPU::fetch_zpx, &CPU::sta, 4));
        map.insert(0x8D, make("STA", &CPU::fetch_abs, &CPU::sta, 4));
        map.insert(0x9D, make("STA", &CPU::fetch_abx, &CPU::sta, 5));
        map.insert(0x99, make("STA", &CPU::fetch_aby, &CPU::sta, 5));
        map.insert(0x81, make("STA", &CPU::fetch_idx, &CPU::sta, 6));
        map.insert(0x91, make("STA", &CPU::fetch_idy, &CPU::sta, 6));

        map.insert(0x86, make("STX", &CPU::fetch_zpg, &CPU::stx, 3));
        map.insert(0x96, make("STX", &CPU::fetch_zpy, &CPU::stx, 4));
        map.insert(0x8E, make("STX", &CPU::fetch_abs, &CPU::stx, 4));

        map.insert(0x84, make("STY", &CPU::fetch_zpg, &CPU::sty, 3));
        map.insert(0x94, make("STY", &CPU::fetch_zpx, &CPU::sty, 4));
        map.insert(0x8C, make("STY", &CPU::fetch_abs, &CPU::sty, 4));

        map.insert(0xCA, make("DEX", &CPU::fetch_imp, &CPU::dex, 2));
        map.insert(0x88, make("DEY", &CPU::fetch_imp, &CPU::dey, 2));

        map.insert(0xD0, make("BNE", &CPU::fetch_rel, &CPU::bne, 2)); // **
        map.insert(0x10, make("BPL", &CPU::fetch_rel, &CPU::bpl, 2)); // **

        map.insert(0x24, make("BIT", &CPU::fetch_zpg, &CPU::bit, 3));
        map.insert(0x2C, make("BIT", &CPU::fetch_abs, &CPU::bit, 4));

        map.insert(0x20, make("JSR", &CPU::fetch_abs, &CPU::jsr, 6));
        map.insert(0x48, make("PHA", &CPU::fetch_imp, &CPU::pha, 3));

        map.insert(0x78, make("SEI", &CPU::fetch_imp, &CPU::sei, 2));
        map.insert(0x38, make("SEC", &CPU::fetch_imp, &CPU::sec, 2));
        map.insert(0xD8, make("CLD", &CPU::fetch_imp, &CPU::cld, 2));
        map.insert(0x9A, make("TXS", &CPU::fetch_imp, &CPU::txs, 2));
        map.insert(0x04, make("NOP", &CPU::fetch_zpg, &CPU::nop, 2));

        map
    }
}
