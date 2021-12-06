use std::cell::RefCell;
use std::fs::File;
use std::io::Error;
use std::rc::Rc;

use cartridge::Cartridge;
use cpu::CPU;
use ppu::PPU;
use ram::RAM;

use crate::bus::Bus;

pub mod bus;
pub mod cpu;
pub mod ppu;
pub mod ram;
pub mod cartridge;
pub mod decoder;

const CARTRIDGE_PATH: &str = "roms/nestest.nes";

fn main() -> Result<(), Error> {
    let bus = Rc::new(RefCell::new(Bus::new()));
    bus.borrow_mut().mount(Box::new(RAM::new()), 0x0000..=0x1fff).unwrap();
    bus.borrow_mut().mount(Box::new(PPU::new()), 0x2000..=0x2007).unwrap();
    bus.borrow_mut().mount(Box::new(Cartridge::from_file(&mut File::open(CARTRIDGE_PATH)?)?), 0x4020..=0xffff).unwrap();

    let mut cpu = CPU::new(bus.clone());
    cpu.reset();

    loop {
        cpu.cycle();
    }
}
