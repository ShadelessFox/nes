use std::cell::RefCell;
use std::fs::File;
use std::io::{Error, Read, Seek, SeekFrom};
use std::rc::Rc;

use bus::Bus;

use crate::cpu::CPU;

mod bus;
mod cpu;
mod ppu;

fn main() -> Result<(), Error> {
    let bus = Rc::new(RefCell::new(Bus { ram: Box::new([0; 0x10000]) }));

    let rom = load_rom_from_nes(&mut File::open("roms/nestest.nes")?)?;
    bus.borrow_mut().write((0x10000 - rom.len()) as u16, &rom);

    let mut reset: [u8; 2] = [0, 0];
    bus.borrow_mut().read(0xfffc as u16, &mut reset);

    let mut cpu = CPU::new(bus.clone());
    cpu.pc = u16::from_le_bytes(reset);

    loop {
        cpu.cycle();
    }
}

fn load_rom_from_nes(file: &mut File) -> Result<Vec<u8>, Error> {
    file.seek(SeekFrom::Start(4))?;
    let mut rom_pages = [0u8];
    file.read(&mut rom_pages)?;
    file.seek(SeekFrom::Start(16))?;
    let mut buf = vec![0; rom_pages[0] as usize * 16384];
    file.read_exact(&mut buf)?;
    Ok(buf)
}