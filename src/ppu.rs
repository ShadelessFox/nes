use crate::bus::{Address, Device};

pub struct PPU {}

impl PPU {
    pub fn new() -> Self {
        PPU{}
    }
}

impl Device for PPU {
    fn read_u8(&mut self, address: Address) -> Option<u8> {
        if address >= 0x2000 && address <= 0x3fff {
            unreachable!()
        } else {
            None
        }
    }

    fn write_u8(&mut self, address: Address, value: u8) -> Option<()> {
        if address >= 0x2000 && address <= 0x3fff {
            unreachable!()
        } else {
            None
        }
    }
}