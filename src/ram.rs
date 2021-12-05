use crate::bus::{Address, Device};

pub struct RAM(Box<[u8; 0xfff]>);

impl RAM {
    pub fn new() -> Self {
        RAM(Box::new([0; 0xfff]))
    }
}

impl Device for RAM {
    fn read_u8(&mut self, address: Address) -> Option<u8> {
        if address <= 0x1fff {
            Some(self.0[(address & 0x7ff) as usize])
        } else {
            None
        }
    }

    fn write_u8(&mut self, address: Address, value: u8) -> Option<()> {
        if address <= 0x1fff {
            self.0[(address & 0x7ff) as usize] = value;
            Some(())
        } else {
            None
        }
    }
}
