pub struct Bus {
    pub(crate) ram: Box<[u8; 65536]>,
}

impl Bus {
    pub fn write(&mut self, location: u16, value: &[u8]) {
        self.ram[location as usize..location as usize + value.len()].clone_from_slice(value)
    }

    pub fn write_u8(&mut self, location: u16, value: u8) {
        self.write(location, &value.to_le_bytes())
    }

    pub fn write_u16(&mut self, location: u16, value: u16) {
        self.write(location, &value.to_le_bytes())
    }

    pub fn read(&mut self, location: u16, destination: &mut [u8]) {
        destination.clone_from_slice(&self.ram[location as usize..location as usize + destination.len()])
    }

    pub fn read_u8(&mut self, location: u16) -> u8 {
        let mut result = [0u8];
        self.read(location, &mut result);
        u8::from_le_bytes(result)
    }

    pub fn read_u16(&mut self, location: u16) -> u16 {
        let mut result = [0u8, 0];
        self.read(location, &mut result);
        u16::from_le_bytes(result)
    }
}
