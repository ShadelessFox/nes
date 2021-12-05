use std::ops::RangeInclusive;

pub type Address = u16;
pub type AddressRange = RangeInclusive<Address>;

pub trait Device {
    fn read_u8(&mut self, address: Address) -> Option<u8>;

    fn write_u8(&mut self, address: Address, value: u8) -> Option<()>;

    fn read_u16(&mut self, address: Address) -> Option<u16> {
        let lo = self.read_u8(address + 0)? as u16;
        let hi = self.read_u8(address + 1)? as u16;
        Some(hi << 8 | lo)
    }

    fn write_u16(&mut self, address: Address, value: u16) -> Option<()> {
        self.write_u8(address + 0, (value >> 0 & 0xff) as u8)?;
        self.write_u8(address + 1, (value >> 8 & 0xff) as u8)
    }

    fn read(&mut self, address: Address, buf: &mut [u8]) -> Option<()> {
        for i in 0..buf.len() {
            buf[i] = self.read_u8(address + i as u16)?;
        }
        Some(())
    }

    fn write(&mut self, address: Address, buf: &[u8]) -> Option<()> {
        for i in 0..buf.len() {
            self.write_u8(address + i as u16, buf[i])?;
        }
        Some(())
    }
}


pub struct Bus {
    devices: Vec<(AddressRange, Box<dyn Device>)>,
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            devices: Vec::new()
        }
    }

    pub fn mount(&mut self, device: Box<dyn Device>, address: AddressRange) -> Result<(), String> {
        if address.is_empty() {
            return Err("Can't mount a device at empty address range".into());
        }

        for (range, _) in self.devices.iter() {
            if range.contains(address.start()) || range.contains(address.end()) {
                return Err(format!("Can't mount device at {:?} because it clashes with another device mounted at {:?}", address, range));
            }
        }

        self.devices.push((address, device));

        Ok(())
    }

    fn get_device_mut(&mut self, address: Address) -> Option<&mut dyn Device> {
        for (range, device) in self.devices.iter_mut() {
            if range.contains(&address) {
                return Some(device.as_mut());
            }
        }
        None
    }
}

impl Device for Bus {
    fn read_u8(&mut self, address: Address) -> Option<u8> {
        self.get_device_mut(address)?.read_u8(address)
    }

    fn write_u8(&mut self, address: Address, value: u8) -> Option<()> {
        self.get_device_mut(address)?.write_u8(address, value)
    }

    fn read_u16(&mut self, address: Address) -> Option<u16> {
        self.get_device_mut(address)?.read_u16(address)
    }

    fn write_u16(&mut self, address: Address, value: u16) -> Option<()> {
        self.get_device_mut(address)?.write_u16(address, value)
    }

    fn read(&mut self, address: Address, buf: &mut [u8]) -> Option<()> {
        self.get_device_mut(address)?.read(address, buf)
    }

    fn write(&mut self, address: Address, buf: &[u8]) -> Option<()> {
        self.get_device_mut(address)?.write(address, buf)
    }
}