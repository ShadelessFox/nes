use std::fs::File;
use std::io::{Error, ErrorKind, Read, Seek, SeekFrom};

use byteorder::{BigEndian, ReadBytesExt};

use crate::bus::{Address, Device};

pub struct Cartridge {
    pub(crate) prg_rom: Vec<u8>,
    pub(crate) chr_rom: Vec<u8>,
}

impl Cartridge {
    pub fn from_file(file: &mut File) -> Result<Self, Error> {
        let magic = file.read_u32::<BigEndian>()?;

        if magic != 0x4e45531a {
            return Err(Error::new(ErrorKind::InvalidData, "Invalid iNES header"));
        }

        let prg_rom_pages = file.read_u8()?;
        let chr_rom_pages = file.read_u8()?;
        let flags = file.read_u8()?;

        file.seek(SeekFrom::Start(16))?;

        if flags & 0x04 > 0 {
            file.seek(SeekFrom::Current(512))?;
        }

        let mut prg_rom_buf = vec![0; prg_rom_pages as usize * 16384];
        let mut chr_rom_buf = vec![0; chr_rom_pages as usize * 8192];

        file.read_exact(&mut prg_rom_buf)?;
        file.read_exact(&mut chr_rom_buf)?;

        Ok(Cartridge {
            prg_rom: prg_rom_buf,
            chr_rom: chr_rom_buf,
        })
    }
}

impl Device for Cartridge {
    fn read_u8(&mut self, address: Address) -> Option<u8> {
        if address >= 0x8000 {
            Some(self.prg_rom[address as usize % self.prg_rom.len()])
        } else {
            None
        }
    }

    fn write_u8(&mut self, address: Address, value: u8) -> Option<()> {
        if address >= 0x8000 {
            let address = address as usize % self.prg_rom.len();
            self.prg_rom[address] = value;
            Some(())
        } else {
            None
        }
    }
}