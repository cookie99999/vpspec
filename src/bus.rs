const ROM_START: u16 = 0x0000;
const ROM_END: u16 = 0x3fff;
const RAM_START: u16 = 0x4000;
const RAM_END: u16 = 0xffff;

pub trait Bus {
    fn read_byte(&mut self, addr: u16) -> u8;
    fn read_word(&mut self, addr: u16) -> u16;
    fn write_byte(&mut self, addr: u16, data: u8);
    fn write_word(&mut self, addr: u16, data: u16);
    fn read_io_byte(&mut self, port: u16) -> u8;
    fn write_io_byte(&mut self, port: u16, data: u8);
    fn load_bin(&mut self, offs: usize, buf: &[u8]);
    fn step(&mut self, cyc: usize);
}

pub struct ZXBus {
    rom: [u8; 0x4000],
    ram: [u8; 0xC000],
    cycles: usize,
    pub irq: bool,
    pub irq_vec: u8,
    keys: [bool; 60],
}

impl Bus for ZXBus {
    fn read_byte(&mut self, addr: u16) -> u8 {
	match addr {
	    ROM_START ..= ROM_END =>
		self.rom[addr as usize],
	    RAM_START ..= RAM_END =>
		self.ram[(addr - RAM_START) as usize],
	}
    }

    fn read_word(&mut self, addr: u16) -> u16 {
	let result: u16 = self.read_byte(addr) as u16 | ((self.read_byte(addr + 1) as u16) << 8);
	result
    }

    fn write_byte(&mut self, addr: u16, data: u8) {
	match addr {
	    ROM_START ..= ROM_END =>
		println!("attempted write to rom at {addr:04X}"),
	    RAM_START ..= RAM_END =>
		self.ram[(addr - RAM_START) as usize] = data,
	};
    }

    fn write_word(&mut self, addr: u16, data: u16) {
	self.write_byte(addr + 1, (data >> 8) as u8);
	self.write_byte(addr, (data & 0x00ff) as u8);
    }

    fn read_io_byte(&mut self, port: u16) -> u8 {
	match port {
	    0xbffe => {
		0 | (self.keys[0] as u8)
	    },
	    _ => {
		println!("unhandled io port read {port:04x}");
		0
	    },
	}
    }

    fn write_io_byte(&mut self, port: u16, data: u8) {
	match port {
	    _ =>
		println!("unhandled io port write {port:04x}"),
	};
    }

    fn load_bin(&mut self, offs: usize, buf: &[u8]) {
	for i in 0..buf.len() {
	    self.rom[offs + i] = buf[i];
	}
    }

    fn step(&mut self, cyc: usize) {

    }
}

impl ZXBus {
    pub fn new() -> Self {
	ZXBus {
	    rom: [0; 0x4000],
	    ram: [0; 0xc000],
	    cycles: 0,
	    irq: false,
	    irq_vec: 0,
	    keys: [false; 60],
	}
    }

    pub fn vram(&self) -> &[u8] {
	&self.ram[0x4000 - RAM_START as usize ..= 0x5aff - RAM_START as usize]
    }

    pub fn keydown(&mut self, key: usize) {
	self.keys[key] = true;
    }

    pub fn keyup(&mut self, key: usize) {
	self.keys[key] = false;
    }
}

pub struct CpmBus {
    ram: [u8; 0x10000],
    ports: [u8; 0x10000],
    pub irq: bool,
    pub irq_vec: u8,
}

impl Bus for CpmBus {
    fn read_byte(&mut self, addr: u16) -> u8 {
	self.ram[addr as usize]
    }

    fn read_word(&mut self, addr: u16) -> u16 {
	let result: u16 = self.read_byte(addr) as u16 | ((self.read_byte(addr.wrapping_add(1)) as u16) << 8);
	result
    }

    fn write_byte(&mut self, addr: u16, data: u8) {
	self.ram[addr as usize] = data;
    }

    fn write_word(&mut self, addr: u16, data: u16) {
	self.write_byte(addr.wrapping_add(1), (data >> 8) as u8);
	self.write_byte(addr, (data & 0x00ff) as u8);
    }

    fn read_io_byte(&mut self, port: u16) -> u8 {
	match port {
	    _ => self.ports[port as usize],
		//todo!("unhandled io port read {port:04x}"),
	}
    }

    fn write_io_byte(&mut self, port: u16, data: u8) {
	match port {
	    0x00aa => {
		print!("{}", data as char);
	    },
	    0x00ff => panic!("warm booted"),
	    _ => {
		//todo!("unhandled io port write {port:04x}"),
		self.ports[port as usize] = data;
	    },
	};
    }

    fn load_bin(&mut self, offs: usize, buf: &[u8]) {
	for i in 0..buf.len() {
	    self.ram[offs + i] = buf[i];
	}
    }

    fn step(&mut self, cyc: usize) {
	
    }
}

impl CpmBus {
    pub fn new() -> Self {
	CpmBus {
	    ram: [0; 0x10000],
	    ports: [0; 0x10000],
	    irq: false,
	    irq_vec: 0,
	}
    }
}
