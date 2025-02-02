extern crate bitflags;

use crate::bus;
use crate::bus::Bus;

#[derive(Debug)]
struct Instruction {
    opcode: u8,
    //opcode2: u8,
    bytes: u8,
    cycles: u8,
    mnemonic: &'static str,
}

macro_rules! instr_set {
    ($({ $o: expr, $b: expr, $c: expr, $mn: expr }),* $(,)?) => {
	[
	    $(Instruction { opcode: $o, bytes: $b, cycles: $c, mnemonic: $mn }),*
	]
    };
}

const INSTR_SET_INTEL: [Instruction; 256] = instr_set![
    {0x00, 1, 4, "NOP"}, {0x01, 3, 10, "LXI"}, {0x02, 1, 7, "STAX"}, {0x03, 1, 5, "INX"},
    {0x04, 1, 5, "INR"}, {0x05, 1, 5, "DCR"}, {0x06, 2, 7, "MVI"}, {0x07, 1, 4, "RLC"},
    {0x08, 1, 4, "*NOP"}, {0x09, 1, 10, "DAD"}, {0x0a, 1, 7, "LDAX"}, {0x0b, 1, 5, "DCX"},
    {0x0c, 1, 5, "INR"}, {0x0d, 1, 5, "DCR"}, {0x0e, 2, 7, "MVI"}, {0x0f, 1, 4, "RRC"},
    {0x10, 1, 4, "*NOP"}, {0x11, 3, 10, "LXI"}, {0x12, 1, 7, "STAX"}, {0x13, 1, 5, "INX"},
    {0x14, 1, 5, "INR"}, {0x15, 1, 5, "DCR"}, {0x16, 2, 7, "MVI"}, {0x17, 1, 4, "RAL"},
    {0x18, 1, 4, "*NOP"}, {0x19, 1, 10, "DAD"}, {0x1a, 1, 7, "LDAX"}, {0x1b, 1, 5, "DCX"},
    {0x1c, 1, 5, "INR"}, {0x1d, 1, 5, "DCR"}, {0x1e, 2, 7, "MVI"}, {0x1f, 1, 4, "RAR"},
    {0x20, 1, 4, "*NOP"}, {0x21, 3, 10, "LXI"}, {0x22, 3, 16, "SHLD"}, {0x23, 1, 5, "INX"},
    {0x24, 1, 5, "INR"}, {0x25, 1, 5, "DCR"}, {0x26, 2, 7, "MVI"}, {0x27, 1, 4, "DAA"},
    {0x28, 1, 4, "*NOP"}, {0x29, 1, 10, "DAD"}, {0x2a, 3, 16, "LHLD"}, {0x2b, 1, 5, "DCX"},
    {0x2c, 1, 5, "INR"}, {0x2d, 1, 5, "DCR"}, {0x2e, 2, 7, "MVI"}, {0x2f, 1, 4, "CMA"},
    {0x30, 1, 4, "*NOP"}, {0x31, 3, 10, "LXI"}, {0x32, 3, 13, "STA"}, {0x33, 1, 5, "INX"},
    {0x34, 1, 10, "INR"}, {0x35, 1, 10, "DCR"}, {0x36, 2, 10, "MVI"}, {0x37, 1, 4, "STC"},
    {0x38, 1, 4, "*NOP"}, {0x39, 1, 10, "DAD"}, {0x3a, 3, 13, "LDA"}, {0x3b, 1, 5, "DCX"},
    {0x3c, 1, 5, "INR"}, {0x3d, 1, 5, "DCR"}, {0x3e, 2, 7, "MVI"}, {0x3f, 1, 4, "CMC"},
    {0x40, 1, 5, "MOV"}, {0x41, 1, 5, "MOV"}, {0x42, 1, 5, "MOV"}, {0x43, 1, 5, "MOV"},
    {0x44, 1, 5, "MOV"}, {0x45, 1, 5, "MOV"}, {0x46, 1, 7, "MOV"}, {0x47, 1, 5, "MOV"},
    {0x48, 1, 5, "MOV"}, {0x49, 1, 5, "MOV"}, {0x4a, 1, 5, "MOV"}, {0x4b, 1, 5, "MOV"},
    {0x4c, 1, 5, "MOV"}, {0x4d, 1, 5, "MOV"}, {0x4e, 1, 7, "MOV"}, {0x4f, 1, 5, "MOV"},
    {0x50, 1, 5, "MOV"}, {0x51, 1, 5, "MOV"}, {0x52, 1, 5, "MOV"}, {0x53, 1, 5, "MOV"},
    {0x54, 1, 5, "MOV"}, {0x55, 1, 5, "MOV"}, {0x56, 1, 7, "MOV"}, {0x57, 1, 5, "MOV"},
    {0x58, 1, 5, "MOV"}, {0x59, 1, 5, "MOV"}, {0x5a, 1, 5, "MOV"}, {0x5b, 1, 5, "MOV"},
    {0x5c, 1, 5, "MOV"}, {0x5d, 1, 5, "MOV"}, {0x5e, 1, 7, "MOV"}, {0x5f, 1, 5, "MOV"},
    {0x60, 1, 5, "MOV"}, {0x61, 1, 5, "MOV"}, {0x62, 1, 5, "MOV"}, {0x63, 1, 5, "MOV"},
    {0x64, 1, 5, "MOV"}, {0x65, 1, 5, "MOV"}, {0x66, 1, 7, "MOV"}, {0x67, 1, 5, "MOV"},
    {0x68, 1, 5, "MOV"}, {0x69, 1, 5, "MOV"}, {0x6a, 1, 5, "MOV"}, {0x6b, 1, 5, "MOV"},
    {0x6c, 1, 5, "MOV"}, {0x6d, 1, 5, "MOV"}, {0x6e, 1, 7, "MOV"}, {0x6f, 1, 5, "MOV"},
    {0x70, 1, 7, "MOV"}, {0x71, 1, 7, "MOV"}, {0x72, 1, 7, "MOV"}, {0x73, 1, 7, "MOV"},
    {0x74, 1, 7, "MOV"}, {0x75, 1, 7, "MOV"}, {0x76, 1, 7, "HLT"}, {0x77, 1, 7, "MOV"},
    {0x78, 1, 5, "MOV"}, {0x79, 1, 5, "MOV"}, {0x7a, 1, 5, "MOV"}, {0x7b, 1, 5, "MOV"},
    {0x7c, 1, 5, "MOV"}, {0x7d, 1, 5, "MOV"}, {0x7e, 1, 7, "MOV"}, {0x7f, 1, 5, "MOV"},
    {0x80, 1, 4, "ADD"}, {0x81, 1, 4, "ADD"}, {0x82, 1, 4, "ADD"}, {0x83, 1, 4, "ADD"},
    {0x84, 1, 4, "ADD"}, {0x85, 1, 4, "ADD"}, {0x86, 1, 7, "ADD"}, {0x87, 1, 4, "ADD"},
    {0x88, 1, 4, "ADC"}, {0x89, 1, 4, "ADC"}, {0x8a, 1, 4, "ADC"}, {0x8b, 1, 4, "ADC"},
    {0x8c, 1, 4, "ADC"}, {0x8d, 1, 4, "ADC"}, {0x8e, 1, 7, "ADC"}, {0x8f, 1, 4, "ADC"},
    {0x90, 1, 4, "SUB"}, {0x91, 1, 4, "SUB"}, {0x92, 1, 4, "SUB"}, {0x93, 1, 4, "SUB"},
    {0x94, 1, 4, "SUB"}, {0x95, 1, 4, "SUB"}, {0x96, 1, 7, "SUB"}, {0x97, 1, 4, "SUB"},
    {0x98, 1, 4, "SBB"}, {0x99, 1, 4, "SBB"}, {0x9a, 1, 4, "SBB"}, {0x9b, 1, 4, "SBB"},
    {0x9c, 1, 4, "SBB"}, {0x9d, 1, 4, "SBB"}, {0x9e, 1, 7, "SBB"}, {0x9f, 1, 4, "SBB"},
    {0xa0, 1, 4, "ANA"}, {0xa1, 1, 4, "ANA"}, {0xa2, 1, 4, "ANA"}, {0xa3, 1, 4, "ANA"},
    {0xa4, 1, 4, "ANA"}, {0xa5, 1, 4, "ANA"}, {0xa6, 1, 7, "ANA"}, {0xa7, 1, 4, "ANA"},
    {0xa8, 1, 4, "XRA"}, {0xa9, 1, 4, "XRA"}, {0xaa, 1, 4, "XRA"}, {0xab, 1, 4, "XRA"},
    {0xac, 1, 4, "XRA"}, {0xad, 1, 4, "XRA"}, {0xae, 1, 7, "XRA"}, {0xaf, 1, 4, "XRA"},
    {0xb0, 1, 4, "ORA"}, {0xb1, 1, 4, "ORA"}, {0xb2, 1, 4, "ORA"}, {0xb3, 1, 4, "ORA"},
    {0xb4, 1, 4, "ORA"}, {0xb5, 1, 4, "ORA"}, {0xb6, 1, 7, "ORA"}, {0xb7, 1, 4, "ORA"},
    {0xb8, 1, 4, "CMP"}, {0xb9, 1, 4, "CMP"}, {0xba, 1, 4, "CMP"}, {0xbb, 1, 4, "CMP"},
    {0xbc, 1, 4, "CMP"}, {0xbd, 1, 4, "CMP"}, {0xbe, 1, 7, "CMP"}, {0xbf, 1, 4, "CMP"},
    {0xc0, 1, 5, "RNZ"}, {0xc1, 1, 10, "POP"}, {0xc2, 3, 10, "JNZ"}, {0xc3, 3, 10, "JMP"},
    {0xc4, 3, 11, "CNZ"}, {0xc5, 1, 11, "PUSH"}, {0xc6, 2, 7, "ADI"}, {0xc7, 1, 11, "RST"},
    {0xc8, 1, 5, "RZ"}, {0xc9, 1, 10, "RET"}, {0xca, 3, 10, "JZ"}, {0xcb, 3, 10, "*JMP"},
    {0xcc, 3, 11, "CZ"}, {0xcd, 3, 17, "CALL"}, {0xce, 2, 7, "ACI"}, {0xcf, 1, 11, "RST"},
    {0xd0, 1, 5, "RNC"}, {0xd1, 1, 10, "POP"}, {0xd2, 3, 10, "JNC"}, {0xd3, 2, 10, "OUT"},
    {0xd4, 3, 11, "CNC"}, {0xd5, 1, 11, "PUSH"}, {0xd6, 2, 7, "SUI"}, {0xd7, 1, 11, "RST"},
    {0xd8, 1, 5, "RC"}, {0xd9, 1, 10, "*RET"}, {0xda, 3, 10, "JC"}, {0xdb, 2, 10, "IN"},
    {0xdc, 3, 11, "CC"}, {0xdd, 3, 17, "*CALL"}, {0xde, 2, 7, "SBI"}, {0xdf, 1, 11, "RST"},
    {0xe0, 1, 5, "RPO"}, {0xe1, 1, 10, "POP"}, {0xe2, 3, 10, "JPO"}, {0xe3, 1, 18, "XTHL"},
    {0xe4, 3, 11, "CPO"}, {0xe5, 1, 11, "PUSH"}, {0xe6, 2, 7, "ANI"}, {0xe7, 1, 11, "RST"},
    {0xe8, 1, 5, "RPE"}, {0xe9, 1, 5, "PCHL"}, {0xea, 3, 10, "JPE"}, {0xeb, 1, 5, "XCHG"},
    {0xec, 3, 11, "CPE"}, {0xed, 3, 17, "*CALL"}, {0xee, 2, 7, "XRI"}, {0xef, 1, 11, "RST"},
    {0xf0, 1, 5, "RP"}, {0xf1, 1, 10, "POP"}, {0xf2, 3, 10, "JP"}, {0xf3, 1, 4, "DI"},
    {0xf4, 3, 11, "CP"}, {0xf5, 1, 11, "PUSH"}, {0xf6, 2, 7, "ORI"}, {0xf7, 1, 11, "RST"},
    {0xf8, 1, 5, "RM"}, {0xf9, 1, 5, "SPHL"}, {0xfa, 3, 10, "JM"}, {0xfb, 1, 4, "EI"},
    {0xfc, 3, 11, "CM"}, {0xfd, 3, 17, "*CALL"}, {0xfe, 2, 7, "CPI"}, {0xff, 1, 11, "RST"},
];

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    struct PSW: u8 {
	const S = 0b10000000;
	const Z = 0b01000000;
	const F5 = 0b00100000;
	const H = 0b00010000;
	const F3 = 0b00001000;
	const P = 0b00000100;
	const N = 0b00000010;
	const C = 0b00000001;
    }
}

impl PSW {
    pub fn as_u8(&self) -> u8 {
	self.bits() as u8
    }
}

pub struct Cpu {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    shadow: [u8; 8],
    ix: u16,
    iy: u16,
    sp: u16,
    pub pc: u16,
    f: PSW,
    pub iff: bool,
    iff2: bool,
    i: u8,
    pub bus: bus::ZXBus,
    instr_set: &'static [Instruction; 256],
    pub cycles: usize,
    ei_pend: bool,
}

impl Cpu {
    pub fn new() -> Self {
	Cpu {
	    a: 0,
	    b: 0,
	    c: 0,
	    d: 0,
	    e: 0,
	    h: 0,
	    l: 0,
	    shadow: [0; 8],
	    ix: 0,
	    iy: 0,
	    sp: 0,
	    pc: 0,
	    f: PSW::empty(),
	    iff: false,
	    iff2: false,
	    i: 0,
	    bus: bus::ZXBus::new(),
	    instr_set: &INSTR_SET_INTEL,
	    cycles: 0,
	    ei_pend: false,
	}
    }

    pub fn reset(&mut self) {
	self.pc = 0;
	self.iff = false;
	self.iff2 = false;
	self.i = 0;
    }

    fn read_rp(&self, rp: u8) -> u16 {
	let rpl = match rp {
	    0 => self.c,
	    1 => self.e,
	    2 => self.l,
	    _ => (self.sp & 0xff) as u8,
	};
	let rph = match rp {
	    0 => self.b,
	    1 => self.d,
	    2 => self.h,
	    _ => ((self.sp >> 8) & 0xff) as u8,
	};
	((rph as u16) << 8) | rpl as u16
    }

    fn write_rp(&mut self, rp: u8, data: u16) {
	let hi = ((data >> 8) & 0xff) as u8;
	let lo = (data & 0xff) as u8;

	match rp {
	    0 => {
		self.b = hi;
		self.c = lo;
	    },
	    1 => {
		self.d = hi;
		self.e = lo;
	    },
	    2 => {
		self.h = hi;
		self.l = lo;
	    },
	    _ => {
		self.sp = data;
	    },
	};
    }

    fn push_word(&mut self, data: u16) {
	let hi = ((data & 0xff00) >> 8) as u8;
	let lo = (data & 0x00ff) as u8;
	self.bus.write_byte(self.sp.wrapping_sub(1), hi);
	self.bus.write_byte(self.sp.wrapping_sub(2), lo);
	self.sp = self.sp.wrapping_sub(2);
    }

    fn pop_word(&mut self) -> u16 {
	let lo = self.bus.read_byte(self.sp);
	let hi = self.bus.read_byte(self.sp.wrapping_add(1));
	self.sp = self.sp.wrapping_add(2);
	((hi as u16) << 8) | (lo as u16)
    }

    fn movb(&mut self, d: u8, s: u8, hlptr: u16) {
	match d {
	    0 => self.b = s,
	    1 => self.c = s,
	    2 => self.d = s,
	    3 => self.e = s,
	    4 => self.h = s,
	    5 => self.l = s,
	    6 => self.bus.write_byte(hlptr, s),
	    _ => self.a = s,
	};
    }

    fn aluop(&mut self, op: u8, s: u8) {
	let mut tmp = self.a as u16;
	let cflag = self.f.contains(PSW::C) as u16;
	match op {
	    0 => { //ADD
		tmp = tmp.wrapping_add(s as u16);
		self.f.set(PSW::H, ((self.a & 0xf) + (s & 0xf)) > 0x0f);
		self.f.set(PSW::C, tmp > 0xff);
	    },
	    1 => { //ADC
		tmp = tmp.wrapping_add(s as u16).wrapping_add(cflag);
		self.f.set(PSW::H, ((self.a & 0xf) + (s & 0xf) + cflag as u8) > 0x0f);
		self.f.set(PSW::C, tmp > 0xff);
	    },
	    2 => { //SUB
		tmp = tmp.wrapping_sub(s as u16);
		self.f.set(PSW::H, ((self.a & 0xf) + ((!s & 0xff) & 0xf) + 1) > 0x0f);
		self.f.set(PSW::C, tmp > 0xff);
	    },
	    3 => { //SBB
		tmp = tmp.wrapping_sub(s as u16).wrapping_sub(cflag);
		self.f.set(PSW::H, ((self.a & 0xf) + ((!s & 0xff) & 0xf) + ((!cflag as u8) & 1)) > 0x0f);
		self.f.set(PSW::C, tmp > 0xff);
	    },
	    4 => { //ANA
		tmp = (self.a & s) as u16;
		self.f.set(PSW::C, false);
		self.f.set(PSW::H, ((self.a | s) & 0x08) != 0);
	    },
	    5 => { //XRA
		tmp = (self.a ^ s) as u16;
		self.f.set(PSW::C, false);
		self.f.set(PSW::H, false);
	    },
	    6 => { //ORA
		tmp = (self.a | s) as u16;
		self.f.set(PSW::C, false);
		self.f.set(PSW::H, false);
	    },
	    _ => { //CMP
		tmp = tmp.wrapping_sub(s as u16);
		self.f.set(PSW::H, ((self.a & 0xf) + ((!s & 0xff) & 0xf) + 1) > 0x0f);
		self.f.set(PSW::C, tmp > 0xff);
	    },
	};
	self.f.set(PSW::Z, (tmp & 0xff) == 0);
	self.f.set(PSW::S, (tmp & 0x80) != 0);
	self.f.set(PSW::P, (((tmp & 0xff) as u8).count_ones() % 2) == 0);
	if op != 7 { //CMP doesn't modify a
	    self.a = tmp as u8;
	}
    }

    fn branch(&mut self, op: u8, c: u8, uncond: u8, addr: u16) {
	let c2 = if uncond != 0 {
	    8
	} else {
	    c
	};
	let cond: bool = match c2 {
	    0 => !self.f.contains(PSW::Z),
	    1 => self.f.contains(PSW::Z),
	    2 => !self.f.contains(PSW::C),
	    3 => self.f.contains(PSW::C),
	    4 => !self.f.contains(PSW::P),
	    5 => self.f.contains(PSW::P),
	    6 => !self.f.contains(PSW::S),
	    7 => self.f.contains(PSW::S),
	    _ => true,
	};
	match op {
	    0 => { //RET
		if cond {
		    self.pc = self.pop_word();
		    self.cycles += 6;
		}
	    },
	    1 => { //JMP
		if cond {
		    self.pc = addr;
		}
	    },
	    2 => { //CALL
		if cond {
		    self.push_word(self.pc);
		    self.pc = addr;
		    self.cycles += 6;
		}
	    },
	    _ => { //RST
		self.push_word(self.pc);
		self.pc = (c << 3) as u16;
	    },
	};
    }

    pub fn step(&mut self) -> usize {
	let oldcycles = self.cycles;
	let mut opcode: u8 = self.bus.read_byte(self.pc);
	if self.bus.irq && self.iff {
	    self.pc -= 1; //1 byte will be added later, want to ret back to interrupted instr
	    self.iff = false;
	    self.bus.irq = false;
	    opcode = self.bus.irq_vec;
	}
	if self.ei_pend {
	    self.iff = true;
	    self.ei_pend = false;
	    //there will be no chance for interrupts until
	    //after this instruction runs so we have effectively
	    //gotten the one instruction delay specified in the manual
	}
	//todo: decode extended opcodes
	let instr: &Instruction = &self.instr_set[opcode as usize];
	self.cycles += instr.cycles as usize;
	let d_bits = (opcode >> 3) & 7;
	let s = opcode & 7;
	let rp = (opcode >> 4) & 3;
	let c = d_bits;
	let n = d_bits;
	let hlptr = self.read_rp(2);

	let s = match s {
	    0b000 => self.b,
	    0b001 => self.c,
	    0b010 => self.d,
	    0b011 => self.e,
	    0b100 => self.h,
	    0b101 => self.l,
	    0b110 => self.bus.read_byte(hlptr),
	    _ => self.a,
	};

	let d = match d_bits {
	    0b000 => self.b,
	    0b001 => self.c,
	    0b010 => self.d,
	    0b011 => self.e,
	    0b100 => self.h,
	    0b101 => self.l,
	    0b110 => self.bus.read_byte(hlptr), //bad trouble, but only used in like 4 instructions so its ok for now
	    _ => self.a,
	};

	let op1 = self.bus.read_byte(self.pc.wrapping_add(1));
	let op2 = self.bus.read_byte(self.pc.wrapping_add(2));
	let opw = ((op2 as u16) << 8) | op1 as u16;

	//println!("A {:02X} F {:02X} B {:02X} C {:02X} D {:02X} E {:02X} H {:02X} L {:02X} SP {:04X}, CYC: {} ime {}",
	//	 self.a, self.f.as_u8(), self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.cycles, self.ime);
	//disas_zilog(self.pc, instr.opcode, op1, op2, opw);

	self.pc = self.pc.wrapping_add(instr.bytes as u16);

	match opcode {
	    0x00 => { //NOP
		//nothing
	    },
	    0x76 => { //HLT
		return 0; //todo proper behavior
	    },
	    0x40..=0x7f => { //LD r1, r2
		self.movb(d_bits, s, hlptr);
	    },
	    0x06 | 0x16 | 0x26 | 0x36 |
	    0x0e | 0x1e | 0x2e | 0x3e => { //LD r, n
		self.movb(d_bits, op1, hlptr);
	    },
	    0x01 | 0x11 | 0x21 | 0x31 => { //LD rp, nn
		self.write_rp(rp, opw);
	    },
	    0x02 | 0x12 => { //LD (rp), A
		let tmp = self.read_rp(rp);
		self.bus.write_byte(tmp, self.a);
	    },
	    0x0a | 0x1a => { //LD A, (rp)
		let tmp = self.read_rp(rp);
		self.a = self.bus.read_byte(tmp);
	    },
	    0x22 => { //LD (nn), HL
		let tmp = self.read_rp(2);
		self.bus.write_byte(opw, (tmp & 0xff) as u8);
		self.bus.write_byte(opw + 1, ((tmp >> 8) & 0xff) as u8);
	    },
	    0x2a => { //LD HL, (nn)
		let lo = self.bus.read_byte(opw);
		let hi = self.bus.read_byte(opw + 1);
		self.write_rp(2, ((hi as u16) << 8) | lo as u16);
	    },
	    0x32 => { //LD (nn), A
		self.bus.write_byte(opw, self.a);
	    },
	    0x3a => { //LD A, (nn)
		self.a = self.bus.read_byte(opw);
	    },
	    0xc5 | 0xd5 | 0xe5 | 0xf5 => { //PUSH
		let mut tmp = self.read_rp(rp);
		if rp == 3 {
		    tmp = ((self.a as u16) << 8) | (self.f.as_u8() as u16);
		}
		self.push_word(tmp);
	    },
	    0xc1 | 0xd1 | 0xe1 | 0xf1 => { //POP
		let tmp = self.pop_word();
		if rp != 3 {
		    self.write_rp(rp, tmp);
		} else {
		    self.a = ((tmp & 0xff00) >> 8) as u8;
		    self.f = PSW::from_bits((tmp & 0x00ff) as u8).unwrap();
		    //todo undocumented bits
		}
	    },
	    0xe3 => { //EX (SP), HL
		let tmp = self.pop_word();
		let hl = self.read_rp(2);
		self.push_word(hl);
		self.write_rp(2, tmp);
	    },
	    0xf9 => { //LD SP, HL
		self.sp = self.read_rp(2);
	    },
	    0xeb => { //EX DE, HL
		let tmp = self.h;
		self.h = self.d;
		self.d = tmp;
		let tmp = self.l;
		self.l = self.e;
		self.e = tmp;
	    },
	    0x03 | 0x13 | 0x23 | 0x33 => { //INC rp
		let tmp = self.read_rp(rp);
		self.write_rp(rp, tmp.wrapping_add(1));
	    },
	    0x0b | 0x1b | 0x2b | 0x3b => { //DEC rp
		let tmp = self.read_rp(rp);
		self.write_rp(rp, tmp.wrapping_sub(1));
	    },
	    0x09 | 0x19 | 0x29 | 0x39 => { //ADD HL, rp
		let hltmp = self.read_rp(2) as u32;
		let rptmp = self.read_rp(rp) as u32;
		let tmp = hltmp + rptmp;
		self.f.set(PSW::C, tmp > 0xffff);
		self.write_rp(2, tmp as u16);
	    },
	    0x80..=0xbf => { //aluops A, r
		let op_bits: u8 = (opcode & 0x38) >> 3;
		self.aluop(op_bits, s);
	    },
	    0xc6 | 0xd6 | 0xe6 | 0xf6 |
	    0xce | 0xde | 0xee | 0xfe => { //aluops A, n
		let op_bits: u8 = (opcode & 0x38) >> 3;
		self.aluop(op_bits, op1);
	    },
	    0x04 | 0x14 | 0x24 | 0x34 |
	    0x0c | 0x1c | 0x2c | 0x3c => { //INC r
		let mut tmp = d.wrapping_add(1) as u16;
		if d_bits == 6 {
		    tmp = self.bus.read_byte(hlptr).wrapping_add(1) as u16;
		}
		self.f.set(PSW::Z, tmp == 0);
		self.f.set(PSW::S, (tmp & 0x80) != 0);
		self.f.set(PSW::P, (((tmp & 0xff) as u8).count_ones() % 2) == 0);
		self.f.set(PSW::H, ((d & 0x0f).wrapping_add(1)) > 0x0f);
		let tmp = tmp as u8;
		match d_bits {
		    0 => self.b = tmp,
		    1 => self.c = tmp,
		    2 => self.d = tmp,
		    3 => self.e = tmp,
		    4 => self.h = tmp,
		    5 => self.l = tmp,
		    6 => self.bus.write_byte(hlptr, tmp),
		    _ => self.a = tmp,
		};
	    },
	    0x05 | 0x15 | 0x25 | 0x35 |
	    0x0d | 0x1d | 0x2d | 0x3d => { //DEC r
		let mut tmp = d.wrapping_sub(1) as u16;
		if d_bits == 6 {
		    tmp = self.bus.read_byte(hlptr).wrapping_sub(1) as u16;
		}
		self.f.set(PSW::Z, tmp == 0);
		self.f.set(PSW::S, (tmp & 0x80) != 0);
		self.f.set(PSW::P, (((tmp & 0xff) as u8).count_ones() % 2) == 0);
		self.f.set(PSW::H, (d & 0x0f) != 0);
		let tmp = tmp as u8;
		match d_bits {
		    0 => self.b = tmp,
		    1 => self.c = tmp,
		    2 => self.d = tmp,
		    3 => self.e = tmp,
		    4 => self.h = tmp,
		    5 => self.l = tmp,
		    6 => self.bus.write_byte(hlptr, tmp),
		    _ => self.a = tmp,
		};
	    },
	    0x07 => { //RLCA
		self.f.set(PSW::C, ((self.a & 0x80) >> 7) != 0);
		self.a = self.a << 1;
		self.a = self.a | (self.f.contains(PSW::C) as u8);
	    },
	    0x17 => { //RLA
		let tmp = self.f.contains(PSW::C) as u8;
		self.f.set(PSW::C, ((self.a & 0x80) >> 7) != 0);
		self.a = self.a << 1;
		self.a = self.a | tmp;
	    },
	    0x27 => { //DAA
		let mut tmp = self.a as u16;
		if ((tmp & 0x0f) > 0x09) || self.f.contains(PSW::H) {
		    self.f.set(PSW::H, (((tmp & 0x0f) + 0x06) & 0xf0) != 0);
		    tmp += 6;
		    if (tmp & 0xff00) != 0 {
			self.f.insert(PSW::C);
		    }
		}
		if ((tmp & 0xf0) > 0x90) || self.f.contains(PSW::C) {
		    tmp += 0x60;
		    if (tmp & 0xff00) != 0 {
			self.f.insert(PSW::C);
		    }
		}
		self.f.set(PSW::Z, (tmp & 0xff) == 0);
		self.f.set(PSW::S, (tmp & 0x80) != 0);
		self.f.set(PSW::P, (((tmp & 0xff) as u8).count_ones() % 2) == 0);
		self.a = tmp as u8;
	    },
	    0x37 => { //SCF
		self.f.insert(PSW::C);
	    },
	    0x0f => { //RRCA
		self.f.set(PSW::C, (self.a & 1) != 0);
		self.a = ((self.a & 1) << 7) | (self.a >> 1);
	    },
	    0x1f => { //RRA
		let tmp = (self.f.contains(PSW::C) as u8) << 7;
		self.f.set(PSW::C, (self.a & 1) != 0);
		self.a = self.a >> 1;
		self.a = self.a | tmp;
	    },
	    0x2f => { //CPL
		self.a = !self.a;
	    },
	    0x3f => { //CCF
		self.f.toggle(PSW::C);
	    },
	    0xe9 => { //JP (HL)
		self.pc = hlptr;
	    },
	    0xcb => { //prefix CB
		todo!("prefix cb");
	    },
	    0xdd => { //prefix DD
		todo!("prefix dd");
	    },
	    0xed => { //prefix ED
		todo!("prefix ed");
	    },
	    0xfd => { //prefix fd
		todo!("prefix fd");
	    },
	    0xc0 | 0xc2..=0xc4 | 0xc7..=0xcd | 0xcf |
	    0xd0 | 0xd2 | 0xd4 | 0xd7 | 0xd8 | 0xda | 0xdc | 0xdf |
	    0xe0 | 0xe2 | 0xe4 | 0xe7..=0xea | 0xec | 0xef |
	    0xf0 | 0xf2 | 0xf4 | 0xf7 | 0xf8 | 0xfa | 0xfc | 0xff => { //branches
		self.branch((opcode & 6) >> 1, c, opcode & 1, opw);
	    },
	    0xd3 => { //OUT (n), A
		self.bus.write_io_byte(op1, self.a);
	    },
	    0xdb => { //IN A, (n)
		self.a = self.bus.read_io_byte(op1);
	    },
	    0xf3 => { //DI
		self.iff = false;
	    },
	    0xfb => { //EI
		self.ei_pend = true;
	    },
	    0x08 => { //EX AF, AF'
		let tmp = self.shadow;
		self.shadow[6] = self.a;
		self.shadow[7] = self.f.as_u8();
		self.a = tmp[0];
		self.f = PSW::from_bits(tmp[1]).unwrap();
	    },
	    0x10 | 0x20 | 0x30 |
	    0x18 | 0x28 | 0x38 => { //DJNZ + JR
		todo!();
	    },
	    0xd9 => { //EXX
		todo!();
	    },
	};
	    
	self.cycles - oldcycles
    }
}

fn disas(pc: u16, opcode: u8, op1: u8, opw: u16) {
    print!("{:04X} ", pc);
    match opcode {
	0x00 => println!("NOP"),
	0x01 => println!("LD BC, ${opw:04X}"),
	0x02 => println!("LD [BC], A"),
	0x03 => println!("INC BC"),
	0x04 => println!("INC B"),
	0x05 => println!("DEC B"),
	0x06 => println!("LD B, ${op1:02X}"),
	0x07 => println!("RLCA"),
	0x08 => println!("EX AF, AF'"),
	0x09 => println!("ADD HL, BC"),
	0x0A => println!("LD A, [BC]"),
	0x0B => println!("DEC BC"),
	0x0C => println!("INC C"),
	0x0D => println!("DEC C"),
	0x0E => println!("LD C, ${op1:02X}"),
	0x0F => println!("RRCA"),
	0x10 => println!("DJNZ ${op1:02X}"), //todo calculate target, same for JRs
	0x11 => println!("LD DE, ${opw:04X}"),
	0x12 => println!("LD [DE], A"),
	0x13 => println!("INC DE"),
	0x14 => println!("INC D"),
	0x15 => println!("DEC D"),
	0x16 => println!("LD D, ${op1:02X}"),
	0x17 => println!("RLA"),
	0x18 => println!("JR ${op1:02X}"),
	0x19 => println!("ADD HL, DE"),
	0x1A => println!("LD A, [DE]"),
	0x1B => println!("DEC DE"),
	0x1C => println!("INC E"),
	0x1D => println!("DEC E"),
	0x1E => println!("LD E, ${op1:02X}"),
	0x1F => println!("RRA"),
	0x20 => println!("JR NZ, ${op1:02X}"),
	0x21 => println!("LD HL, ${opw:04X}"),
	0x22 => println!("LD [${opw:04X}], HL"),
	0x23 => println!("INC HL"),
	0x24 => println!("INC H"),
	0x25 => println!("DEC H"),
	0x26 => println!("LD H, ${op1:02X}"),
	0x27 => println!("DAA"),
	0x28 => println!("JR Z, ${op1:02X}"),
	0x29 => println!("ADD HL, HL"),
	0x2A => println!("LD HL, [${opw:04X}]"),
	0x2B => println!("DEC HL"),
	0x2C => println!("INC L"),
	0x2D => println!("DEC L"),
	0x2E => println!("LD L, ${op1:02X}"),
	0x2F => println!("CPL"),
	0x30 => println!("JR NC, ${op1:02X}"),
	0x31 => println!("LD SP, ${opw:04X}"),
	0x32 => println!("LD [${opw:04X}], A"),
	0x33 => println!("INC SP"),
	0x34 => println!("INC [HL]"),
	0x35 => println!("DEC [HL]"),
	0x36 => println!("LD [HL], ${op1:02X}"),
	0x37 => println!("SCF"),
	0x38 => println!("JR C, ${op1:02X}"),
	0x39 => println!("ADD HL, SP"),
	0x3A => println!("LD A, [${opw:04X}]"),
	0x3B => println!("DEC SP"),
	0x3C => println!("INC A"),
	0x3D => println!("DEC A"),
	0x3E => println!("LD A, ${op1:02X}"),
	0x3F => println!("CCF"),
	0x40 => println!("LD B, B"),
	0x41 => println!("LD B, C"),
	0x42 => println!("LD B, D"),
	0x43 => println!("LD B, E"),
	0x44 => println!("LD B, H"),
	0x45 => println!("LD B, L"),
	0x46 => println!("LD B, [HL]"),
	0x47 => println!("LD B, A"),
	0x48 => println!("LD C, B"),
	0x49 => println!("LD C, C"),
	0x4A => println!("LD C, D"),
	0x4B => println!("LD C, E"),
	0x4C => println!("LD C, H"),
	0x4D => println!("LD C, L"),
	0x4E => println!("LD C, [HL]"),
	0x4F => println!("LD C, A"),
	0x50 => println!("LD D, B"),
	0x51 => println!("LD D, C"),
	0x52 => println!("LD D, D"),
	0x53 => println!("LD D, E"),
	0x54 => println!("LD D, H"),
	0x55 => println!("LD D, L"),
	0x56 => println!("LD D, [HL]"),
	0x57 => println!("LD D, A"),
	0x58 => println!("LD E, B"),
	0x59 => println!("LD E, C"),
	0x5A => println!("LD E, D"),
	0x5B => println!("LD E, E"),
	0x5C => println!("LD E, H"),
	0x5D => println!("LD E, L"),
	0x5E => println!("LD E, [HL]"),
	0x5F => println!("LD E, A"),
	0x60 => println!("LD H, B"),
	0x61 => println!("LD H, C"),
	0x62 => println!("LD H, D"),
	0x63 => println!("LD H, E"),
	0x64 => println!("LD H, H"),
	0x65 => println!("LD H, L"),
	0x66 => println!("LD H, [HL]"),
	0x67 => println!("LD H, A"),
	0x68 => println!("LD L, B"),
	0x69 => println!("LD L, C"),
	0x6A => println!("LD L, D"),
	0x6B => println!("LD L, E"),
	0x6C => println!("LD L, H"),
	0x6D => println!("LD L, L"),
	0x6E => println!("LD L, [HL]"),
	0x6F => println!("LD L, A"),
	0x70 => println!("LD [HL], B"),
	0x71 => println!("LD [HL], C"),
	0x72 => println!("LD [HL], D"),
	0x73 => println!("LD [HL], E"),
	0x74 => println!("LD [HL], H"),
	0x75 => println!("LD [HL], L"),
	0x76 => println!("HLT"),
	0x77 => println!("LD [HL], A"),
	0x78 => println!("LD A, B"),
	0x79 => println!("LD A, C"),
	0x7A => println!("LD A, D"),
	0x7B => println!("LD A, E"),
	0x7C => println!("LD A, H"),
	0x7D => println!("LD A, L"),
	0x7E => println!("LD A, [HL]"),
	0x7F => println!("LD A, A"),
	0x80 => println!("ADD A, B"),
	0x81 => println!("ADD A, C"),
	0x82 => println!("ADD A, D"),
	0x83 => println!("ADD A, E"),
	0x84 => println!("ADD A, H"),
	0x85 => println!("ADD A, L"),
	0x86 => println!("ADD A, [HL]"),
	0x87 => println!("ADD A, A"),
	0x88 => println!("ADC A, B"),
	0x89 => println!("ADC A, C"),
	0x8A => println!("ADC A, D"),
	0x8B => println!("ADC A, E"),
	0x8C => println!("ADC A, H"),
	0x8D => println!("ADC A, L"),
	0x8E => println!("ADC A, [HL]"),
	0x8F => println!("ADC A, A"),
	0x90 => println!("SUB A, B"),
	0x91 => println!("SUB A, C"),
	0x92 => println!("SUB A, D"),
	0x93 => println!("SUB A, E"),
	0x94 => println!("SUB A, H"),
	0x95 => println!("SUB A, L"),
	0x96 => println!("SUB A, [HL]"),
	0x97 => println!("SUB A, A"),
	0x98 => println!("SBC A, B"),
	0x99 => println!("SBC A, C"),
	0x9A => println!("SBC A, D"),
	0x9B => println!("SBC A, E"),
	0x9C => println!("SBC A, H"),
	0x9D => println!("SBC A, L"),
	0x9E => println!("SBC A, M"),
	0x9F => println!("SBC A, A"),
	0xA0 => println!("AND A, B"),
	0xA1 => println!("AND A, C"),
	0xA2 => println!("AND A, D"),
	0xA3 => println!("AND A, E"),
	0xA4 => println!("AND A, H"),
	0xA5 => println!("AND A, L"),
	0xA6 => println!("AND A, [HL]"),
	0xA7 => println!("AND A, A"),
	0xA8 => println!("XOR A, B"),
	0xA9 => println!("XOR A, C"),
	0xAA => println!("XOR A, D"),
	0xAB => println!("XOR A, E"),
	0xAC => println!("XOR A, H"),
	0xAD => println!("XOR A, L"),
	0xAE => println!("XOR A, [HL]"),
	0xAF => println!("XOR A, A"),
	0xB0 => println!("OR A, B"),
	0xB1 => println!("OR A, C"),
	0xB2 => println!("OR A, D"),
	0xB3 => println!("OR A, E"),
	0xB4 => println!("OR A, H"),
	0xB5 => println!("OR A, L"),
	0xB6 => println!("OR A, [HL]"),
	0xB7 => println!("OR A, A"),
	0xB8 => println!("CP A, B"),
	0xB9 => println!("CP A, C"),
	0xBA => println!("CP A, D"),
	0xBB => println!("CP A, E"),
	0xBC => println!("CP A, H"),
	0xBD => println!("CP A, L"),
	0xBE => println!("CP A, [HL]"),
	0xBF => println!("CP A, A"),
	0xC0 => println!("RET NZ"),
	0xC1 => println!("POP BC"),
	0xC2 => println!("JP NZ, ${opw:04X}"),
	0xC3 => println!("JP ${opw:04X}"),
	0xC4 => println!("CALL NZ, ${opw:04X}"),
	0xC5 => println!("PUSH BC"),
	0xC6 => println!("ADD A, ${op1:02X}"),
	0xC7 => println!("RST $00"),
	0xC8 => println!("RET Z"),
	0xC9 => println!("RET"),
	0xCA => println!("JP Z, ${opw:04X}"),
	0xCB => println!("prefix cb"),
	0xCC => println!("CALL Z, ${opw:04X}"),
	0xCD => println!("CALL ${opw:04X}"),
	0xCE => println!("ADC A, ${op1:02X}"),
	0xCF => println!("RST $08"),
	0xD0 => println!("RET NC"),
	0xD1 => println!("POP DE"),
	0xD2 => println!("JP NC, ${opw:04X}"),
	0xD3 => println!("OUT ${op1:02X}"),
	0xD4 => println!("CALL NC, ${opw:04X}"),
	0xD5 => println!("PUSH DE"),
	0xD6 => println!("SUB A, ${op1:02X}"),
	0xD7 => println!("RST $10"),
	0xD8 => println!("RET C"),
	0xD9 => println!("EXX"),
	0xDA => println!("JP C, ${opw:04X}"),
	0xDB => println!("IN ${op1:02X}"),
	0xDC => println!("CALL C, ${opw:04X}"),
	0xDD => println!("prefix dd"),
	0xDE => println!("SBC A, ${op1:02X}"),
	0xDF => println!("RST $18"),
	0xE0 => println!("RET PO"),
	0xE1 => println!("POP HL"),
	0xE2 => println!("JP PO, ${opw:04X}"),
	0xE3 => println!("LD [SP], HL"),
	0xE4 => println!("CALL PO, ${opw:04X}"),
	0xE5 => println!("PUSH HL"),
	0xE6 => println!("AND A, ${op1:02X}"),
	0xE7 => println!("RST $20"),
	0xE8 => println!("RET PE"),
	0xE9 => println!("JP HL"),
	0xEA => println!("JP PE, ${opw:04X}"),
	0xEB => println!("EX DE, HL"),
	0xEC => println!("CALL PE, ${opw:04X}"),
	0xED => println!("prefix ed"),
	0xEE => println!("XOR A, ${op1:02X}"),
	0xEF => println!("RST $28"),
	0xF0 => println!("RET P"),
	0xF1 => println!("POP AF"),
	0xF2 => println!("JP P, ${opw:04X}"),
	0xF3 => println!("DI"),
	0xF4 => println!("CALL P, ${opw:04X}"),
	0xF5 => println!("PUSH AF"),
	0xF6 => println!("OR A, ${op1:02X}"),
	0xF7 => println!("RST $30"),
	0xF8 => println!("RET M"),
	0xF9 => println!("LD SP, HL"),
	0xFA => println!("JP M, ${opw:04X}"),
	0xFB => println!("EI"),
	0xFC => println!("CALL M, ${opw:04X}"),
	0xFD => println!("prefix fd"),
	0xFE => println!("CP A, ${op1:02X}"),
	0xFF => println!("RST $38"),
    };
}
