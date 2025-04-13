mod z80;
mod bus;

use crate::bus::Bus;
use std::env;
use std::thread;
use std::time;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use sdl2::pixels::PixelFormatEnum;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::mixer::{InitFlag, AUDIO_S16LSB, DEFAULT_CHANNELS};

use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct State {
    pub pc: u16,
    pub sp: u16,
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: u8,
    pub h: u8,
    pub l: u8,
    pub i: u8,
    pub r: u8,
    //pub ei: u8,
    pub ix: u16,
    pub iy: u16,
    pub af_: u16,
    pub bc_: u16,
    pub de_: u16,
    pub hl_: u16,
    pub im: u8,
    //pub p: u8,
    pub iff1: u8,
    pub iff2: u8,
    pub q: u8,
    pub wz: u16,
    pub ram: Vec<(u16, u8)>,
    #[serde(default)]
    pub ports: Vec<(u16, u8, char)>,
}

impl State {
    pub fn new() -> Self {
	State {
	    pc: 0,
	    sp: 0,
	    a: 0,
	    b: 0,
	    c: 0,
	    d: 0,
	    e: 0,
	    f: 0,
	    h: 0,
	    l: 0,
	    i: 0,
	    r: 0,
	    ix: 0,
	    iy: 0,
	    af_: 0,
	    bc_: 0,
	    de_: 0,
	    hl_: 0,
	    im: 0,
	    iff1: 0,
	    iff2: 0,
	    q: 0,
	    wz: 0,
	    ram: Vec::new(),
	    ports: Vec::new(),
	}
    }
}

#[derive(Serialize, Deserialize)]
struct Test {
    pub name: String,
    pub initial: State,
    pub r#final: State,
}

fn main() {
    let mut cpu = z80::Cpu::new(false);
    let mut stdin = io::stdin();

    /*
    let mut entries: Vec<_> = std::fs::read_dir("/home/cookie/src/sstest-z80/v1/").unwrap()
	.map(|r| r.unwrap())
	.collect();
    entries.sort_by_key(|e| e.path());
    for entry in entries {
	let file = std::fs::File::open(entry.path()).unwrap();
	let reader = BufReader::new(file);
	let tests: Vec<Test> = serde_json::from_reader(reader).unwrap();
	for test in tests {
	    //load initial state
	    cpu.pc = test.initial.pc;
	    cpu.sp = test.initial.sp;
	    cpu.a = test.initial.a;
	    cpu.b = test.initial.b;
	    cpu.c = test.initial.c;
	    cpu.d = test.initial.d;
	    cpu.e = test.initial.e;
	    cpu.f = z80::PSW::from_bits(test.initial.f).unwrap();
	    cpu.h = test.initial.h;
	    cpu.l = test.initial.l;
	    cpu.i = test.initial.i;
	    cpu.r = test.initial.r;
	    cpu.ix = test.initial.ix;
	    cpu.iy = test.initial.iy;
	    cpu.shadow[6] = ((test.initial.af_ & 0xff00) >> 8) as u8;
	    cpu.shadow[7] = (test.initial.af_ & 0x00ff) as u8;
	    cpu.shadow[0] = ((test.initial.bc_ & 0xff00) >> 8) as u8;
	    cpu.shadow[1] = (test.initial.bc_ & 0x00ff) as u8;
	    cpu.shadow[2] = ((test.initial.de_ & 0xff00) >> 8) as u8;
	    cpu.shadow[3] = (test.initial.de_ & 0x00ff) as u8;
	    cpu.shadow[4] = ((test.initial.hl_ & 0xff00) >> 8) as u8;
	    cpu.shadow[5] = (test.initial.hl_ & 0x00ff) as u8;
	    cpu.im = test.initial.im;
	    cpu.iff = test.initial.iff1 != 0;
	    cpu.iff2 = test.initial.iff2 != 0;
	    cpu.q = test.initial.q;
	    cpu.wz = test.initial.wz;
	    for r in test.initial.ram {
		cpu.bus.write_byte(r.0, r.1);
	    }
	    for p in test.initial.ports {
		cpu.bus.write_io_byte(p.0, p.1);
	    }

	    let _ = cpu.step();
	    cpu.ei_pend = false;

	    //extract state to compare
	    let mut after: State = State::new();
	    after.pc = cpu.pc;
	    after.sp = cpu.sp;
	    after.a = cpu.a;
	    after.b = cpu.b;
	    after.c = cpu.c;
	    after.d = cpu.d;
	    after.e = cpu.e;
	    after.f = cpu.f.as_u8();
	    after.h = cpu.h;
	    after.l = cpu.l;
	    after.i = cpu.i;
	    after.r = cpu.r;
	    after.ix = cpu.ix;
	    after.iy = cpu.iy;
	    after.af_ = ((cpu.shadow[6] as u16) << 8) | (cpu.shadow[7] as u16);
	    after.bc_ = ((cpu.shadow[0] as u16) << 8) | (cpu.shadow[1] as u16);
	    after.de_ = ((cpu.shadow[2] as u16) << 8) | (cpu.shadow[3] as u16);
	    after.hl_ = ((cpu.shadow[4] as u16) << 8) | (cpu.shadow[5] as u16);
	    after.im = cpu.im;
	    after.iff1 = cpu.iff as u8;
	    after.iff2 = cpu.iff2 as u8;
	    after.q = cpu.q;
	    after.wz = cpu.wz;
	    for r in &test.r#final.ram {
		after.ram.push((r.0, cpu.bus.read_byte(r.0)));
	    }
	    for p in &test.r#final.ports {
		after.ports.push((p.0, cpu.bus.read_io_byte(p.0), 'r'));
	    }

	    if after != test.r#final {
		if !test.name.starts_with("ED A2") && !test.name.starts_with("ED B2") &&
		    !test.name.starts_with("ED AA") && !test.name.starts_with("ED BA") &&
		    !test.name.starts_with("ED 40") && !test.name.starts_with("ED 50") &&
		    !test.name.starts_with("ED 60") && !test.name.starts_with("ED 70") &&
		    !test.name.starts_with("ED 48") && !test.name.starts_with("ED 58") &&
		    !test.name.starts_with("ED 68") && !test.name.starts_with("ED 78") &&
		    !test.name.starts_with("ED B0") && !test.name.starts_with("ED B8") &&
		    !test.name.starts_with("ED B1") && !test.name.starts_with("ED B9") &&
		    !test.name.starts_with("ED B2") && !test.name.starts_with("ED BA") &&
		    !test.name.starts_with("ED B3") && !test.name.starts_with("ED BB") {
		    println!("{:x?}", after);
		    println!("{:x?}", test.r#final);
		    panic!("failed {}", test.name);
		}
	    }
	}
    }
    println!("All tests passed");*/
    let path = env::args().nth(1).expect("Usage: vpspec <path>");
    let buf: Vec<u8> = std::fs::read(path).unwrap();
    let stub_buf: Vec<u8> = std::fs::read("cpmstub.bin").unwrap();
    cpu.bus.load_bin(0xdc00, &stub_buf);
    cpu.bus.write_byte(5, 0xc3);
    cpu.bus.write_word(6, 0xdc00); //jmp $dc00
    cpu.bus.load_bin(0x100, &buf);
    cpu.reset();
    cpu.pc = 0x100;
    
    'running: loop {
	let cyc = cpu.step();
	if cyc == 0 || cpu.pc == 0 {
	    println!("died at {:04x} cyc = {cyc}", cpu.pc);
	    break 'running;
	}
	cpu.bus.step(cyc);

	//let _ = stdin.read(&mut [0u8]).unwrap();
    }
}
