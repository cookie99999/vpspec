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

const pal_dim: [u8; 24] = [0x00, 0x00, 0x00,
		 0x00, 0x00, 0xd8,
		 0xd8, 0x00, 0x00,
		 0xd8, 0x00, 0xd8,
		 0x00, 0xd8, 0x00,
		 0x00, 0xd8, 0xd8,
		 0xd8, 0xd8, 0x00,
		 0xd8, 0xd8, 0xd8,];

const pal_bri: [u8; 24] = [0x00, 0x00, 0x00,
		 0x00, 0x00, 0xff,
		 0xff, 0x00, 0x00,
		 0xff, 0x00, 0xff,
		 0x00, 0xff, 0x00,
		 0x00, 0xff, 0xff,
		 0xff, 0xff, 0x00,
		 0xff, 0xff, 0xff,];

fn draw_screen(bus: &mut bus::ZXBus, tex: &mut sdl2::render::Texture) {
    tex.with_lock(None, |buf: &mut [u8], pitch: usize| {
	for x in 0..256 {
	    for y in 0..192 {
		let offset = y as usize * pitch + x as usize * 3;
		let px_addr_hi = 0b01000000 | (((y as u8) >> 3) & 0x18)
		    | ((y as u8) & 7);
		let px_addr_lo = (((y as u8) << 2) & 0xe0) | (((x as u8) >> 3) & 0x1f);
		let px_addr = ((px_addr_hi as u16) << 8) | (px_addr_lo as u16);
		let byte = bus.read_byte(px_addr);
		let px = (byte >> (((x % 8) as i8) - 7).abs()) & 1;
		let attr_addr = (y / 8) * 32 + (x / 8);
		let attr = bus.read_byte(0x5800 + attr_addr as u16);
		let bri = (attr & 0x40) != 0;
		let paper = (attr >> 3) & 7;
		let ink = attr & 7;
		let col = if px != 0 {
		    ink as usize
		} else {
		    paper as usize
		};
		let col = col * 3;
		let pal = match bri {
		    true => pal_bri,
		    _ => pal_dim,
		};
		buf[offset] = pal[col];
		buf[offset + 1] = pal[col + 1];
		buf[offset + 2] = pal[col + 2];
	    }
	}
    }).unwrap();
}

fn main() {
    let mut cpu = z80::Cpu::new(false);
    let mut stdin = io::stdin();

    let context = sdl2::init().unwrap();
    let video = context.video().unwrap();
    let width = 256 * 3;
    let height = 192 * 3;
    let win = video.window("vpspec", width, height)
	.position_centered()
	.opengl()
	.build()
	.unwrap();
    let mut canvas = win.into_canvas().build().unwrap();
    let tex_create = canvas.texture_creator();
    let mut tex = tex_create
	.create_texture_streaming(PixelFormatEnum::RGB24, 256, 192)
	.unwrap();
    let mut event_pump = context.event_pump().unwrap();

    canvas.clear();
    canvas.present();

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
    //let path = env::args().nth(1).expect("Usage: vpspec <path>");
    let buf: Vec<u8> = std::fs::read("48k.rom").unwrap();
    //let stub_buf: Vec<u8> = std::fs::read("cpmstub.bin").unwrap();
    //cpu.bus.load_bin(0xdc00, &stub_buf);
    //cpu.bus.write_byte(5, 0xc3);
    //cpu.bus.write_word(6, 0xdc00); //jmp $dc00
    cpu.bus.load_bin(0, &buf);
    cpu.reset();
    //cpu.pc = 0x100;

    let mut acc: u128 = 0;
    'running: loop {
	let now = time::Instant::now();
	for e in event_pump.poll_iter() {
	    match e {
		Event::Quit {..} |
		Event::KeyDown { keycode: Some(Keycode::Escape), ..} => {
		    break 'running;
		},
		Event::KeyDown { keycode: Some(Keycode::Return), ..} => {
		    cpu.bus.keydown(0);
		},
		Event::KeyUp { keycode: Some(Keycode::Return), ..} => {
		    cpu.bus.keyup(0);
		},
		Event::KeyDown { keycode: Some(Keycode::LShift), ..} => {
		    cpu.bus.keydown(1);
		},
		Event::KeyUp { keycode: Some(Keycode::LShift), ..} => {
		    cpu.bus.keyup(1);
		},
		_ => {},
	    }
	}
	
	let cyc = cpu.step();
	if cyc == 0 {
	    println!("died at {:04x} cyc = {cyc}", cpu.pc);
	    break 'running;
	}
	cpu.bus.step(cyc);

	let elapsed = now.elapsed();
	acc += elapsed.as_micros();
	if cpu.bus.vblank {
	    //acc -= 20_000;
	    draw_screen(&mut cpu.bus, &mut tex);
	    canvas.copy(&tex, None, None).unwrap();
	    canvas.present();
	    cpu.bus.vblank = false;
	}
	//let _ = stdin.read(&mut [0u8]).unwrap();

    }
}
