mod z80;
mod bus;

use crate::bus::Bus;
use std::env;
use std::thread;
use std::time;
use std::io;
use std::io::prelude::*;
use sdl2::pixels::PixelFormatEnum;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::mixer::{InitFlag, AUDIO_S16LSB, DEFAULT_CHANNELS};

fn main() {
    let mut cpu = z80::Cpu::new(false);
    let mut stdin = io::stdin();

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
	    break 'running;
	}
	cpu.bus.step(cyc);

	//let _ = stdin.read(&mut [0u8]).unwrap();
    }
}
