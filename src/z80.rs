extern crate bitflags;

use crate::bus;
use crate::bus::Bus;

#[derive(Debug)]
struct Instruction {
    opcode: u8,
    //opcode2: u8,
    bytes: u8,
    cycles: u8,
}

macro_rules! instr_set {
    ($({ $o: expr, $b: expr, $c: expr }),* $(,)?) => {
	[
	    $(Instruction { opcode: $o, bytes: $b, cycles: $c }),*
	]
    };
}

const INSTR_SET: [Instruction; 256] = instr_set![
    {0x00, 1, 4}, {0x01, 3, 10}, {0x02, 1, 7}, {0x03, 1, 6}, {0x04, 1, 4}, {0x05, 1, 4}, {0x06, 2, 7}, {0x07, 1, 4},
    {0x08, 1, 4}, {0x09, 1, 11}, {0x0a, 1, 7}, {0x0b, 1, 6}, {0x0c, 1, 4}, {0x0d, 1, 4}, {0x0e, 2, 7}, {0x0f, 1, 4},
    {0x10, 2, 8}, {0x11, 3, 10}, {0x12, 1, 7}, {0x13, 1, 6}, {0x14, 1, 4}, {0x15, 1, 4}, {0x16, 2, 7}, {0x17, 1, 4},
    {0x18, 2, 12}, {0x19, 1, 11}, {0x1a, 1, 7}, {0x1b, 1, 6}, {0x1c, 1, 4}, {0x1d, 1, 4}, {0x1e, 2, 7}, {0x1f, 1, 4},
    {0x20, 2, 7}, {0x21, 3, 10}, {0x22, 3, 16}, {0x23, 1, 6}, {0x24, 1, 4}, {0x25, 1, 4}, {0x26, 2, 7}, {0x27, 1, 4},
    {0x28, 2, 7}, {0x29, 1, 11}, {0x2a, 3, 16}, {0x2b, 1, 6}, {0x2c, 1, 4}, {0x2d, 1, 4}, {0x2e, 2, 7}, {0x2f, 1, 4},
    {0x30, 2, 7}, {0x31, 3, 10}, {0x32, 3, 13}, {0x33, 1, 6}, {0x34, 1, 11}, {0x35, 1, 11}, {0x36, 2, 10}, {0x37, 1, 4},
    {0x38, 2, 7}, {0x39, 1, 11}, {0x3a, 3, 13}, {0x3b, 1, 6}, {0x3c, 1, 4}, {0x3d, 1, 4}, {0x3e, 2, 7}, {0x3f, 1, 4},
    {0x40, 1, 4}, {0x41, 1, 4}, {0x42, 1, 4}, {0x43, 1, 4}, {0x44, 1, 4}, {0x45, 1, 4}, {0x46, 1, 7}, {0x47, 1, 4},
    {0x48, 1, 4}, {0x49, 1, 4}, {0x4a, 1, 4}, {0x4b, 1, 4}, {0x4c, 1, 4}, {0x4d, 1, 4}, {0x4e, 1, 7}, {0x4f, 1, 4},
    {0x50, 1, 4}, {0x51, 1, 4}, {0x52, 1, 4}, {0x53, 1, 4}, {0x54, 1, 4}, {0x55, 1, 4}, {0x56, 1, 7}, {0x57, 1, 4},
    {0x58, 1, 4}, {0x59, 1, 4}, {0x5a, 1, 4}, {0x5b, 1, 4}, {0x5c, 1, 4}, {0x5d, 1, 4}, {0x5e, 1, 7}, {0x5f, 1, 4},
    {0x60, 1, 4}, {0x61, 1, 4}, {0x62, 1, 4}, {0x63, 1, 4}, {0x64, 1, 4}, {0x65, 1, 4}, {0x66, 1, 7}, {0x67, 1, 4},
    {0x68, 1, 4}, {0x69, 1, 4}, {0x6a, 1, 4}, {0x6b, 1, 4}, {0x6c, 1, 4}, {0x6d, 1, 4}, {0x6e, 1, 7}, {0x6f, 1, 4},
    {0x70, 1, 7}, {0x71, 1, 7}, {0x72, 1, 7}, {0x73, 1, 7}, {0x74, 1, 7}, {0x75, 1, 7}, {0x76, 1, 4}, {0x77, 1, 7},
    {0x78, 1, 4}, {0x79, 1, 4}, {0x7a, 1, 4}, {0x7b, 1, 4}, {0x7c, 1, 4}, {0x7d, 1, 4}, {0x7e, 1, 7}, {0x7f, 1, 4},
    {0x80, 1, 4}, {0x81, 1, 4}, {0x82, 1, 4}, {0x83, 1, 4}, {0x84, 1, 4}, {0x85, 1, 4}, {0x86, 1, 7}, {0x87, 1, 4},
    {0x88, 1, 4}, {0x89, 1, 4}, {0x8a, 1, 4}, {0x8b, 1, 4}, {0x8c, 1, 4}, {0x8d, 1, 4}, {0x8e, 1, 7}, {0x8f, 1, 4},
    {0x90, 1, 4}, {0x91, 1, 4}, {0x92, 1, 4}, {0x93, 1, 4}, {0x94, 1, 4}, {0x95, 1, 4}, {0x96, 1, 7}, {0x97, 1, 4},
    {0x98, 1, 4}, {0x99, 1, 4}, {0x9a, 1, 4}, {0x9b, 1, 4}, {0x9c, 1, 4}, {0x9d, 1, 4}, {0x9e, 1, 7}, {0x9f, 1, 4},
    {0xa0, 1, 4}, {0xa1, 1, 4}, {0xa2, 1, 4}, {0xa3, 1, 4}, {0xa4, 1, 4}, {0xa5, 1, 4}, {0xa6, 1, 7}, {0xa7, 1, 4},
    {0xa8, 1, 4}, {0xa9, 1, 4}, {0xaa, 1, 4}, {0xab, 1, 4}, {0xac, 1, 4}, {0xad, 1, 4}, {0xae, 1, 7}, {0xaf, 1, 4},
    {0xb0, 1, 4}, {0xb1, 1, 4}, {0xb2, 1, 4}, {0xb3, 1, 4}, {0xb4, 1, 4}, {0xb5, 1, 4}, {0xb6, 1, 7}, {0xb7, 1, 4},
    {0xb8, 1, 4}, {0xb9, 1, 4}, {0xba, 1, 4}, {0xbb, 1, 4}, {0xbc, 1, 4}, {0xbd, 1, 4}, {0xbe, 1, 7}, {0xbf, 1, 4},
    {0xc0, 1, 5}, {0xc1, 1, 10}, {0xc2, 3, 10}, {0xc3, 3, 10}, {0xc4, 3, 10}, {0xc5, 1, 11}, {0xc6, 2, 7}, {0xc7, 1, 11},
    {0xc8, 1, 5}, {0xc9, 1, 10}, {0xca, 3, 10}, {0xcb, 0, 0}, {0xcc, 3, 10}, {0xcd, 3, 17}, {0xce, 2, 7}, {0xcf, 1, 11},
    {0xd0, 1, 5}, {0xd1, 1, 10}, {0xd2, 3, 10}, {0xd3, 2, 11}, {0xd4, 3, 10}, {0xd5, 1, 11}, {0xd6, 2, 7}, {0xd7, 1, 11},
    {0xd8, 1, 5}, {0xd9, 1, 4}, {0xda, 3, 10}, {0xdb, 2, 11}, {0xdc, 3, 10}, {0xdd, 0, 0}, {0xde, 2, 7}, {0xdf, 1, 11},
    {0xe0, 1, 5}, {0xe1, 1, 10}, {0xe2, 3, 10}, {0xe3, 1, 19}, {0xe4, 3, 10}, {0xe5, 1, 11}, {0xe6, 2, 7}, {0xe7, 1, 11},
    {0xe8, 1, 5}, {0xe9, 1, 4}, {0xea, 3, 10}, {0xeb, 1, 4}, {0xec, 3, 10}, {0xed, 0, 0}, {0xee, 2, 7}, {0xef, 1, 11},
    {0xf0, 1, 5}, {0xf1, 1, 10}, {0xf2, 3, 10}, {0xf3, 1, 4}, {0xf4, 3, 10}, {0xf5, 1, 11}, {0xf6, 2, 7}, {0xf7, 1, 11},
    {0xf8, 1, 5}, {0xf9, 1, 6}, {0xfa, 3, 10}, {0xfb, 1, 4}, {0xfc, 3, 10}, {0xfd, 0, 0}, {0xfe, 2, 7}, {0xff, 1, 11},
];

const CB_SET: [Instruction; 256] = instr_set![
    {0x00, 2, 8}, {0x01, 2, 8}, {0x02, 2, 8}, {0x03, 2, 8}, {0x04, 2, 8}, {0x05, 2, 8}, {0x06, 2, 15}, {0x07, 2, 8},
    {0x08, 2, 8}, {0x09, 2, 8}, {0x0a, 2, 8}, {0x0b, 2, 8}, {0x0c, 2, 8}, {0x0d, 2, 8}, {0x0e, 2, 15}, {0x0f, 2, 8},
    {0x10, 2, 8}, {0x11, 2, 8}, {0x12, 2, 8}, {0x13, 2, 8}, {0x14, 2, 8}, {0x15, 2, 8}, {0x16, 2, 15}, {0x17, 2, 8},
    {0x18, 2, 8}, {0x19, 2, 8}, {0x1a, 2, 8}, {0x1b, 2, 8}, {0x1c, 2, 8}, {0x1d, 2, 8}, {0x1e, 2, 15}, {0x1f, 2, 8},
    {0x20, 2, 8}, {0x21, 2, 8}, {0x22, 2, 8}, {0x23, 2, 8}, {0x24, 2, 8}, {0x25, 2, 8}, {0x26, 2, 15}, {0x27, 2, 8},
    {0x28, 2, 8}, {0x29, 2, 8}, {0x2a, 2, 8}, {0x2b, 2, 8}, {0x2c, 2, 8}, {0x2d, 2, 8}, {0x2e, 2, 15}, {0x2f, 2, 8},
    {0x30, 2, 8}, {0x31, 2, 8}, {0x32, 2, 8}, {0x33, 2, 8}, {0x34, 2, 8}, {0x35, 2, 8}, {0x36, 2, 15}, {0x37, 2, 8},
    {0x38, 2, 8}, {0x39, 2, 8}, {0x3a, 2, 8}, {0x3b, 2, 8}, {0x3c, 2, 8}, {0x3d, 2, 8}, {0x3e, 2, 15}, {0x3f, 2, 8},
    {0x40, 2, 8}, {0x41, 2, 8}, {0x42, 2, 8}, {0x43, 2, 8}, {0x44, 2, 8}, {0x45, 2, 8}, {0x46, 2, 12}, {0x47, 2, 8},
    {0x48, 2, 8}, {0x49, 2, 8}, {0x4a, 2, 8}, {0x4b, 2, 8}, {0x4c, 2, 8}, {0x4d, 2, 8}, {0x4e, 2, 12}, {0x4f, 2, 8},
    {0x50, 2, 8}, {0x51, 2, 8}, {0x52, 2, 8}, {0x53, 2, 8}, {0x54, 2, 8}, {0x55, 2, 8}, {0x56, 2, 12}, {0x57, 2, 8},
    {0x58, 2, 8}, {0x59, 2, 8}, {0x5a, 2, 8}, {0x5b, 2, 8}, {0x5c, 2, 8}, {0x5d, 2, 8}, {0x5e, 2, 12}, {0x5f, 2, 8},
    {0x60, 2, 8}, {0x61, 2, 8}, {0x62, 2, 8}, {0x63, 2, 8}, {0x64, 2, 8}, {0x65, 2, 8}, {0x66, 2, 12}, {0x67, 2, 8},
    {0x68, 2, 8}, {0x69, 2, 8}, {0x6a, 2, 8}, {0x6b, 2, 8}, {0x6c, 2, 8}, {0x6d, 2, 8}, {0x6e, 2, 12}, {0x6f, 2, 8},
    {0x70, 2, 8}, {0x71, 2, 8}, {0x72, 2, 8}, {0x73, 2, 8}, {0x74, 2, 8}, {0x75, 2, 8}, {0x76, 2, 12}, {0x77, 2, 8},
    {0x78, 2, 8}, {0x79, 2, 8}, {0x7a, 2, 8}, {0x7b, 2, 8}, {0x7c, 2, 8}, {0x7d, 2, 8}, {0x7e, 2, 12}, {0x7f, 2, 8},
    {0x80, 2, 8}, {0x81, 2, 8}, {0x82, 2, 8}, {0x83, 2, 8}, {0x84, 2, 8}, {0x85, 2, 8}, {0x86, 2, 15}, {0x87, 2, 8},
    {0x88, 2, 8}, {0x89, 2, 8}, {0x8a, 2, 8}, {0x8b, 2, 8}, {0x8c, 2, 8}, {0x8d, 2, 8}, {0x8e, 2, 15}, {0x8f, 2, 8},
    {0x90, 2, 8}, {0x91, 2, 8}, {0x92, 2, 8}, {0x93, 2, 8}, {0x94, 2, 8}, {0x95, 2, 8}, {0x96, 2, 15}, {0x97, 2, 8},
    {0x98, 2, 8}, {0x99, 2, 8}, {0x9a, 2, 8}, {0x9b, 2, 8}, {0x9c, 2, 8}, {0x9d, 2, 8}, {0x9e, 2, 15}, {0x9f, 2, 8},
    {0xa0, 2, 8}, {0xa1, 2, 8}, {0xa2, 2, 8}, {0xa3, 2, 8}, {0xa4, 2, 8}, {0xa5, 2, 8}, {0xa6, 2, 15}, {0xa7, 2, 8},
    {0xa8, 2, 8}, {0xa9, 2, 8}, {0xaa, 2, 8}, {0xab, 2, 8}, {0xac, 2, 8}, {0xad, 2, 8}, {0xae, 2, 15}, {0xaf, 2, 8},
    {0xb0, 2, 8}, {0xb1, 2, 8}, {0xb2, 2, 8}, {0xb3, 2, 8}, {0xb4, 2, 8}, {0xb5, 2, 8}, {0xb6, 2, 15}, {0xb7, 2, 8},
    {0xb8, 2, 8}, {0xb9, 2, 8}, {0xba, 2, 8}, {0xbb, 2, 8}, {0xbc, 2, 8}, {0xbd, 2, 8}, {0xbe, 2, 15}, {0xbf, 2, 8},
    {0xc0, 2, 8}, {0xc1, 2, 8}, {0xc2, 2, 8}, {0xc3, 2, 8}, {0xc4, 2, 8}, {0xc5, 2, 8}, {0xc6, 2, 15}, {0xc7, 2, 8},
    {0xc8, 2, 8}, {0xc9, 2, 8}, {0xca, 2, 8}, {0xcb, 2, 8}, {0xcc, 2, 8}, {0xcd, 2, 8}, {0xce, 2, 15}, {0xcf, 2, 8},
    {0xd0, 2, 8}, {0xd1, 2, 8}, {0xd2, 2, 8}, {0xd3, 2, 8}, {0xd4, 2, 8}, {0xd5, 2, 8}, {0xd6, 2, 15}, {0xd7, 2, 8},
    {0xd8, 2, 8}, {0xd9, 2, 8}, {0xda, 2, 8}, {0xdb, 2, 8}, {0xdc, 2, 8}, {0xdd, 2, 8}, {0xde, 2, 15}, {0xdf, 2, 8},
    {0xe0, 2, 8}, {0xe1, 2, 8}, {0xe2, 2, 8}, {0xe3, 2, 8}, {0xe4, 2, 8}, {0xe5, 2, 8}, {0xe6, 2, 15}, {0xe7, 2, 8},
    {0xe8, 2, 8}, {0xe9, 2, 8}, {0xea, 2, 8}, {0xeb, 2, 8}, {0xec, 2, 8}, {0xed, 2, 8}, {0xee, 2, 15}, {0xef, 2, 8},
    {0xf0, 2, 8}, {0xf1, 2, 8}, {0xf2, 2, 8}, {0xf3, 2, 8}, {0xf4, 2, 8}, {0xf5, 2, 8}, {0xf6, 2, 15}, {0xf7, 2, 8},
    {0xf8, 2, 8}, {0xf9, 2, 8}, {0xfa, 2, 8}, {0xfb, 2, 8}, {0xfc, 2, 8}, {0xfd, 2, 8}, {0xfe, 2, 15}, {0xff, 2, 8},
];

const DD_SET: [Instruction; 256] = instr_set![
    {0x00, 2, 8}, {0x01, 4, 14}, {0x02, 2, 11}, {0x03, 2, 10}, {0x04, 2, 8}, {0x05, 2, 8}, {0x06, 3, 11}, {0x07, 2, 8},
    {0x08, 2, 8}, {0x09, 2, 15}, {0x0a, 2, 11}, {0x0b, 2, 10}, {0x0c, 2, 8}, {0x0d, 2, 8}, {0x0e, 3, 11}, {0x0f, 2, 8},
    {0x10, 3, 12}, {0x11, 4, 14}, {0x12, 2, 11}, {0x13, 2, 10}, {0x14, 2, 8}, {0x15, 2, 8}, {0x16, 3, 11}, {0x17, 2, 8},
    {0x18, 3, 16}, {0x19, 2, 15}, {0x1a, 2, 11}, {0x1b, 2, 10}, {0x1c, 2, 8}, {0x1d, 2, 8}, {0x1e, 3, 11}, {0x1f, 2, 8},
    {0x20, 3, 11}, {0x21, 4, 14}, {0x22, 4, 20}, {0x23, 2, 10}, {0x24, 2, 8}, {0x25, 2, 8}, {0x26, 3, 11}, {0x27, 2, 8},
    {0x28, 3, 11}, {0x29, 2, 15}, {0x2a, 4, 20}, {0x2b, 2, 10}, {0x2c, 2, 8}, {0x2d, 2, 8}, {0x2e, 3, 11}, {0x2f, 2, 8},
    {0x30, 3, 11}, {0x31, 4, 14}, {0x32, 4, 17}, {0x33, 2, 10}, {0x34, 3, 23}, {0x35, 3, 23}, {0x36, 4, 19}, {0x37, 2, 8},
    {0x38, 3, 11}, {0x39, 2, 15}, {0x3a, 4, 17}, {0x3b, 2, 10}, {0x3c, 2, 8}, {0x3d, 2, 8}, {0x3e, 3, 11}, {0x3f, 2, 8},
    {0x40, 2, 8}, {0x41, 2, 8}, {0x42, 2, 8}, {0x43, 2, 8}, {0x44, 2, 8}, {0x45, 2, 8}, {0x46, 3, 19}, {0x47, 2, 8},
    {0x48, 2, 8}, {0x49, 2, 8}, {0x4a, 2, 8}, {0x4b, 2, 8}, {0x4c, 2, 8}, {0x4d, 2, 8}, {0x4e, 3, 19}, {0x4f, 2, 8},
    {0x50, 2, 8}, {0x51, 2, 8}, {0x52, 2, 8}, {0x53, 2, 8}, {0x54, 2, 8}, {0x55, 2, 8}, {0x56, 3, 19}, {0x57, 2, 8},
    {0x58, 2, 8}, {0x59, 2, 8}, {0x5a, 2, 8}, {0x5b, 2, 8}, {0x5c, 2, 8}, {0x5d, 2, 8}, {0x5e, 3, 19}, {0x5f, 2, 8},
    {0x60, 2, 8}, {0x61, 2, 8}, {0x62, 2, 8}, {0x63, 2, 8}, {0x64, 2, 8}, {0x65, 2, 8}, {0x66, 3, 19}, {0x67, 2, 8},
    {0x68, 2, 8}, {0x69, 2, 8}, {0x6a, 2, 8}, {0x6b, 2, 8}, {0x6c, 2, 8}, {0x6d, 2, 8}, {0x6e, 3, 19}, {0x6f, 2, 8},
    {0x70, 3, 19}, {0x71, 3, 19}, {0x72, 3, 19}, {0x73, 3, 19}, {0x74, 3, 19}, {0x75, 3, 19}, {0x76, 2, 8}, {0x77, 3, 19},
    {0x78, 2, 8}, {0x79, 2, 8}, {0x7a, 2, 8}, {0x7b, 2, 8}, {0x7c, 2, 8}, {0x7d, 2, 8}, {0x7e, 3, 19}, {0x7f, 2, 8},
    {0x80, 2, 8}, {0x81, 2, 8}, {0x82, 2, 8}, {0x83, 2, 8}, {0x84, 2, 8}, {0x85, 2, 8}, {0x86, 3, 19}, {0x87, 2, 8},
    {0x88, 2, 8}, {0x89, 2, 8}, {0x8a, 2, 8}, {0x8b, 2, 8}, {0x8c, 2, 8}, {0x8d, 2, 8}, {0x8e, 3, 19}, {0x8f, 2, 8},
    {0x90, 2, 8}, {0x91, 2, 8}, {0x92, 2, 8}, {0x93, 2, 8}, {0x94, 2, 8}, {0x95, 2, 8}, {0x96, 3, 19}, {0x97, 2, 8},
    {0x98, 2, 8}, {0x99, 2, 8}, {0x9a, 2, 8}, {0x9b, 2, 8}, {0x9c, 2, 8}, {0x9d, 2, 8}, {0x9e, 3, 19}, {0x9f, 2, 8},
    {0xa0, 2, 8}, {0xa1, 2, 8}, {0xa2, 2, 8}, {0xa3, 2, 8}, {0xa4, 2, 8}, {0xa5, 2, 8}, {0xa6, 3, 19}, {0xa7, 2, 8},
    {0xa8, 2, 8}, {0xa9, 2, 8}, {0xaa, 2, 8}, {0xab, 2, 8}, {0xac, 2, 8}, {0xad, 2, 8}, {0xae, 3, 19}, {0xaf, 2, 8},
    {0xb0, 2, 8}, {0xb1, 2, 8}, {0xb2, 2, 8}, {0xb3, 2, 8}, {0xb4, 2, 8}, {0xb5, 2, 8}, {0xb6, 3, 19}, {0xb7, 2, 8},
    {0xb8, 2, 8}, {0xb9, 2, 8}, {0xba, 2, 8}, {0xbb, 2, 8}, {0xbc, 2, 8}, {0xbd, 2, 8}, {0xbe, 3, 19}, {0xbf, 2, 8},
    {0xc0, 2, 9}, {0xc1, 2, 14}, {0xc2, 4, 14}, {0xc3, 4, 14}, {0xc4, 4, 14}, {0xc5, 2, 15}, {0xc6, 3, 11}, {0xc7, 2, 15},
    {0xc8, 2, 9}, {0xc9, 2, 14}, {0xca, 4, 14}, {0xcb, 0, 0}, {0xcc, 4, 14}, {0xcd, 4, 21}, {0xce, 3, 11}, {0xcf, 2, 15},
    {0xd0, 2, 9}, {0xd1, 2, 14}, {0xd2, 4, 14}, {0xd3, 3, 15}, {0xd4, 4, 14}, {0xd5, 2, 15}, {0xd6, 3, 11}, {0xd7, 2, 15},
    {0xd8, 2, 9}, {0xd9, 2, 8}, {0xda, 4, 14}, {0xdb, 3, 15}, {0xdc, 4, 14}, {0xdd, 0, 0}, {0xde, 3, 11}, {0xdf, 2, 15},
    {0xe0, 2, 9}, {0xe1, 2, 14}, {0xe2, 4, 14}, {0xe3, 2, 23}, {0xe4, 4, 14}, {0xe5, 2, 15}, {0xe6, 3, 11}, {0xe7, 2, 15},
    {0xe8, 2, 9}, {0xe9, 2, 8}, {0xea, 4, 14}, {0xeb, 2, 8}, {0xec, 4, 14}, {0xed, 0, 0}, {0xee, 3, 11}, {0xef, 2, 15},
    {0xf0, 2, 9}, {0xf1, 2, 14}, {0xf2, 4, 14}, {0xf3, 2, 8}, {0xf4, 4, 14}, {0xf5, 2, 15}, {0xf6, 3, 11}, {0xf7, 2, 15},
    {0xf8, 2, 9}, {0xf9, 2, 10}, {0xfa, 4, 14}, {0xfb, 2, 8}, {0xfc, 4, 14}, {0xfd, 0, 0}, {0xfe, 3, 11}, {0xff, 2, 15},
];

const ED_SET: [Instruction; 256] = instr_set![
    {0x00, 2, 8}, {0x01, 2, 8}, {0x02, 2, 8}, {0x03, 2, 8}, {0x04, 2, 8}, {0x05, 2, 8}, {0x06, 2, 8}, {0x07, 2, 8},
    {0x08, 2, 8}, {0x09, 2, 8}, {0x0a, 2, 8}, {0x0b, 2, 8}, {0x0c, 2, 8}, {0x0d, 2, 8}, {0x0e, 2, 8}, {0x0f, 2, 8},
    {0x10, 2, 8}, {0x11, 2, 8}, {0x12, 2, 8}, {0x13, 2, 8}, {0x14, 2, 8}, {0x15, 2, 8}, {0x16, 2, 8}, {0x17, 2, 8},
    {0x18, 2, 8}, {0x19, 2, 8}, {0x1a, 2, 8}, {0x1b, 2, 8}, {0x1c, 2, 8}, {0x1d, 2, 8}, {0x1e, 2, 8}, {0x1f, 2, 8},
    {0x20, 2, 8}, {0x21, 2, 8}, {0x22, 2, 8}, {0x23, 2, 8}, {0x24, 2, 8}, {0x25, 2, 8}, {0x26, 2, 8}, {0x27, 2, 8},
    {0x28, 2, 8}, {0x29, 2, 8}, {0x2a, 2, 8}, {0x2b, 2, 8}, {0x2c, 2, 8}, {0x2d, 2, 8}, {0x2e, 2, 8}, {0x2f, 2, 8},
    {0x30, 2, 8}, {0x31, 2, 8}, {0x32, 2, 8}, {0x33, 2, 8}, {0x34, 2, 8}, {0x35, 2, 8}, {0x36, 2, 8}, {0x37, 2, 8},
    {0x38, 2, 8}, {0x39, 2, 8}, {0x3a, 2, 8}, {0x3b, 2, 8}, {0x3c, 2, 8}, {0x3d, 2, 8}, {0x3e, 2, 8}, {0x3f, 2, 8},
    {0x40, 2, 12}, {0x41, 2, 12}, {0x42, 2, 15}, {0x43, 4, 20}, {0x44, 2, 8}, {0x45, 2, 14}, {0x46, 2, 8}, {0x47, 2, 9},
    {0x48, 2, 12}, {0x49, 2, 12}, {0x4a, 2, 15}, {0x4b, 4, 20}, {0x4c, 2, 8}, {0x4d, 2, 14}, {0x4e, 2, 8}, {0x4f, 2, 9},
    {0x50, 2, 12}, {0x51, 2, 12}, {0x52, 2, 15}, {0x53, 4, 20}, {0x54, 2, 8}, {0x55, 2, 14}, {0x56, 2, 8}, {0x57, 2, 9},
    {0x58, 2, 12}, {0x59, 2, 12}, {0x5a, 2, 15}, {0x5b, 4, 20}, {0x5c, 2, 8}, {0x5d, 2, 14}, {0x5e, 2, 8}, {0x5f, 2, 9},
    {0x60, 2, 12}, {0x61, 2, 12}, {0x62, 2, 15}, {0x63, 4, 20}, {0x64, 2, 8}, {0x65, 2, 14}, {0x66, 2, 8}, {0x67, 2, 18},
    {0x68, 2, 12}, {0x69, 2, 12}, {0x6a, 2, 15}, {0x6b, 4, 20}, {0x6c, 2, 8}, {0x6d, 2, 14}, {0x6e, 2, 8}, {0x6f, 2, 18},
    {0x70, 2, 12}, {0x71, 2, 12}, {0x72, 2, 15}, {0x73, 4, 20}, {0x74, 2, 8}, {0x75, 2, 14}, {0x76, 2, 8}, {0x77, 2, 8},
    {0x78, 2, 12}, {0x79, 2, 12}, {0x7a, 2, 12}, {0x7b, 4, 20}, {0x7c, 2, 8}, {0x7d, 2, 14}, {0x7e, 2, 8}, {0x7f, 2, 8},
    {0x80, 2, 8}, {0x81, 2, 8}, {0x82, 2, 8}, {0x83, 2, 8}, {0x84, 2, 8}, {0x85, 2, 8}, {0x86, 2, 8}, {0x87, 2, 8},
    {0x88, 2, 8}, {0x89, 2, 8}, {0x8a, 2, 8}, {0x8b, 2, 8}, {0x8c, 2, 8}, {0x8d, 2, 8}, {0x8e, 2, 8}, {0x8f, 2, 8},
    {0x90, 2, 8}, {0x91, 2, 8}, {0x92, 2, 8}, {0x93, 2, 8}, {0x94, 2, 8}, {0x95, 2, 8}, {0x96, 2, 8}, {0x97, 2, 8},
    {0x98, 2, 8}, {0x99, 2, 8}, {0x9a, 2, 8}, {0x9b, 2, 8}, {0x9c, 2, 8}, {0x9d, 2, 8}, {0x9e, 2, 8}, {0x9f, 2, 8},
    {0xa0, 2, 16}, {0xa1, 2, 16}, {0xa2, 2, 16}, {0xa3, 2, 16}, {0xa4, 2, 8}, {0xa5, 2, 8}, {0xa6, 2, 8}, {0xa7, 2, 8},
    {0xa8, 2, 16}, {0xa9, 2, 16}, {0xaa, 2, 16}, {0xab, 2, 16}, {0xac, 2, 8}, {0xad, 2, 8}, {0xae, 2, 8}, {0xaf, 2, 8},
    {0xb0, 2, 16}, {0xb1, 2, 16}, {0xb2, 2, 16}, {0xb3, 2, 16}, {0xb4, 2, 8}, {0xb5, 2, 8}, {0xb6, 2, 8}, {0xb7, 2, 8},
    {0xb8, 2, 16}, {0xb9, 2, 16}, {0xba, 2, 16}, {0xbb, 2, 16}, {0xbc, 2, 8}, {0xbd, 2, 8}, {0xbe, 2, 8}, {0xbf, 2, 8},
    {0xc0, 2, 8}, {0xc1, 2, 8}, {0xc2, 2, 8}, {0xc3, 2, 8}, {0xc4, 2, 8}, {0xc5, 2, 8}, {0xc6, 2, 8}, {0xc7, 2, 8},
    {0xc8, 2, 8}, {0xc9, 2, 8}, {0xca, 2, 8}, {0xcb, 2, 8}, {0xcc, 2, 8}, {0xcd, 2, 8}, {0xce, 2, 8}, {0xcf, 2, 8},
    {0xd0, 2, 8}, {0xd1, 2, 8}, {0xd2, 2, 8}, {0xd3, 2, 8}, {0xd4, 2, 8}, {0xd5, 2, 8}, {0xd6, 2, 8}, {0xd7, 2, 8},
    {0xd8, 2, 8}, {0xd9, 2, 8}, {0xda, 2, 8}, {0xdb, 2, 8}, {0xdc, 2, 8}, {0xdd, 2, 8}, {0xde, 2, 8}, {0xdf, 2, 8},
    {0xe0, 2, 8}, {0xe1, 2, 8}, {0xe2, 2, 8}, {0xe3, 2, 8}, {0xe4, 2, 8}, {0xe5, 2, 8}, {0xe6, 2, 8}, {0xe7, 2, 8},
    {0xe8, 2, 8}, {0xe9, 2, 8}, {0xea, 2, 8}, {0xeb, 2, 8}, {0xec, 2, 8}, {0xed, 2, 8}, {0xee, 2, 8}, {0xef, 2, 8},
    {0xf0, 2, 8}, {0xf1, 2, 8}, {0xf2, 2, 8}, {0xf3, 2, 8}, {0xf4, 2, 8}, {0xf5, 2, 8}, {0xf6, 2, 8}, {0xf7, 2, 8},
    {0xf8, 2, 8}, {0xf9, 2, 8}, {0xfa, 2, 8}, {0xfb, 2, 8}, {0xfc, 2, 8}, {0xfd, 2, 8}, {0xfe, 2, 8}, {0xff, 2, 8},
];

const FD_SET: [Instruction; 256] = instr_set![
    {0x00, 2, 8}, {0x01, 4, 14}, {0x02, 2, 11}, {0x03, 2, 10}, {0x04, 2, 8}, {0x05, 2, 8}, {0x06, 3, 11}, {0x07, 2, 8},
    {0x08, 2, 8}, {0x09, 2, 15}, {0x0a, 2, 11}, {0x0b, 2, 10}, {0x0c, 2, 8}, {0x0d, 2, 8}, {0x0e, 3, 11}, {0x0f, 2, 8},
    {0x10, 3, 12}, {0x11, 4, 14}, {0x12, 2, 11}, {0x13, 2, 10}, {0x14, 2, 8}, {0x15, 2, 8}, {0x16, 3, 11}, {0x17, 2, 8},
    {0x18, 3, 16}, {0x19, 2, 15}, {0x1a, 2, 11}, {0x1b, 2, 10}, {0x1c, 2, 8}, {0x1d, 2, 8}, {0x1e, 3, 11}, {0x1f, 2, 8},
    {0x20, 3, 11}, {0x21, 4, 14}, {0x22, 4, 20}, {0x23, 2, 10}, {0x24, 2, 8}, {0x25, 2, 8}, {0x26, 3, 11}, {0x27, 2, 8},
    {0x28, 3, 11}, {0x29, 2, 15}, {0x2a, 4, 20}, {0x2b, 2, 10}, {0x2c, 2, 8}, {0x2d, 2, 8}, {0x2e, 3, 11}, {0x2f, 2, 8},
    {0x30, 3, 11}, {0x31, 4, 14}, {0x32, 4, 17}, {0x33, 2, 10}, {0x34, 3, 23}, {0x35, 3, 23}, {0x36, 4, 19}, {0x37, 2, 8},
    {0x38, 3, 11}, {0x39, 2, 15}, {0x3a, 4, 17}, {0x3b, 2, 10}, {0x3c, 2, 8}, {0x3d, 2, 8}, {0x3e, 3, 11}, {0x3f, 2, 8},
    {0x40, 2, 8}, {0x41, 2, 8}, {0x42, 2, 8}, {0x43, 2, 8}, {0x44, 2, 8}, {0x45, 2, 8}, {0x46, 3, 19}, {0x47, 2, 8},
    {0x48, 2, 8}, {0x49, 2, 8}, {0x4a, 2, 8}, {0x4b, 2, 8}, {0x4c, 2, 8}, {0x4d, 2, 8}, {0x4e, 3, 19}, {0x4f, 2, 8},
    {0x50, 2, 8}, {0x51, 2, 8}, {0x52, 2, 8}, {0x53, 2, 8}, {0x54, 2, 8}, {0x55, 2, 8}, {0x56, 3, 19}, {0x57, 2, 8},
    {0x58, 2, 8}, {0x59, 2, 8}, {0x5a, 2, 8}, {0x5b, 2, 8}, {0x5c, 2, 8}, {0x5d, 2, 8}, {0x5e, 3, 19}, {0x5f, 2, 8},
    {0x60, 2, 8}, {0x61, 2, 8}, {0x62, 2, 8}, {0x63, 2, 8}, {0x64, 2, 8}, {0x65, 2, 8}, {0x66, 3, 19}, {0x67, 2, 8},
    {0x68, 2, 8}, {0x69, 2, 8}, {0x6a, 2, 8}, {0x6b, 2, 8}, {0x6c, 2, 8}, {0x6d, 2, 8}, {0x6e, 3, 19}, {0x6f, 2, 8},
    {0x70, 3, 19}, {0x71, 3, 19}, {0x72, 3, 19}, {0x73, 3, 19}, {0x74, 3, 19}, {0x75, 3, 19}, {0x76, 2, 8}, {0x77, 3, 19},
    {0x78, 2, 8}, {0x79, 2, 8}, {0x7a, 2, 8}, {0x7b, 2, 8}, {0x7c, 2, 8}, {0x7d, 2, 8}, {0x7e, 3, 19}, {0x7f, 2, 8},
    {0x80, 2, 8}, {0x81, 2, 8}, {0x82, 2, 8}, {0x83, 2, 8}, {0x84, 2, 8}, {0x85, 2, 8}, {0x86, 3, 19}, {0x87, 2, 8},
    {0x88, 2, 8}, {0x89, 2, 8}, {0x8a, 2, 8}, {0x8b, 2, 8}, {0x8c, 2, 8}, {0x8d, 2, 8}, {0x8e, 3, 19}, {0x8f, 2, 8},
    {0x90, 2, 8}, {0x91, 2, 8}, {0x92, 2, 8}, {0x93, 2, 8}, {0x94, 2, 8}, {0x95, 2, 8}, {0x96, 3, 19}, {0x97, 2, 8},
    {0x98, 2, 8}, {0x99, 2, 8}, {0x9a, 2, 8}, {0x9b, 2, 8}, {0x9c, 2, 8}, {0x9d, 2, 8}, {0x9e, 3, 19}, {0x9f, 2, 8},
    {0xa0, 2, 8}, {0xa1, 2, 8}, {0xa2, 2, 8}, {0xa3, 2, 8}, {0xa4, 2, 8}, {0xa5, 2, 8}, {0xa6, 3, 19}, {0xa7, 2, 8},
    {0xa8, 2, 8}, {0xa9, 2, 8}, {0xaa, 2, 8}, {0xab, 2, 8}, {0xac, 2, 8}, {0xad, 2, 8}, {0xae, 3, 19}, {0xaf, 2, 8},
    {0xb0, 2, 8}, {0xb1, 2, 8}, {0xb2, 2, 8}, {0xb3, 2, 8}, {0xb4, 2, 8}, {0xb5, 2, 8}, {0xb6, 3, 19}, {0xb7, 2, 8},
    {0xb8, 2, 8}, {0xb9, 2, 8}, {0xba, 2, 8}, {0xbb, 2, 8}, {0xbc, 2, 8}, {0xbd, 2, 8}, {0xbe, 3, 19}, {0xbf, 2, 8},
    {0xc0, 2, 9}, {0xc1, 2, 14}, {0xc2, 4, 14}, {0xc3, 4, 14}, {0xc4, 4, 14}, {0xc5, 2, 15}, {0xc6, 3, 11}, {0xc7, 2, 15},
    {0xc8, 2, 9}, {0xc9, 2, 14}, {0xca, 4, 14}, {0xcb, 0, 0}, {0xcc, 4, 14}, {0xcd, 4, 21}, {0xce, 3, 11}, {0xcf, 2, 15},
    {0xd0, 2, 9}, {0xd1, 2, 14}, {0xd2, 4, 14}, {0xd3, 3, 15}, {0xd4, 4, 14}, {0xd5, 2, 15}, {0xd6, 3, 11}, {0xd7, 2, 15},
    {0xd8, 2, 9}, {0xd9, 2, 8}, {0xda, 4, 14}, {0xdb, 3, 15}, {0xdc, 4, 14}, {0xdd, 0, 0}, {0xde, 3, 11}, {0xdf, 2, 15},
    {0xe0, 2, 9}, {0xe1, 2, 14}, {0xe2, 4, 14}, {0xe3, 2, 23}, {0xe4, 4, 14}, {0xe5, 2, 15}, {0xe6, 3, 11}, {0xe7, 2, 15},
    {0xe8, 2, 9}, {0xe9, 2, 8}, {0xea, 4, 14}, {0xeb, 2, 8}, {0xec, 4, 14}, {0xed, 0, 0}, {0xee, 3, 11}, {0xef, 2, 15},
    {0xf0, 2, 9}, {0xf1, 2, 14}, {0xf2, 4, 14}, {0xf3, 2, 8}, {0xf4, 4, 14}, {0xf5, 2, 15}, {0xf6, 3, 11}, {0xf7, 2, 15},
    {0xf8, 2, 9}, {0xf9, 2, 10}, {0xfa, 4, 14}, {0xfb, 2, 8}, {0xfc, 4, 14}, {0xfd, 0, 0}, {0xfe, 3, 11}, {0xff, 2, 15},
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
    im: u8,
    i: u8,
    r: u8,
    pub bus: bus::CpmBus,
    pub cycles: usize,
    ei_pend: bool,
    dbg: bool,
}

impl Cpu {
    pub fn new(dbg: bool) -> Self {
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
	    im: 0,
	    i: 0,
	    r: 0,
	    bus: bus::CpmBus::new(),
	    cycles: 0,
	    ei_pend: false,
	    dbg: dbg,
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
	self.bus.write_word(self.sp.wrapping_sub(2), data);
	self.sp = self.sp.wrapping_sub(2);
    }

    fn pop_word(&mut self) -> u16 {
	let tmp = self.bus.read_word(self.sp);
	self.sp = self.sp.wrapping_add(2);
	tmp
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
		    if c2 != 8 {
			self.cycles += 6;
		    }
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
	let op1 = self.bus.read_byte(self.pc.wrapping_add(1));
	let op2 = self.bus.read_byte(self.pc.wrapping_add(2));
	let opw = ((op2 as u16) << 8) | op1 as u16;
	
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

	let instr: &Instruction = match opcode {
	    0xcb => &CB_SET[op1 as usize],
	    0xdd => &DD_SET[op1 as usize],
	    0xed => &ED_SET[op1 as usize],
	    0xfd => &FD_SET[op1 as usize],
	    _ => &INSTR_SET[opcode as usize],
	};
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

	if self.dbg {
	    println!("A {:02X} F {:02X} B {:02X} C {:02X} D {:02X} E {:02X} H {:02X} L {:02X} SP {:04X}, CYC: {} iff {}",
		     self.a, self.f.as_u8(), self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.cycles, self.iff);
	    disas(self.pc, instr.opcode, op1, opw);
	}
	
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
		self.bus.write_word(opw, tmp);
	    },
	    0x2a => { //LD HL, (nn)
		let tmp = self.bus.read_word(opw);
		self.write_rp(2, tmp);
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
	    0xc0 | 0xc2..=0xc4 | 0xc7..=0xcd | 0xcf |
	    0xd0 | 0xd2 | 0xd4 | 0xd7 | 0xd8 | 0xda | 0xdc | 0xdf |
	    0xe0 | 0xe2 | 0xe4 | 0xe7..=0xea | 0xec | 0xef |
	    0xf0 | 0xf2 | 0xf4 | 0xf7 | 0xf8 | 0xfa | 0xfc | 0xff => { //branches
		self.branch((opcode & 6) >> 1, c, opcode & 1, opw);
	    },
	    0xd3 => { //OUT (n), A
		self.bus.write_io_byte(op1 as u16, self.a);
	    },
	    0xdb => { //IN A, (n)
		self.a = self.bus.read_io_byte(op1 as u16);
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
	    0xed => { //misc prefix
		let tmp = self.pc.wrapping_sub(instr.bytes as u16);
		opcode = self.bus.read_byte(tmp.wrapping_add(1));
		let oplo = self.bus.read_byte(tmp.wrapping_add(2));
		let ophi = self.bus.read_byte(tmp.wrapping_add(3));
		let opw = ((ophi as u16) << 8) | (oplo as u16);

		let d_bits = (opcode >> 3) & 7;
		let s = opcode & 7;
		let rp = (opcode >> 4) & 3;
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
		    0b110 => self.bus.read_byte(hlptr),
		    _ => self.a,
		};

		match opcode {
		    0x40 | 0x50 | 0x60 |
		    0x48 | 0x58 | 0x68 | 0x78 => { //16 bit IN
			let bctmp = self.read_rp(0);
			let tmp = self.bus.read_io_byte(bctmp);

			match d_bits {
			    0 => self.b = tmp,
			    1 => self.c = tmp,
			    2 => self.d = tmp,
			    3 => self.e = tmp,
			    4 => self.h = tmp,
			    5 => self.l = tmp,
			    6 => {},
			    _ => self.a = tmp,
			};
		    },
		    0x41 | 0x51 | 0x61 |
		    0x49 | 0x59 | 0x69 | 0x79 => { //16 bit OUT
			let bctmp = self.read_rp(0);

			if d_bits != 6 {
			    self.bus.write_io_byte(bctmp, d);
			} else {
			    self.bus.write_io_byte(bctmp, 0);
			}
		    },
		    0x42 | 0x52 | 0x62 | 0x72 => { //SBC HL, rp
			let tmp = self.read_rp(2) as u32;
			let cflag = self.f.contains(PSW::C) as u32;
			let tmp = tmp.wrapping_sub(self.read_rp(rp) as u32).wrapping_sub(cflag);
			//todo: flags
			self.write_rp(2, tmp as u16);
		    },
		    0x4a | 0x5a | 0x6a | 0x7a => { //ADC HL, rp
			let tmp = self.read_rp(2) as u32;
			let cflag = self.f.contains(PSW::C) as u32;
			let tmp = tmp.wrapping_add(self.read_rp(rp) as u32).wrapping_add(cflag);
			//todo: flags
			self.write_rp(2, tmp as u16);
		    },
		    0x43 | 0x53 | 0x63 | 0x73 => { //LD (nn), rp
			let tmp = self.read_rp(rp);
			self.bus.write_word(opw, tmp);
		    },
		    0x4b | 0x5b | 0x6b | 0x7b => { //LD rp, (nn)
			let tmp = self.bus.read_word(opw);
			self.write_rp(rp, tmp);
		    },
		    0x46 | 0x56 | 0x5e => { //IM y
			self.im = d;
		    },
		    0x44 => { //NEG
			self.a = self.a ^ 0x80;
		    },
		    0x47 => { //LD I, A
			self.i = self.a;
		    },
		    0x57 => { //LD A, I
			self.a = self.i;
		    },
		    0x4f => { //LD R, A
			self.r = self.a;
		    },
		    0x5f => { //LD A, R
			self.a = self.r;
		    },
		    0x45 => { //RETN
			self.iff = self.iff2;
			self.pc = self.pop_word();
		    },
		    0x4d => { //RETI
			self.pc = self.pop_word();
			//todo interrupt acknowledge stuff
		    },
		    0x6f => { //RLD
			let tmp = self.bus.read_byte(hlptr);
			let tmp2 = (tmp & 0xf0) >> 4;
			let tmp = ((tmp & 0xf0) >> 4) | ((tmp & 0x0f) << 4);
			let tmp3 = self.a & 0x0f;
			self.a = (self.a & 0xf0) | tmp2;
			let tmp = (tmp & 0xf0) | tmp3;
			self.bus.write_byte(hlptr, tmp);
		    },
		    0x67 => { //RRD
			let tmp = self.bus.read_byte(hlptr);
			let tmplo = tmp & 0x0f;
			let alo = (self.a & 0x0f) << 4;
			let tmphi = (tmp & 0xf0) >> 4;
			let tmp = (tmp & 0xf0) | tmphi;
			let tmp = (tmp & 0x0f) | alo;
			self.a = (self.a & 0xf0) | tmplo;
		    },
		    0xa0 => { //LDI
			let hl = self.read_rp(2);
			let de = self.read_rp(1);
			let bc = self.read_rp(0);
			let tmp = self.bus.read_byte(hl);
			self.bus.write_byte(de, tmp);
			self.write_rp(1, de.wrapping_add(1));
			self.write_rp(2, hl.wrapping_add(1));
			self.write_rp(0, bc.wrapping_sub(1));

			self.f.remove(PSW::H);
			self.f.remove(PSW::N);
			self.f.set(PSW::P, bc.wrapping_sub(1) != 0);
		    },
		    0xb0 => { //LDIR
			let hl = self.read_rp(2);
			let de = self.read_rp(1);
			let bc = self.read_rp(0);
			let tmp = self.bus.read_byte(hl);
			self.bus.write_byte(de, tmp);
			self.write_rp(1, de.wrapping_add(1));
			self.write_rp(2, hl.wrapping_add(1));
			self.write_rp(0, bc.wrapping_sub(1));
			
			self.f.remove(PSW::H);
			self.f.remove(PSW::N);
			self.f.set(PSW::P, bc.wrapping_sub(1) != 0);
			
			if bc.wrapping_sub(1) != 0 {
			    self.pc -= 2;
			}
		    },
		    0xa8 => { //LDD
			let hl = self.read_rp(2);
			let de = self.read_rp(1);
			let bc = self.read_rp(0);
			let tmp = self.bus.read_byte(hl);
			self.bus.write_byte(de, tmp);
			self.write_rp(1, de.wrapping_sub(1));
			self.write_rp(2, hl.wrapping_sub(1));
			self.write_rp(0, bc.wrapping_sub(1));

			self.f.remove(PSW::H);
			self.f.remove(PSW::N);
			self.f.set(PSW::P, bc.wrapping_sub(1) != 0);
		    },
		    0xb8 => { //LDDR
			let hl = self.read_rp(2);
			let de = self.read_rp(1);
			let bc = self.read_rp(0);
			let tmp = self.bus.read_byte(hl);
			self.bus.write_byte(de, tmp);
			self.write_rp(1, de.wrapping_sub(1));
			self.write_rp(2, hl.wrapping_sub(1));
			self.write_rp(0, bc.wrapping_sub(1));

			self.f.remove(PSW::H);
			self.f.remove(PSW::N);
			self.f.set(PSW::P, bc.wrapping_sub(1) != 0);

			if bc.wrapping_sub(1) != 0 {
			    self.pc -= 2;
			}
		    },
		    0xa1 => { //CPI
			let tmp = self.bus.read_byte(hlptr);
			let tmp = self.a.wrapping_sub(tmp);
			let bc = self.read_rp(0);
			self.write_rp(0, bc.wrapping_sub(1));

			self.f.set(PSW::S, (tmp & 0x80) != 0);
			self.f.set(PSW::Z, tmp == 0);
			//todo H
			self.f.set(PSW::P, bc.wrapping_sub(1) != 0);
			self.f.insert(PSW::N);
		    },
		    0xb1 => { //CPIR
			let tmp = self.bus.read_byte(hlptr);
			let tmp = self.a.wrapping_sub(tmp);
			let bc = self.read_rp(0);
			self.write_rp(0, bc.wrapping_sub(1));

			self.f.set(PSW::S, (tmp & 0x80) != 0);
			self.f.set(PSW::Z, tmp == 0);
			//todo H
			self.f.set(PSW::P, bc.wrapping_sub(1) != 0);
			self.f.insert(PSW::N);

			if bc.wrapping_sub(1) != 0 {
			    self.pc -= 2;
			}
		    },
		    _ => {
			todo!("unimplemented instruction ED {opcode:02X}");
		    },
		};
	    },
	    0xfd => { //iy prefix
		let tmp = self.pc.wrapping_sub(instr.bytes as u16);
		opcode = self.bus.read_byte(tmp.wrapping_add(1));
		let oplo = self.bus.read_byte(tmp.wrapping_add(2));
		let ophi = self.bus.read_byte(tmp.wrapping_add(3));
		let opw = ((ophi as u16) << 8) | (oplo as u16);
		
		let d_bits = (opcode >> 3) & 7;
		let s = opcode & 7;
		let rp = (opcode >> 4) & 3;
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
		    0b110 => self.bus.read_byte(hlptr),
		    _ => self.a,
		};

		match opcode {
		    0xe1 => { //POP IY
			self.iy = self.pop_word();
		    },
		    _ => {
			todo!("unimplemented opcode FD {opcode:02X}");
		    },
		};
	    },
	    0xdd => { //ix prefix
		let tmp = self.pc.wrapping_sub(instr.bytes as u16);
		opcode = self.bus.read_byte(tmp.wrapping_add(1));
		let oplo = self.bus.read_byte(tmp.wrapping_add(2));
		let ophi = self.bus.read_byte(tmp.wrapping_add(3));
		let opw = ((ophi as u16) << 8) | (oplo as u16);
		
		let d_bits = (opcode >> 3) & 7;
		let s = opcode & 7;
		let rp = (opcode >> 4) & 3;
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
		    0b110 => self.bus.read_byte(hlptr),
		    _ => self.a,
		};

		match opcode {
		    0xe5 => { //PUSH IX
			self.push_word(self.ix);
		    },
		    _ => {
			todo!("unimplemented opcode DD {opcode:02X}");
		    },
		};
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
