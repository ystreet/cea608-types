// Copyright (C) 2024 Matthew Waters <matthew@centricular.com>
//
// Licensed under the MIT license <LICENSE-MIT> or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use cea608_types::*;

use std::{env, io::Read};

use std::sync::OnceLock;

static TRACING: OnceLock<()> = OnceLock::new();

#[macro_use]
extern crate log;

pub fn debug_init() {
    TRACING.get_or_init(|| {
        env_logger::init();
    });
}

fn main() -> std::process::ExitCode {
    debug_init();
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("608-dump filename");
        return std::process::ExitCode::from(1);
    }

    let file = std::fs::File::open(args[1].clone()).unwrap();
    let mut buf_reader = std::io::BufReader::new(file);

    loop {
        let mut data = [0; 2];
        let Ok(_) = buf_reader.read_exact(&mut data) else {
            break;
        };
        if data != [0x80, 0x80] {
            debug!("{data:#x?}");
            let codes = tables::Code::from_data(data);
            println!("{codes:?}");
        }
    }

    std::process::ExitCode::SUCCESS
}
