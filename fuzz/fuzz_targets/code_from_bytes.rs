#![no_main]
use libfuzzer_sys::fuzz_target;

use cea608_types::tables::Code;

use once_cell::sync::Lazy;

#[macro_use]
extern crate log;

pub fn debug_init() {
    static TRACING: Lazy<()> = Lazy::new(|| {
        env_logger::init();
    });

    Lazy::force(&TRACING);
}

fuzz_target!(|data: [u8; 2]| {
    debug_init();
    if let Ok(code) = Code::from_data(data) {
        for (i, c) in code.iter().enumerate() {
            info!("{i} parsed {c:?}");
            let mut written = vec![];
            let _ = c.write(&mut written);
            assert_eq!(data[i], written[0]);
            if matches!(c, Code::Control(_)) {
                assert_eq!(data[1], written[1]);
                break;
            }
        }
    }
});
