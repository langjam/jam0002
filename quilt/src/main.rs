use clap::Parser as ClapParser;

use std::io;

/// Run a quilt program
#[derive(ClapParser)]
#[clap(about, version, author)]
pub struct Args {
    /// A quilt program
    file: String,

    /// Pixel size
    #[clap(short, long, default_value_t = 1)]
    pixel_size: u8,
}

fn main() {
    let args = Args::parse();
    quilt_lang::run(&args.file, args.pixel_size as u32, io::stdout());
}
