use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::{Error, Result};
use std::path::Path;

pub fn lex(filename: &String) -> Result<()> {
    let f = File::open(filename)?;
    let mut file = BufReader::new(&f);
    for line in file.lines() {
        let l = line.unwrap();
        println!("{}", l);
    }
    Ok(())
}
