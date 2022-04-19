use std::{fs::File, io::Error};
use utf8_read::{Reader, StreamPosition};

use crate::errors::ErrorKind;

pub trait Source {
    fn get_next_char(&mut self) -> Result<char, ErrorKind>;
    fn current_position(&self) -> u64;
}

pub struct FileSource {
    current_pos: StreamPosition,
    reader: utf8_read::Reader<File>,
    current_char: char,
}

impl Source for FileSource {
    fn get_next_char(&mut self) -> Result<char, ErrorKind> {
       match self.reader.next_char() {
            Ok(ch) => {
                match ch {
                    utf8_read::Char::Eof => Ok(3 as char),
                    utf8_read::Char::NoData => Ok(3 as char),
                    utf8_read::Char::Char(read_char) => {
                        self.current_char = read_char;
                        self.current_pos = *self.reader.borrow_pos();
                        Ok(read_char)},
                }
            }
            Err(e) => match e {
                utf8_read::Error::IoError(e) => Err(ErrorKind::IoError(e.to_string())), 
                utf8_read::Error::MalformedUtf8(p, c) => Err(ErrorKind::MalformedUtf8 { position: p.byte(), bad_utf: c }),
            },
       }
    }

    fn current_position(&self) -> u64 {
        self.current_pos.byte() as u64
    }
}


impl FileSource {
    pub fn new(filename: String) -> Result<FileSource, Error> {
        let f = File::open(filename);
        match f {
            Err(e) => Err(e),
            Ok(file) => {
            let reader = Reader::new(file);
            let pos = *reader.borrow_pos();    
            Ok(FileSource{
                reader: reader,
                current_pos: pos,
                current_char: '\0',
            }) }
        }
    }

}

pub struct TestSource {
    text: String,
    pos: usize
}

impl Source for TestSource {
    fn get_next_char(&mut self) -> Result<char, ErrorKind> {
        match self.text.chars().nth(self.pos) {
            Some(c) => {
                self.pos += c.len_utf8();
                Ok(c)
            },
            _ => Ok(3 as char)
        }
    }

    fn current_position(&self) -> u64 {
        self.pos as u64
    }
}

impl TestSource {
    pub fn new(text: String, pos: usize) -> TestSource {
        TestSource{
            text,
            pos
        }
    }
}

#[cfg(test)]
mod test {
    use std::{fs::{self}};

    use super::{FileSource, Source};

    #[test]
    fn basic_read_file() {
        //Will unwrap cuz it's test
        let data = ['a','a','a','b','b','b'];
        fs::write("test.txt", "aaabbb").expect("unable to write");
        let mut fs = FileSource::new(String::from("test.txt")).unwrap();
        let mut i = 0;
        loop {
            match fs.get_next_char() {
                Err(_) => panic!(),
                Ok(ch) => {
                    if ch.is_control() {
                        break;
                    }
                    assert_eq!(ch, data[i]);
                    i += 1;
                }
            }
        }
        fs::remove_file("test.txt").unwrap();
    }
}
