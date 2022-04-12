use std::error::Error;
use std::fs::File;
use utf8_read::{Reader};

use crate::errors::ReadError;

pub trait Source {
    fn get_next_char(&mut self) -> Result<char, ReadError>;
    fn current_position(&self) -> u64;
}

pub struct FileSource {
    current_pos: u64,
    reader: utf8_read::Reader<File>,
    current_char: char,
}

impl Source for FileSource {
    //!FIXME: this is bad xD
    fn get_next_char(&mut self) -> Result<char, ReadError> {
       match self.reader.next_char() {
            Ok(ch) => {
                match ch {
                    utf8_read::Char::Eof => Ok(3 as char),
                    utf8_read::Char::NoData => Ok(3 as char),
                    utf8_read::Char::Char(read_char) => {
                        self.current_pos += 1;
                        self.current_char = read_char;
                        Ok(read_char)},
                }
            }
            Err(e) => Err(ReadError::ReadingError(Box::new(e))),
       }
    }

    fn current_position(&self) -> u64 {
        self.current_pos
    }
}


impl FileSource {
    pub fn new(filename: String) -> Result<FileSource, Box<dyn Error>> {
        let f = File::open(filename);
        match f {
            Err(e) => Err(Box::new(e)),
            Ok(file) => Ok(FileSource{
                current_pos: 0,
                reader: Reader::new(file),
                current_char: '\0',
            })
        }
    }

}

pub struct TestSource {
    text: String,
    pos: usize
}

impl Source for TestSource {
    //!FIXME: this is bad xD
    fn get_next_char(&mut self) -> Result<char, ReadError> {
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
