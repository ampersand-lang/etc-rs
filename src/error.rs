use std::fmt::{self, Display};

use failure::{Error, Fail};

#[derive(Debug)]
pub struct MultiError {
    errors: Vec<Error>,
}

impl Display for MultiError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for error in &self.errors {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl Fail for MultiError {}

impl From<Vec<Error>> for MultiError {
    fn from(errors: Vec<Error>) -> Self {
        Self { errors }
    }
}
