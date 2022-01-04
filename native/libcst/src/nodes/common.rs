use std::rc::Rc;

use crate::tokenizer::Token;

pub(crate) type TokenRef<'r, 'a> = &'r Rc<Token<'a>>;
