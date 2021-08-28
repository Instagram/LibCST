use std::mem::swap;

use crate::nodes::{
    ClassDef, CompoundStatement, Else, ExceptHandler, Finally, For, FunctionDef, If, IndentedBlock,
    Module, OrElse, SimpleStatementLine, Statement, Suite, Try, While, With, WithLeadingLines,
};
use crate::tokenizer::whitespace_parser::{
    parse_empty_lines, parse_trailing_whitespace, Config, WhitespaceError,
};

pub type Result<T> = std::result::Result<T, WhitespaceError>;

pub trait Inflate<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()>;
}

impl<'a> Inflate<'a> for IndentedBlock<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        for stat in &mut self.body {
            stat.inflate(config)?;
        }
        // We want to be able to only keep comments in the footer that are actually for
        // this IndentedBlock. We do so by assuming that lines which are indented to the
        // same level as the block itself are comments that go at the footer of the
        // block. Comments that are indented to less than this indent are assumed to
        // belong to the next line of code. We override the indent here because the
        // dedent node's absolute indent is the resulting indentation after the dedent
        // is performed. Its this way because the whitespace state for both the dedent's
        // whitespace_after and the next BaseCompoundStatement's whitespace_before is
        // shared. This allows us to partially parse here and parse the rest of the
        // whitespace and comments on the next line, effectively making sure that
        // comments are attached to the correct node.
        let footer = parse_empty_lines(
            config,
            &mut self.dedent_tok.whitespace_after,
            Some(self.indent_tok.whitespace_before.absolute_indent),
        )?;
        let header = parse_trailing_whitespace(config, &mut self.newline_tok.whitespace_before)?;
        self.footer = footer;
        self.header = header;
        self.indent = self.indent_tok.relative_indent;
        Ok(())
    }
}

impl<'a> Inflate<'a> for Module<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        for stat in &mut self.body {
            stat.inflate(config)?;
        }
        let mut footer = parse_empty_lines(config, &mut self.eof_tok.whitespace_before, Some(""))?;
        let mut header = vec![];
        if let Some(stmt) = self.body.first_mut() {
            swap(&mut stmt.leading_lines(), &mut &header);
            let mut last_indented = None;
            for (num, line) in footer.iter().enumerate() {
                if !line.whitespace.0.is_empty() {
                    last_indented = Some(num);
                } else if line.comment.is_some() {
                    // This is a non-indented comment. Everything from here should belong in the
                    // footer.
                    break;
                }
            }
            if let Some(num) = last_indented {
                if num + 1 == footer.len() {
                    footer = vec![];
                } else {
                    let (_, rest) = footer.split_at(num + 1);
                    footer = rest.to_vec();
                }
            }
        } else {
            swap(&mut header, &mut footer);
        }
        self.footer = footer;
        self.header = header;
        Ok(())
    }
}

impl<'a> Inflate<'a> for Statement<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::Compound(s) => s.inflate(config),
            Self::Simple(s) => s.inflate(config),
        }
    }
}

impl<'a> Inflate<'a> for SimpleStatementLine<'a> {
    fn inflate(&mut self, _config: &Config<'a>) -> Result<()> {
        Ok(())
    }
}

impl<'a> Inflate<'a> for CompoundStatement<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::FunctionDef(f) => f.inflate(config),
            Self::If(f) => f.inflate(config),
            Self::For(f) => f.inflate(config),
            Self::While(f) => f.inflate(config),
            Self::ClassDef(f) => f.inflate(config),
            Self::Try(f) => f.inflate(config),
            Self::With(f) => f.inflate(config),
        }
    }
}

impl<'a> Inflate<'a> for FunctionDef<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        // TODO
        self.body.inflate(config)
    }
}

impl<'a> Inflate<'a> for If<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)?;
        self.orelse.inflate(config)
    }
}

impl<'a> Inflate<'a> for For<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)?;
        self.orelse.inflate(config)
    }
}

impl<'a> Inflate<'a> for While<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)?;
        self.orelse.inflate(config)
    }
}

impl<'a> Inflate<'a> for ClassDef<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)
    }
}

impl<'a> Inflate<'a> for Try<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)?;
        for h in &mut self.handlers {
            h.inflate(config)?;
        }
        self.orelse.inflate(config)?;
        self.finalbody.inflate(config)
    }
}

impl<'a> Inflate<'a> for With<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)
    }
}

impl<'a> Inflate<'a> for ExceptHandler<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)
    }
}

impl<'a> Inflate<'a> for Suite<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::IndentedBlock(b) => b.inflate(config),
            _ => Ok(()),
        }
    }
}

impl<'a> Inflate<'a> for OrElse<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        match self {
            Self::Elif(e) => e.inflate(config),
            Self::Else(e) => e.inflate(config),
        }
    }
}

impl<'a> Inflate<'a> for Else<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)
    }
}

impl<'a> Inflate<'a> for Finally<'a> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        self.body.inflate(config)
    }
}

impl<'a, T: Inflate<'a>> Inflate<'a> for Option<T> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        if let Some(t) = self {
            t.inflate(config)
        } else {
            Ok(())
        }
    }
}

impl<'a, T: Inflate<'a> + ?Sized> Inflate<'a> for Box<T> {
    fn inflate(&mut self, config: &Config<'a>) -> Result<()> {
        (**self).inflate(config)
    }
}
