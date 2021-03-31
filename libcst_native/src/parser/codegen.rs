#[derive(Debug)]
pub struct CodegenState {
    pub tokens: Vec<String>,
    pub indent_tokens: Vec<String>,
    pub default_newline: String,
}

impl CodegenState {
    pub fn indent(&mut self, v: String) {
        self.indent_tokens.push(v);
    }
    pub fn dedent(&mut self) {
        self.indent_tokens.pop();
    }
    pub fn add_indent(&mut self) {
        self.tokens.extend(self.indent_tokens.iter().cloned());
    }
    pub fn add_token(&mut self, tok: String) {
        self.tokens.push(tok);
    }

    pub fn to_string(self) -> String {
        self.tokens.as_slice().concat()
    }
}

pub trait Codegen {
    fn codegen(&self, state: &mut CodegenState) -> ();
}

#[cfg(windows)]
const LINE_ENDING: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static str = "\n";

impl Default for CodegenState {
    fn default() -> Self {
        Self {
            default_newline: LINE_ENDING.to_string(),
            indent_tokens: Default::default(),
            tokens: Default::default(),
        }
    }
}
