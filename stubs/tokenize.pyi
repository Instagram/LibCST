from token import (
    AMPER,
    AMPEREQUAL,
    AT,
    ATEQUAL,
    CIRCUMFLEX,
    CIRCUMFLEXEQUAL,
    COLON,
    COLONEQUAL,
    COMMA,
    COMMENT,
    DEDENT,
    DOT,
    DOUBLESLASH,
    DOUBLESLASHEQUAL,
    DOUBLESTAR,
    DOUBLESTAREQUAL,
    ELLIPSIS,
    ENCODING,
    ENDMARKER,
    EQEQUAL,
    EQUAL,
    ERRORTOKEN,
    EXACT_TOKEN_TYPES,
    GREATER,
    GREATEREQUAL,
    INDENT,
    LBRACE,
    LEFTSHIFT,
    LEFTSHIFTEQUAL,
    LESS,
    LESSEQUAL,
    LPAR,
    LSQB,
    MINEQUAL,
    MINUS,
    N_TOKENS,
    NAME,
    NEWLINE,
    NL,
    NOTEQUAL,
    NT_OFFSET,
    NUMBER,
    OP,
    PERCENT,
    PERCENTEQUAL,
    PLUS,
    PLUSEQUAL,
    RARROW,
    RBRACE,
    RIGHTSHIFT,
    RIGHTSHIFTEQUAL,
    RPAR,
    RSQB,
    SEMI,
    SLASH,
    SLASHEQUAL,
    STAR,
    STAREQUAL,
    STRING,
    TILDE,
    TYPE_COMMENT,
    TYPE_IGNORE,
    VBAR,
    VBAREQUAL,
)
from typing import Callable, Generator, Sequence, Tuple

Hexnumber: str = ...
Binnumber: str = ...
Octnumber: str = ...
Decnumber: str = ...
Intnumber: str = ...
Exponent: str = ...
Pointfloat: str = ...
Expfloat: str = ...
Floatnumber: str = ...
Imagnumber: str = ...
Number: str = ...
Whitespace: str = ...
Comment: str = ...
Ignore: str = ...
Name: str = ...

class TokenInfo(Tuple[int, str, Tuple[int, int], Tuple[int, int], int]):
    exact_type: int = ...
    type: int = ...
    string: str = ...
    start: Tuple[int, int] = ...
    end: Tuple[int, int] = ...
    line: int = ...
    def __repr__(self) -> str: ...

def detect_encoding(readline: Callable[[], bytes]) -> Tuple[str, Sequence[bytes]]: ...
def tokenize(readline: Callable[[], bytes]) -> Generator[TokenInfo, None, None]: ...
