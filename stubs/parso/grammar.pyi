from typing import Any, Callable, Generic, Sequence, TypeVar

from parso.utils import PythonVersionInfo


_Token = Any
_NodeT = TypeVar("_NodeT")

class Grammar(Generic[_NodeT]):
    _default_normalizer_config: Optional[Any] = ...
    _error_normalizer_config: Optional[Any] = None
    _start_nonterminal: str = ...
    _token_namespace: Optional[str] = None
    def __init__(
        self,
        text: str,
        tokenizer: Callable[[Sequence[str], int], Sequence[_Token]],
        parser: Any = ...,
        diff_parser: Any = None,
    ) -> None: ...
    def parse(
        self,
        code: Union[str, bytes] = None,
        error_recovery: bool = True,
        path: Optional[str] = None,
        start_symbol: Optional[str] = None,
        cache: bool = False,
        diff_cache: bool = False,
        cache_path: Optional[str] = None,
    ) -> _NodeT: ...

class PythonGrammar(Grammar):
    version_info: PythonVersionInfo
    def __init__(self, bnf_text: str, version_info: PythonVersionInfo) -> None: ...

# Realistically, this should be `language: Literal["python"]` since only python is
# supported, but pyre doesn't support literal types yet.
def load_grammar(
    language: str = "python", version: Optional[str] = None, path: str = None
) -> Grammar: ...
