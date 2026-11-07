# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import builtins
import importlib
import pickle
import sys
import tempfile
import threading
import time
import unittest
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from types import SimpleNamespace
from typing import Dict, List, Mapping, Optional, Sequence, Tuple
from unittest.mock import patch

import libcst as cst
from libcst.metadata import (
    CodePosition,
    FullRepoManager,
    JediDefinition,
    JediDefinitionProvider,
    MetadataWrapper,
)


class JediDefinitionProviderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)

    def wrapper(self, code: str, path: str = "example.py") -> MetadataWrapper:
        cache = JediDefinitionProvider.gen_cache(self.root, [path])
        return MetadataWrapper(
            cst.parse_module(code), cache={JediDefinitionProvider: cache[path]}
        )

    def call(
        self, wrapper: MetadataWrapper, index: int = -1, expression_index: int = 0
    ) -> cst.Call:
        statement = cst.ensure_type(wrapper.module.body[index], cst.SimpleStatementLine)
        expression = cst.ensure_type(statement.body[expression_index], cst.Expr)
        return cst.ensure_type(expression.value, cst.Call)

    def test_function_and_call_resolve_to_definition(self) -> None:
        wrapper = self.wrapper("def produce():\n    return 1\nproduce()\n")
        data = wrapper.resolve(JediDefinitionProvider)
        function = cst.ensure_type(wrapper.module.body[0], cst.FunctionDef)
        call = self.call(wrapper, 1)
        expected = (
            JediDefinition(
                "produce",
                "example.produce",
                "function",
                "example",
                self.root / "example.py",
                CodePosition(1, 4),
            ),
        )
        self.assertEqual(data[function.name], expected)
        self.assertEqual(data[call.func], expected)
        self.assertEqual(data[call], expected)
        self.assertEqual(pickle.loads(pickle.dumps(expected)), expected)

    def test_attributes_use_the_attribute_identifier(self) -> None:
        wrapper = self.wrapper(
            "class Factory:\n"
            "    def build(self):\n"
            "        return 1\n"
            "Factory().build()\n"
        )
        call = self.call(wrapper)
        data = wrapper.resolve(JediDefinitionProvider)
        self.assertEqual(data[call][0].full_name, "example.Factory.build")
        self.assertEqual(data[call.func], data[call])
        attribute = cst.ensure_type(call.func, cst.Attribute)
        self.assertEqual(data[attribute.attr], data[call])
        self.assertEqual(data[attribute.value][0].full_name, "example.Factory")

    def test_relative_import_and_alias_with_full_repo_manager(self) -> None:
        package = self.root / "pkg"
        package.mkdir()
        (package / "__init__.py").write_text("")
        (package / "helper.py").write_text("def target():\n    pass\n")
        (package / "caller.py").write_text(
            "from .helper import target as alias\nalias()\n"
        )
        manager = FullRepoManager(
            self.root, ["pkg/caller.py"], {JediDefinitionProvider}
        )
        wrapper = manager.get_metadata_wrapper_for_path("pkg/caller.py")
        call = self.call(wrapper)
        definition = wrapper.resolve(JediDefinitionProvider)[call][0]
        self.assertEqual(definition.full_name, "pkg.helper.target")
        self.assertEqual(definition.module_path, package / "helper.py")
        self.assertEqual(definition.position, CodePosition(1, 4))
        cached = pickle.loads(pickle.dumps(manager.get_cache_for_path("pkg/caller.py")))
        second = MetadataWrapper(wrapper.module, cache=cached)
        second_call = self.call(second)
        self.assertEqual(
            second.resolve(JediDefinitionProvider)[second_call], (definition,)
        )

    def test_uses_current_module_not_file_contents(self) -> None:
        (self.root / "example.py").write_text("def stale(): pass\nstale()\n")
        for name in ("fresh", "newer"):
            wrapper = self.wrapper(f"def {name}(): pass\n{name}()\n")
            call = self.call(wrapper)
            self.assertEqual(
                wrapper.resolve(JediDefinitionProvider)[call][0].name, name
            )

    def test_unicode_identifiers_and_crlf_columns(self) -> None:
        wrapper = self.wrapper(
            "def caf\u00e9():\r\n    return 1\r\n\u00e9 = 'text'; caf\u00e9()\r\n"
        )
        call = self.call(wrapper, expression_index=-1)
        definition = wrapper.resolve(JediDefinitionProvider)[call][0]
        self.assertEqual(definition.name, "caf\u00e9")
        self.assertEqual(definition.position, CodePosition(1, 4))

    def test_multiple_and_unresolved_definitions(self) -> None:
        wrapper = self.wrapper(
            "def left(): pass\ndef right(): pass\n"
            "if condition:\n    choice = left\nelse:\n    choice = right\n"
            "choice()\nmissing()\n"
        )
        data = wrapper.resolve(JediDefinitionProvider)
        choice = self.call(wrapper, -2)
        missing = self.call(wrapper)
        self.assertEqual({item.name for item in data[choice]}, {"left", "right"})
        self.assertEqual(data[missing], ())
        self.assertEqual(data[missing.func], ())

    def test_builtin_is_definition_not_expression_type(self) -> None:
        wrapper = self.wrapper("len('value')\n")
        call = self.call(wrapper, 0)
        definitions = wrapper.resolve(JediDefinitionProvider)[call]
        self.assertEqual(len(definitions), 1)
        self.assertEqual(definitions[0].full_name, "builtins.len")
        self.assertEqual(definitions[0].type, "function")

    def test_other_call_expression_is_not_assigned_metadata(self) -> None:
        wrapper = self.wrapper("(lambda: 1)()\n[print][0]()\n")
        data = wrapper.resolve(JediDefinitionProvider)
        for index in range(len(wrapper.module.body)):
            self.assertNotIn(self.call(wrapper, index), data)

    def test_cache_keys_and_resolved_paths(self) -> None:
        cache = JediDefinitionProvider.gen_cache(
            self.root, ["pkg/../example.py"], timeout=1, use_pyproject_toml=True
        )
        self.assertEqual(list(cache), ["pkg/../example.py"])
        with self.assertRaisesRegex(ValueError, "Cache is required"):
            MetadataWrapper(cst.parse_module("x\n")).resolve(JediDefinitionProvider)

    def test_optional_import_is_lazy_and_has_actionable_error(self) -> None:
        wrapper = self.wrapper("x\n")
        with patch.dict(sys.modules, {"jedi": None}):
            # Reloading the public metadata namespace must not import Jedi.
            importlib.reload(sys.modules["libcst.metadata"])
            with self.assertRaisesRegex(ImportError, r'install "libcst\[jedi\]"'):
                wrapper.resolve(JediDefinitionProvider)

    def test_dependency_import_failure_is_not_reported_as_missing_jedi(self) -> None:
        original_import = builtins.__import__
        error = ModuleNotFoundError("missing Jedi dependency", name="parso")

        def import_module(
            name: str,
            globals: Optional[Mapping[str, object]] = None,
            locals: Optional[Mapping[str, object]] = None,
            fromlist: Sequence[str] = (),
            level: int = 0,
        ) -> object:
            if name == "jedi":
                raise error
            return original_import(name, globals, locals, fromlist, level)

        with patch("builtins.__import__", import_module):
            with self.assertRaises(ModuleNotFoundError) as raised:
                self.wrapper("x\n").resolve(JediDefinitionProvider)
        self.assertIs(raised.exception, error)

    def test_shared_position_is_inferred_once_and_missing_location_is_supported(
        self,
    ) -> None:
        queries: List[Tuple[int, int]] = []
        result = SimpleNamespace(
            name="target",
            full_name=None,
            type="function",
            module_name="builtins",
            module_path=None,
            line=None,
            column=None,
        )

        class Script:
            def __init__(self, **kwargs: object) -> None:
                pass

            def infer(self, line: int, column: int) -> List[SimpleNamespace]:
                queries.append((line, column))
                return [result]

        wrapper = self.wrapper("obj.target()\n")
        with patch("jedi.Script", Script):
            data = wrapper.resolve(JediDefinitionProvider)
        call = self.call(wrapper, 0)
        self.assertEqual(queries.count((1, 10)), 1)
        self.assertEqual(data[call], data[call.func])
        self.assertIsNone(data[call][0].position)
        self.assertEqual(
            pickle.loads(pickle.dumps(data[call])),
            (JediDefinition("target", None, "function", "builtins", None, None),),
        )

    def test_serializes_jedi_resolutions_between_threads(self) -> None:
        state: Dict[str, int] = {"active": 0, "maximum": 0}
        gate = threading.Lock()

        class Script:
            def __init__(self, **kwargs: object) -> None:
                with gate:
                    state["active"] += 1
                    state["maximum"] = max(state["maximum"], state["active"])

            def infer(self, line: int, column: int) -> List[SimpleNamespace]:
                time.sleep(0.01)
                with gate:
                    state["active"] -= 1
                return []

        def resolve(index: int) -> None:
            self.wrapper("name\n", f"file{index}.py").resolve(JediDefinitionProvider)

        with patch("jedi.Script", Script), ThreadPoolExecutor(max_workers=4) as pool:
            list(pool.map(resolve, range(8)))
        self.assertEqual(state["maximum"], 1)

    def test_inference_failure_releases_lock_for_another_thread(self) -> None:
        with patch("jedi.Script.infer", side_effect=RuntimeError("inference failed")):
            with self.assertRaisesRegex(RuntimeError, "inference failed"):
                self.wrapper("x\n").resolve(JediDefinitionProvider)
        with ThreadPoolExecutor(max_workers=1) as pool:
            result = pool.submit(
                lambda: self.wrapper("len\n").resolve(JediDefinitionProvider)
            ).result(timeout=10)
        self.assertTrue(result)
