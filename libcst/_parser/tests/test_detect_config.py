# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from typing import Union

from libcst._parser._detect_config import detect_config
from libcst._parser._types.config import ParserConfig, PartialParserConfig
from libcst.testing.utils import UnitTest, data_provider


class TestDetectConfig(UnitTest):
    @data_provider(
        {
            "empty_input": {
                "source": b"",
                "partial": PartialParserConfig(),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=["\n", ""],
                    encoding="utf-8",
                    default_indent="    ",
                    default_newline="\n",
                    has_trailing_newline=False,
                ),
            },
            "detect_trailing_newline_disabled": {
                "source": b"",
                "partial": PartialParserConfig(),
                "detect_trailing_newline": False,
                "expected_config": ParserConfig(
                    lines=[""],  # the trailing newline isn't inserted
                    encoding="utf-8",
                    default_indent="    ",
                    default_newline="\n",
                    has_trailing_newline=False,
                ),
            },
            "newline_inferred": {
                "source": b"first_line\r\n\nsomething\n",
                "partial": PartialParserConfig(),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=["first_line\r\n", "\n", "something\n", ""],
                    encoding="utf-8",
                    default_indent="    ",
                    default_newline="\r\n",
                    has_trailing_newline=True,
                ),
            },
            "newline_partial_given": {
                "source": b"first_line\r\nsecond_line\r\n",
                "partial": PartialParserConfig(default_newline="\n"),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=["first_line\r\n", "second_line\r\n", ""],
                    encoding="utf-8",
                    default_indent="    ",
                    default_newline="\n",  # The given partial disables inference
                    has_trailing_newline=True,
                ),
            },
            "indent_inferred": {
                "source": b"if test:\n\t  something\n",
                "partial": PartialParserConfig(),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=["if test:\n", "\t  something\n", ""],
                    encoding="utf-8",
                    default_indent="\t  ",
                    default_newline="\n",
                    has_trailing_newline=True,
                ),
            },
            "indent_partial_given": {
                "source": b"if test:\n\t  something\n",
                "partial": PartialParserConfig(default_indent="      "),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=["if test:\n", "\t  something\n", ""],
                    encoding="utf-8",
                    default_indent="      ",
                    default_newline="\n",
                    has_trailing_newline=True,
                ),
            },
            "encoding_inferred": {
                "source": b"#!/usr/bin/python3\n# -*- coding: latin-1 -*-\npass\n",
                "partial": PartialParserConfig(),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=[
                        "#!/usr/bin/python3\n",
                        "# -*- coding: latin-1 -*-\n",
                        "pass\n",
                        "",
                    ],
                    encoding="iso-8859-1",  # this is an alias for latin-1
                    default_indent="    ",
                    default_newline="\n",
                    has_trailing_newline=True,
                ),
            },
            "encoding_partial_given": {
                "source": b"#!/usr/bin/python3\n# -*- coding: latin-1 -*-\npass\n",
                "partial": PartialParserConfig(encoding="us-ascii"),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=[
                        "#!/usr/bin/python3\n",
                        "# -*- coding: latin-1 -*-\n",
                        "pass\n",
                        "",
                    ],
                    encoding="us-ascii",
                    default_indent="    ",
                    default_newline="\n",
                    has_trailing_newline=True,
                ),
            },
            "encoding_str_not_bytes_disables_inference": {
                "source": "#!/usr/bin/python3\n# -*- coding: latin-1 -*-\npass\n",
                "partial": PartialParserConfig(),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=[
                        "#!/usr/bin/python3\n",
                        "# -*- coding: latin-1 -*-\n",
                        "pass\n",
                        "",
                    ],
                    encoding="utf-8",  # because source is a str, don't infer latin-1
                    default_indent="    ",
                    default_newline="\n",
                    has_trailing_newline=True,
                ),
            },
            "encoding_non_ascii_compatible_utf_16_with_bom": {
                "source": b"\xff\xfet\x00e\x00s\x00t\x00",
                "partial": PartialParserConfig(encoding="utf-16"),
                "detect_trailing_newline": True,
                "expected_config": ParserConfig(
                    lines=["test\n", ""],
                    encoding="utf-16",
                    default_indent="    ",
                    default_newline="\n",
                    has_trailing_newline=False,
                ),
            },
        }
    )
    def test_detect_module_config(
        self,
        *,
        source: Union[str, bytes],
        partial: PartialParserConfig,
        detect_trailing_newline: bool,
        expected_config: ParserConfig,
    ) -> None:
        self.assertEqual(
            detect_config(
                source, partial=partial, detect_trailing_newline=detect_trailing_newline
            ).config,
            expected_config,
        )
