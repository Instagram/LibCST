# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
from typing import Optional

from libcst.codemod._cli import _calculate_module
from libcst.testing.utils import data_provider, UnitTest


class TestPackageCalculation(UnitTest):
    @data_provider(
        (
            # Providing no root should give back no module.
            (None, "/some/dummy/file.py", None),
            # Providing a file outside the root should give back no module.
            ("/home/username/root", "/some/dummy/file.py", None),
            ("/home/username/root/", "/some/dummy/file.py", None),
            ("/home/username/root", "/home/username/file.py", None),
            # Various files inside the root should give back valid modules.
            ("/home/username/root", "/home/username/root/file.py", "file"),
            ("/home/username/root/", "/home/username/root/file.py", "file"),
            (
                "/home/username/root/",
                "/home/username/root/some/dir/file.py",
                "some.dir.file",
            ),
            # Various special files inside the root should give back valid modules.
            (
                "/home/username/root/",
                "/home/username/root/some/dir/__init__.py",
                "some.dir",
            ),
            (
                "/home/username/root/",
                "/home/username/root/some/dir/__main__.py",
                "some.dir",
            ),
            # some windows tests
            (
                "c:/Program Files/",
                "d:/Program Files/some/dir/file.py",
                None,
            ),
            (
                "c:/Program Files/other/",
                "c:/Program Files/some/dir/file.py",
                None,
            ),
            (
                "c:/Program Files/",
                "c:/Program Files/some/dir/file.py",
                "some.dir.file",
            ),
            (
                "c:/Program Files/",
                "c:/Program Files/some/dir/__main__.py",
                "some.dir",
            ),
        ),
    )
    def test_calculate_module(
        self, repo_root: Optional[str], filename: str, module: str
    ) -> None:
        self.assertEqual(_calculate_module(repo_root, filename), module)
