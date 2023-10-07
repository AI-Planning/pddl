#
# Copyright 2021-2023 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""Test introduction documentation page."""
import logging
from io import StringIO
from unittest.mock import patch

from tests.conftest import ROOT_DIRECTORY
from tests.test_docs.base import BaseTestMarkdownDocs, compile_and_exec


class TestIntroduction(BaseTestMarkdownDocs):
    """Test that the code snippet in the introduction are correct."""

    MD_FILE = ROOT_DIRECTORY / "README.md"

    @classmethod
    def setup_class(cls):
        """Set up the test."""
        super().setup_class()
        cls.python_code_blocks = cls.extract_code_blocks("python")
        cls.output_code_blocks = cls.extract_code_blocks("output")
        cls.locals = {}

    def test_python_and_output(self):
        """Test Python snipped and its output."""
        for python_code_block, output_code_block in zip(
            self.python_code_blocks, self.output_code_blocks
        ):
            logging.debug(f"Testing block: {python_code_block[:50]}...")
            with patch("sys.stdout", new_callable=StringIO) as mock_stdout:
                compile_and_exec(python_code_block, locals_dict=self.locals)
                actual_output = mock_stdout.getvalue()
                expected_output = output_code_block
                assert actual_output == expected_output
