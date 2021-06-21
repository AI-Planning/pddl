# -*- coding: utf-8 -*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl.  If not, see <https://www.gnu.org/licenses/>.
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
