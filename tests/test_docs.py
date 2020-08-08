# -*- coding: utf-8 -*-
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Lydia is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Lydia.  If not, see <https://www.gnu.org/licenses/>.
#

"""This module contains tests for the code in the docs."""
import logging
from io import StringIO
from pathlib import Path
from unittest.mock import patch


def test_readme_example(markdown_parser):
    """Test the readme example."""
    all_blocks = markdown_parser.parse(Path("README.md").read_text())

    python_blocks = [
        b for b in all_blocks if b["type"] == "block_code" and b["info"] == "python"
    ]
    output_blocks = [
        b for b in all_blocks if b["type"] == "block_code" and b["info"] == "output"
    ]

    # zip to the shortest list of blocks
    for python_block, output_block in zip(python_blocks, output_blocks):
        logging.debug(f"Testing block: {python_block['text'][:50]}...")
        with patch("sys.stdout", new_callable=StringIO) as mock_stdout:
            exec(python_block["text"])
            actual_output = mock_stdout.getvalue()
            expected_output = output_block["text"]
            assert actual_output == expected_output
