# -*- coding: utf-8 -*-

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
