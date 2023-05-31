#!/usr/bin/env python3
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

"""This module contains tests verifying that the formatter outputs are syntactically valid and semantically faithful."""
from pathlib import Path

import pytest

from pddl.formatter import domain_to_string
from tests.conftest import DOMAIN_FILES


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_domain_formatter(domain_parser, pddl_file: Path):
    """Test generated domain formatting."""
    expected_domain_obj = domain_parser(pddl_file.read_text())
    actual_domain_str = domain_to_string(expected_domain_obj)
    actual_domain_obj = domain_parser(actual_domain_str)
    assert actual_domain_obj == expected_domain_obj
