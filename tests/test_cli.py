#
# Copyright 2021-2025 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""This module contains tests for the CLI tool."""
import logging

import pytest
from click.testing import CliRunner

from pddl.__main__ import cli
from tests.conftest import DOMAIN_FILES, PROBLEM_FILES


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_pddl_domain_cli(pddl_file):
    """Test PDDL domain cli."""
    logging.debug(f"Parsing domain file: {pddl_file}")
    result = CliRunner().invoke(
        cli, args=["domain", str(pddl_file)], catch_exceptions=False
    )
    assert result.exit_code == 0


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_pddl_problem_cli(pddl_file):
    """Test PDDL domain cli."""
    logging.debug(f"Parsing problem file: {pddl_file}")
    result = CliRunner().invoke(
        cli, args=["problem", str(pddl_file)], catch_exceptions=False
    )
    assert result.exit_code == 0
