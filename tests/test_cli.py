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
