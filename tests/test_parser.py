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

"""This module contains the tests for the domain parser."""
from pathlib import Path

import pytest
from pytest import lazy_fixture  # type: ignore  # noqa

# from pddl.core import Domain, Problem
from pddl.core import Domain, Problem
from tests.conftest import (
    BLOCKSWORLD_FILES,
    DOMAIN_FILES,
    PROBLEM_FILES,
    TRIANGLE_FILES,
)


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_domain_parser(domain_parser, pddl_file: Path):
    """Test only that the domain parsing works for all the fixtures."""
    domain_parser(pddl_file.read_text())


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_problem_parser(problem_parser, pddl_file: Path):
    """Test only that the problem parsing works for all the fixtures."""
    problem_parser(pddl_file.read_text())


@pytest.mark.parametrize(
    "pddl_file,expected_domain",
    [
        (BLOCKSWORLD_FILES / "domain.pddl", lazy_fixture("blocksworld_domain")),
        (TRIANGLE_FILES / "domain.pddl", lazy_fixture("triangle_tireworld_domain")),
    ],
)
def test_check_domain_parser_output(domain_parser, pddl_file: Path, expected_domain):
    """Test domain parsing."""
    actual_domain = domain_parser(pddl_file.read_text())

    assert isinstance(actual_domain, Domain)
    assert actual_domain == expected_domain


@pytest.mark.parametrize(
    "pddl_file,expected_problem",
    [(BLOCKSWORLD_FILES / "p01.pddl", lazy_fixture("blocksworld_problem_01"))],
)
def test_check_problem_parser_output(problem_parser, pddl_file: Path, expected_problem):
    """Test problem parsing."""
    problem = problem_parser(pddl_file.read_text())

    assert isinstance(problem, Problem)
    assert problem == expected_problem
