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

"""This module contains parametrized tests for both domains and problems parsers."""
from pathlib import Path

import pytest
from pytest_lazy_fixtures import lf as lazy_fixture  # type:ignore  # noqa

from pddl.core import Domain, Problem
from tests.conftest import (
    BLOCKSWORLD_FILES,
    BLOCKSWORLD_FOND_FILES,
    PROBLEM_FILES,
    TRIANGLE_FILES,
)


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_problem_parser(domain_parser, problem_parser, pddl_file: Path):
    """Test problem parsing and validation works for all the fixtures."""
    problem = problem_parser(pddl_file.read_text())
    domain_file = pddl_file.parent / "domain.pddl"
    domain = domain_parser(domain_file.read_text())
    problem.check(domain)


@pytest.mark.parametrize(
    "pddl_file,expected_problem",
    [
        (
            BLOCKSWORLD_FILES / "p01.pddl",
            lazy_fixture("blocksworld_problem_01"),  # type:ignore
        ),
        (
            BLOCKSWORLD_FOND_FILES / "p01.pddl",
            lazy_fixture("blocksworld_fond_01"),  # type:ignore
        ),
    ],
)
def test_check_problem_parser_output(problem_parser, pddl_file: Path, expected_problem):
    """Test problem parsing."""
    actual_problem = problem_parser(pddl_file.read_text())

    assert isinstance(actual_problem, Problem)
    assert actual_problem == expected_problem


@pytest.mark.parametrize(
    "pddl_file,expected_domain",
    [
        (
            BLOCKSWORLD_FILES / "domain.pddl",
            lazy_fixture("blocksworld_domain"),  # type:ignore
        ),
        (
            TRIANGLE_FILES / "domain.pddl",
            lazy_fixture("triangle_tireworld_domain"),  # type:ignore
        ),
        (
            BLOCKSWORLD_FOND_FILES / "domain.pddl",
            lazy_fixture("blocksworld_fond_domain"),  # type:ignore
        ),
    ],
)
def test_check_domain_parser_output(domain_parser, pddl_file: Path, expected_domain):
    """Test domain parsing."""
    actual_domain = domain_parser(pddl_file.read_text())

    assert isinstance(actual_domain, Domain)
    assert actual_domain == expected_domain
