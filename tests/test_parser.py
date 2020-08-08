# -*- coding: utf-8 -*-

"""This module contains the tests for the domain parser."""
from pathlib import Path

import pytest
from pytest import lazy_fixture  # type: ignore  # noqa

from pddl.core import Domain, Problem
from tests.conftest import BLOCKSWORLD_FILES


@pytest.mark.parametrize(
    "pddl_file",
    [
        BLOCKSWORLD_FILES / "domain.pddl",
        # TRIANGLE_FILES / "domain.pddl",  TODO: effect of action with nested and-oneof-and
    ],
)
def test_domain_parser(domain_parser, pddl_file: Path):
    """Test only that the domain parsing works for all the fixtures."""
    domain_parser(pddl_file.read_text())


@pytest.mark.parametrize("pddl_file", [*BLOCKSWORLD_FILES.glob("./p*.pddl")])
def test_problem_parser(problem_parser, pddl_file: Path):
    """Test only that the problem parsing works for all the fixtures."""
    problem_parser(pddl_file.read_text())


@pytest.mark.parametrize(
    "pddl_file,expected_domain",
    [(BLOCKSWORLD_FILES / "domain.pddl", lazy_fixture("blocksworld_domain"))],
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
