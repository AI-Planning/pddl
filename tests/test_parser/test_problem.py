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

"""This module contains the tests for the domain parser."""
from pathlib import Path
from textwrap import dedent

import pytest
from pytest import lazy_fixture  # type:ignore  # noqa

from pddl.core import Problem
from pddl.parser.problem import ProblemParser
from pddl.requirements import Requirements
from tests.conftest import BLOCKSWORLD_FILES, BLOCKSWORLD_FOND_FILES, PROBLEM_FILES


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_problem_parser(problem_parser, pddl_file: Path):
    """Test only that the problem parsing works for all the fixtures."""
    problem_parser(pddl_file.read_text())


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


def test_problem_requirements_section_parsed() -> None:
    """Check that the requirements section is parsed correctly."""
    problem_str = dedent(
        """
        (define (problem test-problem)
            (:domain test-domain)
            (:requirements :typing)
            (:objects a b c)
            (:init (p b b b))
            (:goal (g a a a))
        )"""
    )
    problem = ProblemParser()(problem_str)

    assert problem.requirements == {Requirements.TYPING}
