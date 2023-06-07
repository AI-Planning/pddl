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
from textwrap import dedent

from pddl.parser.problem import ProblemParser
from pddl.requirements import Requirements


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
