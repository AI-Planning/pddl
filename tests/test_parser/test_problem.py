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

"""This module contains the tests for the domain parser."""
from textwrap import dedent

import lark
import pytest

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


def test_problem_objects_repetition_in_simple_typed_lists_not_allowed() -> None:
    """Check objects repetition in simple typed lists is detected and a parsing error is raised."""
    problem_str = dedent(
        """
    (define (problem test-problem)
        (:domain test-domain)
        (:requirements :typing)
        (:objects a b c a)
        (:init )
        (:goal (and ))
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['a', 'b', 'c', 'a'\\]: "
        "duplicate name 'a' in typed list already present",
    ):
        ProblemParser()(problem_str)


def test_problem_objects_repetition_in_typed_lists_not_allowed() -> None:
    """Check objects repetition in typed lists is detected and a parsing error is raised."""
    problem_str = dedent(
        """
    (define (problem test-problem)
        (:domain test-domain)
        (:requirements :typing)
        (:objects a - t1 b - t2 c - t3 a - t4)
        (:init )
        (:goal (and ))
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=r".*error while parsing tokens \['a', '-', 't1', 'b', '-', 't2', 'c', '-', 't3', 'a', '-', 't4'\]: "
        r"duplicate name 'a' in typed list already inherits from types \['t1'\]",
    ):
        ProblemParser()(problem_str)


def test_problem_init_predicate_arg_repetition_allowed() -> None:
    """Check argument repetition in predicate list is allowed."""
    problem_str = dedent(
        """
    (define (problem test-problem)
        (:domain test-domain)
        (:requirements :typing)
        (:objects a)
        (:init (p a a))
        (:goal (and ))
    )
    """
    )
    ProblemParser()(problem_str)


def test_problem_init_predicate_repetition_name_allowed() -> None:
    """Check predicate repetition in init condition is allowed."""
    problem_str = dedent(
        """
    (define (problem test-problem)
        (:domain test-domain)
        (:requirements :typing)
        (:objects a)
        (:init (p a a) (p a a))
        (:goal (and ))
    )
    """
    )
    ProblemParser()(problem_str)


def test_numeric_comparison_in_goal() -> None:
    """Try to parse a goal with a numeric condition."""
    problem_str = dedent(
        """
    (define (problem hello-3-times)
        (:domain hello-world-functions)

        (:init
            ; if this was undefined, some planners would not assumed `0`
            (= (hello_counter jimmy) 0)
        )

        (:goal
            (> 5 3)
        )
    )
    """
    )
    ProblemParser()(problem_str)


def test_numeric_function_comparison_in_goal() -> None:
    """Try to parse a goal with a numeric condition and function."""
    problem_str = dedent(
        """
    (define (problem hello-3-times)
        (:domain hello-world-functions)

        (:init
            ; if this was undefined, some planners would not assumed `0`
            (= (hello_counter jimmy) 0)
        )

        (:goal
            (>= (hello_counter jimmy) 3)
        )
    )
    """
    )
    ProblemParser()(problem_str)


def test_numeric_function_equality_in_goal() -> None:
    """Try to parse a goal with a numeric condition and function."""
    problem_str = dedent(
        """
    (define (problem hello-3-times)
        (:domain hello-world-functions)

        (:init
            ; if this was undefined, some planners would not assumed `0`
            (= (hello_counter jimmy) 0)
            (= (hello_counter jammy) 0)
        )

        (:goal
            (and
                (= 3 (hello_counter jimmy))
                (= (hello_counter jammy) 5)
            )
        )
    )
    """
    )
    ProblemParser()(problem_str)
