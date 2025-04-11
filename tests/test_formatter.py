#!/usr/bin/env python3
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

"""This module contains tests verifying that the formatter outputs are syntactically valid and semantically faithful."""
from pathlib import Path
from textwrap import dedent

import pytest

from pddl.action import Action
from pddl.core import Domain, Problem
from pddl.formatter import domain_to_string, problem_to_string
from pddl.logic import Constant, Variable, constants
from pddl.logic.base import And, ForallCondition
from pddl.logic.functions import (
    EqualTo,
    GreaterEqualThan,
    Increase,
    LesserEqualThan,
    NumericFunction,
    NumericValue,
)
from pddl.requirements import Requirements
from tests.conftest import DOMAIN_FILES, PROBLEM_FILES


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_domain_formatter(domain_parser, pddl_file: Path):
    """Test generated domain formatting."""
    expected_domain_obj = domain_parser(pddl_file.read_text())
    actual_domain_str = domain_to_string(expected_domain_obj)
    actual_domain_obj = domain_parser(actual_domain_str)
    assert (
        actual_domain_obj == expected_domain_obj
    ), f"Domain mismatch for {str(pddl_file).split('/')[-2]}"


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_problem_formatter(problem_parser, pddl_file):
    """Test generated problem formatting."""
    expected_problem_obj = problem_parser(pddl_file.read_text())
    actual_problem_str = problem_to_string(expected_problem_obj)
    actual_problem_obj = problem_parser(actual_problem_str)
    assert actual_problem_obj == expected_problem_obj


def test_typed_constants_formatting_in_domain() -> None:
    """Test that types and typed constants are formatted correctly."""
    t1, t2, t3 = "type_1", "type_2", "type_3"

    a1, b1, c1 = constants("a b c", type_=t1)
    d2, e2, f2 = constants("d e f", type_=t2)
    g3, h3, i3 = constants("g h i", type_=t3)
    j, k, lc = constants("j k l", type_=None)

    # define the domain object.
    domain = Domain(
        "my_domain",
        requirements=[Requirements.TYPING],
        types={t1: None, t2: t1, t3: t1},
        constants=[a1, b1, c1, d2, e2, f2, g3, h3, i3, j, k, lc],
    )

    domain_str = domain_to_string(domain)

    assert domain_str == dedent(
        """\
    (define (domain my_domain)
        (:requirements :typing)
        (:types
            type_1 - object
            type_2 type_3 - type_1
        )
        (:constants a b c - type_1 d e f - type_2 g h i - type_3 j k l)
    )"""
    )


def test_typed_objects_formatting_in_problem() -> None:
    """Test that typed objects are formatted correctly."""
    t1, t2, t3 = "type_1", "type_2", "type_3"

    a1, b1, c1 = constants("a b c", type_=t1)
    d2, e2, f2 = constants("d e f", type_=t2)
    g3, h3, i3 = constants("g h i", type_=t3)
    j, k, lc = constants("j k l", type_=None)

    problem = Problem(
        "problem-1",
        domain_name="my_domain",
        requirements=[Requirements.TYPING],
        objects=[a1, b1, c1, d2, e2, f2, g3, h3, i3, j, k, lc],
    )
    problem_str = problem_to_string(problem)

    assert problem_str == dedent(
        """\
    (define (problem problem-1)
        (:domain my_domain)
        (:requirements :typing)
        (:objects a b c - type_1 d e f - type_2 g h i - type_3 j k l)
        (:init )
        (:goal (and ))
    )"""
    )


def test_numerical_hello_world_domain_formatter():
    """Test that numerical NumericFunctions are formatted correctly."""
    neighbor = Variable("neighbor")
    hello_counter = NumericFunction("hello_counter", neighbor)
    action = Action(
        "say-hello-world",
        parameters=[neighbor],
        precondition=LesserEqualThan(hello_counter, NumericValue(3)),
        effect=And(Increase(hello_counter, NumericValue(1))),
    )

    domain = Domain(
        name="hello-world-functions",
        requirements=[Requirements.STRIPS, Requirements.NUMERIC_FLUENTS],
        functions={hello_counter: None},
        actions=[action],
    )

    assert domain_to_string(domain) == "\n".join(
        (
            "(define (domain hello-world-functions)",
            "    (:requirements :numeric-fluents :strips)",
            "    (:functions (hello_counter ?neighbor))",
            "    (:action say-hello-world",
            "        :parameters (?neighbor)",
            "        :precondition (<= (hello_counter ?neighbor) 3)",
            "        :effect (increase (hello_counter ?neighbor) 1)",
            "    )",
            ")",
        )
    )


def test_numerical_hello_world_problem_formatter():
    """Test that numerical NumericFunctions are formatted correctly."""
    neighbors = [Constant(name, "neighbor") for name in ("Alice", "Bob", "Charlie")]
    problem = Problem(
        name="hello-3-times",
        domain_name="hello-world-functions",
        objects=neighbors,
        init=[
            EqualTo(NumericFunction("hello_counter", neighbor), NumericValue(0))
            for neighbor in neighbors
        ],
        goal=ForallCondition(
            GreaterEqualThan(
                NumericFunction("hello_counter", Variable("neighbor")), NumericValue(1)
            ),
            [Variable("neighbor", ["neighbor"])],
        ),
    )

    assert problem_to_string(problem) == "\n".join(
        (
            "(define (problem hello-3-times)",
            "    (:domain hello-world-functions)",
            "    (:objects Alice Bob Charlie - neighbor)",
            "    (:init (= (hello_counter Alice) 0) (= (hello_counter Bob) 0) (= (hello_counter Charlie) 0))",
            "    (:goal (forall (?neighbor - neighbor) (>= (hello_counter ?neighbor) 1)))",
            ")",
        )
    )
