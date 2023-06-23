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
from textwrap import dedent

import pytest

from pddl.core import Domain, Problem
from pddl.formatter import domain_to_string, problem_to_string
from pddl.logic import constants
from pddl.requirements import Requirements
from tests.conftest import DOMAIN_FILES, PROBLEM_FILES


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_domain_formatter(domain_parser, pddl_file: Path):
    """Test generated domain formatting."""
    expected_domain_obj = domain_parser(pddl_file.read_text())
    actual_domain_str = domain_to_string(expected_domain_obj)
    actual_domain_obj = domain_parser(actual_domain_str)
    assert actual_domain_obj == expected_domain_obj


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
        (:types type_2 type_3 - type_1 type_1)
        (:constants a b c - type_1 d e f - type_2 g h i - type_3 j k l)
        (:predicates )
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
