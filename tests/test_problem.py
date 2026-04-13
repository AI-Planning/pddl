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

"""This module contains tests for a PDDL problem."""

import copy
import pickle  # nosec

import pytest

from pddl.core import Domain, Problem
from pddl.exceptions import PDDLValidationError
from pddl.logic.base import And, Not
from pddl.logic.functions import EqualTo as FunctionEqualTo
from pddl.logic.functions import (
    GreaterEqualThan,
    GreaterThan,
    LesserEqualThan,
    LesserThan,
    Metric,
    NumericFunction,
)
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate
from pddl.parser.symbols import Symbols
from pddl.requirements import Requirements
from tests.conftest import pddl_objects_problems


@pytest.mark.parametrize("problem_obj", pddl_objects_problems)
def test_pickle_problem(problem_obj: Problem) -> None:
    """Test that problem objects can be pickled correctly."""
    problem_obj_bytes = pickle.dumps(problem_obj)  # nosec
    actual_problem_obj = pickle.loads(problem_obj_bytes)  # nosec
    assert problem_obj == actual_problem_obj


@pytest.mark.parametrize("problem_obj", pddl_objects_problems)
def test_deepcopy_problem(problem_obj: Problem) -> None:
    """Test that problem objects can be deepcopied correctly."""
    new_problem_obj = copy.deepcopy(problem_obj)
    assert problem_obj == new_problem_obj


class TestProblemEmpty:
    """Test the empty problem."""

    def setup_method(self):
        """Set up the tests."""
        self.domain = Domain("empty_domain")
        self.problem = Problem("empty_problem", self.domain)

    def test_name(self):
        """Test the name getter."""
        assert self.problem.name == "empty_problem"

    def test_requirements(self):
        """Test the requirements getter."""
        assert self.problem.requirements == set()

    def test_objects(self):
        """Test the objects getter."""
        assert self.problem.objects == set()

    def test_init(self):
        """Test the init getter."""
        assert self.problem.init == set()

    def test_goal(self):
        """Test the goal getter."""
        assert self.problem.goal == And()


def test_build_simple_problem():
    """Test a simple PDDL problem."""
    x, y, z = variables("x y z")
    o1, o2, o3 = constants("o1 o2 o3")
    p = Predicate("p", x, y, z)
    q = Predicate("q", x, y, z)
    problem = Problem(
        "simple_problem",
        domain_name="simple_domain",
        objects=[o1, o2, o3],
        init={p, Not(q)},
        goal=p & q,
    )
    assert problem


def test_build_problem_with_metric():
    """Test a PDDL problem with metric."""
    x, y = variables("x y")
    o1, o2 = constants("o1 o2")
    p = Predicate("p", x, y)
    q = Predicate("q")
    total_cost = NumericFunction(Symbols.TOTAL_COST.value)
    problem = Problem(
        "simple_problem",
        domain_name="simple_domain",
        objects=[o1, o2],
        init={p, Not(q), FunctionEqualTo(total_cost, 0)},
        goal=p & q,
        metric=Metric([total_cost]),
    )
    assert problem


def test_build_problem_with_metric_list():
    """Test a PDDL problem with two functions and metric."""
    x, y = variables("x y")
    o1, o2 = constants("o1 o2")
    p = Predicate("p", x, y)
    q = Predicate("q")
    cost1 = NumericFunction("cost1", x, y)
    cost2 = NumericFunction("cost2")
    problem = Problem(
        "simple_problem",
        domain_name="simple_domain",
        objects=[o1, o2],
        init={p, Not(q), FunctionEqualTo(cost1, 0), FunctionEqualTo(cost2, 1)},
        goal=p & q & GreaterEqualThan(cost1, 3) & LesserEqualThan(cost2, 10),
        metric=Metric([cost1, cost2], Metric.MAXIMIZE),
    )
    assert problem


def test_build_problem_with_numeric_goal():
    """Test a PDDL problem with numeric fluents in goal."""
    x, y = variables("x y")
    o1, o2 = constants("o1 o2")
    p = Predicate("p", x, y)
    q = Predicate("q")
    cost1 = NumericFunction("cost1", x, y)
    cost2 = NumericFunction("cost2")
    problem = Problem(
        "simple_problem",
        domain_name="simple_domain",
        objects=[o1, o2],
        init={p, Not(q), FunctionEqualTo(cost1, 0), FunctionEqualTo(cost2, 10)},
        goal=p & q & GreaterThan(cost1, 3) & LesserThan(cost2, 10),
    )
    assert problem


def test_problem_check_domain_name_mismatch() -> None:
    """Test domain name check fails when names differ beyond case."""
    domain = Domain("simple_domain")
    problem = Problem("simple_problem", domain_name="SIMPLE_DOMAIN2")

    with pytest.raises(PDDLValidationError, match="Domain names don't match."):
        problem.check(domain)


def test_problem_check_domain_name_case_insensitive_match() -> None:
    """Test domain name check accepts names that only differ by case."""
    domain = Domain("simple_domain")
    problem = Problem("simple_problem", domain_name="SIMPLE_DOMAIN")

    problem.check(domain)


def test_problem_check_domain_requirements_match() -> None:
    """Test problem requirements are subset of a domain's requirements."""
    domain = Domain("simple_domain", requirements=[Requirements.STRIPS, Requirements.EQUALITY])
    problem = Problem("simple_problem", domain_name="simple_domain", requirements=[Requirements.STRIPS])

    problem.check(domain)
