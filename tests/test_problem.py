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

"""This module contains tests for a PDDL problem."""
import copy
import pickle  # nosec

import pytest

from pddl.core import Domain, Problem
from pddl.logic.base import And, Not
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate
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

    def setup(self):
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
