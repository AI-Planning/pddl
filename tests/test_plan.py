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

"""This module contains tests for a PDDL plan."""

import pytest

from pddl.action import Action
from pddl.core import Domain, Plan, Problem
from pddl.exceptions import PDDLValidationError
from pddl.logic.base import Not
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate


class TestPlanEmpty:
    """Test the empty plan."""

    def setup_method(self):
        """Set up the tests."""
        self.plan = Plan(actions=[])

    def test_str(self):
        """Test the name getter."""
        assert str(self.plan) == ""

    def test_actions(self):
        """Test the actions getter."""
        assert self.plan.actions == []


def test_build_simple_plan():
    """Test a simple PDDL plan."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])

    assert plan


def test_plan_equality():
    """Test plan equality."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan1 = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])
    plan2 = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])
    plan3 = Plan(actions=[(a1_name, [a]), (a2_name, [b, c])])

    assert plan1 == plan2
    assert plan1 != plan3


def test_bad_plan_action_name():
    """Test validator detects plan actions with invalid names."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    plan = Plan(actions=[(a1_name, [a, b]), (a1_name, [c, b])])

    x, y = variables("x y")
    p = Predicate("p", x, y)
    action_1 = Action("action_a", [x, y], precondition=p, effect=Not(p))
    domain = Domain("simple_domain", predicates={p}, actions={action_1})
    problem = Problem("simple_problem", domain, objects={a, b, c})

    with pytest.raises(
        PDDLValidationError,
        match="Action schema name of .* not found in domain.",
    ):
        plan.check(domain, problem)


def test_bad_plan_action_instantiation():
    """Test validator detects plan actions with invalid number of parameters."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan = Plan(actions=[(a1_name, [a]), (a2_name, [b, c, c]), (a1_name, [c, b, a])])

    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    action_1 = Action("action_1", [x, y, z], precondition=p, effect=Not(p))
    action_2 = Action("action_2", [x, y, z], precondition=Not(p), effect=p)
    domain = Domain("simple_domain", predicates={p}, actions={action_1, action_2})
    problem = Problem("simple_problem", domain, objects={a, b, c})

    with pytest.raises(
        PDDLValidationError,
        match="Number of parameters of .* does not match with the action schema in the domain.",
    ):
        plan.check(domain, problem)


def test_bad_plan_objects():
    """Test validator detects bad objects in plan."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan = Plan(actions=[(a1_name, [a, b, c]), (a2_name, [b, c, a])])

    a, b = constants("a b")
    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    action_1 = Action("action_1", [x, y, z], precondition=p, effect=Not(p))
    action_2 = Action("action_2", [x, y, z], precondition=Not(p), effect=p)
    domain = Domain(
        "simple_domain", constants={a}, predicates={p}, actions={action_1, action_2}
    )

    d, e, f = constants("d e f")
    problem = Problem("simple_problem", domain, objects={d, e, f})

    with pytest.raises(
        PDDLValidationError,
        match="Some parameters of .* are not in the problem objects.",
    ):
        plan.check(domain, problem)
