# -*- coding: utf-8 -*-
#
# Copyright 2021-2022 WhiteMech
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#
"""This module contains tests for a PDDL action."""

from pddl.core import Action
from pddl.logic import Predicate, Variable, variables
from pddl.logic.base import Imply, OneOf


class TestActionEmpty:
    """Test the empty action."""

    def setup(self):
        """Set up the tests."""
        self.action = Action("empty_action", [])

    def test_name(self):
        """Test the name getter."""
        assert self.action.name == "empty_action"

    def test_parameters(self):
        """Test the parameters getter."""
        assert self.action.parameters == ()

    def test_precondition(self):
        """Test the precondition getter."""
        assert self.action.precondition is None

    def test_effect(self):
        """Test the effect getter."""
        assert self.action.effect is None


def test_build_simple_action():
    """Test a simple PDDL action."""
    x, y, z = variables("x y z", types=["type1"])
    p = Predicate("p", x, y, z)
    q = Predicate("q", x, y, z)
    action = Action(
        "simple_action",
        parameters=(x, y, z),
        precondition=p & ~q,
        effect=~p & q,
    )
    assert action


def test_build_action_1():
    """Test PDDL action 1."""
    x, y = variables("x y", types=["type1"])
    z = Variable("z", type_tags={"type2"})
    p = Predicate("p", x, y)
    q = Predicate("q", z)
    action = Action(
        "action_1",
        parameters=(x, y, z),
        precondition=p,
        effect=OneOf(p & ~q, p & q),
    )
    assert action


def test_build_action_2():
    """Test PDDL action 2."""
    x, y = variables("x y", types=["type1"])
    z, w = variables("z w", types=["type2"])
    p = Predicate("p", x, y)
    q = Predicate("q", z)
    r = Predicate("r", w)
    action = Action(
        "action_1",
        parameters=(x, y, z, w),
        precondition=Imply(p & q, r),
        effect=OneOf(p & ~q, p & q),
    )
    assert action
