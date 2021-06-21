# -*- coding: utf-8 -*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl.  If not, see <https://www.gnu.org/licenses/>.
#
"""This module contains tests for a PDDL action."""

from pddl.core import Action
from pddl.logic import Predicate, Variable, variables
from pddl.logic.base import FalseFormula, Imply, OneOf


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
        assert self.action.precondition == FalseFormula()

    def test_effect(self):
        """Test the effect getter."""
        assert self.action.effect == FalseFormula()


def test_build_simple_action():
    """Test a simple PDDL action."""
    x, y, z = variables("x y z", types=["type1"])
    p = Predicate("p", x, y, z)
    q = Predicate("q", x, y, z)
    action = Action(
        "simple_action", parameters=(x, y, z), precondition=p & ~q, effect=~p & q,
    )
    assert action


def test_build_action_1():
    """Test PDDL action 1."""
    x, y = variables("x y", types=["type1"])
    z = Variable("z", type_tags={"type2"})
    p = Predicate("p", x, y)
    q = Predicate("q", z)
    action = Action(
        "action_1", parameters=(x, y, z), precondition=p, effect=OneOf(p & ~q, p & q),
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
