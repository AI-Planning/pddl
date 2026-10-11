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

"""This module contains tests for a PDDL durative action."""

import pytest

from pddl.durative_action import DurativeAction
from pddl.logic import Predicate, constants, variables
from pddl.logic.base import Not
from pddl.logic.functions import NumericValue
from pddl.logic.temporal import (
    Duration,
    SimpleDurationConstraint,
    TimedCondition,
    TimedEffect,
    TimeSpecifier,
)
from pddl.parser.symbols import Symbols


class TestDurativeActionEmpty:
    """Test the empty durative action."""

    def setup_method(self):
        """Set up the tests."""
        self.action = DurativeAction("empty_action", [])

    def test_name(self):
        """Test the name getter."""
        assert self.action.name == "empty_action"

    def test_parameters(self):
        """Test the parameters getter."""
        assert self.action.parameters == ()

    def test_duration(self):
        """Test the duration getter."""
        assert self.action.duration is None

    def test_condition(self):
        """Test the condition getter."""
        assert self.action.condition is None

    def test_effect(self):
        """Test the effect getter."""
        assert self.action.effect is None


def test_build_simple_durative_action() -> None:
    """Test building a simple durative action."""
    x = variables("x")[0]
    p = Predicate("p", x)
    action = DurativeAction(
        "move",
        parameters=(x,),
        duration=Duration(
            SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(5))
        ),
        condition=TimedCondition(TimeSpecifier.START, p),
        effect=TimedEffect(TimeSpecifier.END, Not(p) & p),
    )
    assert action.name == "move"
    assert action.parameters == (x,)
    assert action.terms == (x,)
    assert action.duration is not None
    assert action.condition is not None
    assert action.effect is not None


def test_durative_action_str() -> None:
    """Test the string representation of a durative action."""
    x = variables("x")[0]
    p = Predicate("p", x)
    q = Predicate("q", x)
    action = DurativeAction(
        "move",
        parameters=(x,),
        duration=Duration(
            SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(5))
        ),
        condition=TimedCondition(TimeSpecifier.START, p),
        effect=TimedEffect(TimeSpecifier.END, q),
    )
    assert str(action) == (
        "(:durative-action move\n"
        "    :parameters (?x)\n"
        "    :duration (= ?duration 5)\n"
        "    :condition (at start (p ?x))\n"
        "    :effect (at end (q ?x))\n"
        ")"
    )


def test_durative_action_equality_and_hash() -> None:
    """Test equality and hashing of durative actions."""
    x = variables("x")[0]
    p = Predicate("p", x)
    duration = Duration(SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(5)))
    condition = TimedCondition(TimeSpecifier.START, p)
    effect = TimedEffect(TimeSpecifier.END, p)
    a1 = DurativeAction(
        "move", parameters=(x,), duration=duration, condition=condition, effect=effect
    )
    a2 = DurativeAction(
        "move", parameters=(x,), duration=duration, condition=condition, effect=effect
    )
    a3 = DurativeAction(
        "move",
        parameters=(x,),
        duration=duration,
        condition=condition,
        effect=TimedEffect(TimeSpecifier.START, p),
    )
    assert a1 == a2
    assert hash(a1) == hash(a2)
    assert a1 != a3
    assert a1 != "not a durative action"


def test_durative_action_instantiate() -> None:
    """Test instantiation of a durative action."""
    x = variables("x")[0]
    o = constants("o")[0]
    p = Predicate("p", x)
    action = DurativeAction(
        "move",
        parameters=(x,),
        duration=Duration(
            SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(5))
        ),
        condition=TimedCondition(TimeSpecifier.START, p),
        effect=TimedEffect(TimeSpecifier.END, p),
    )
    instantiated = action.instantiate([o])
    assert instantiated.parameters == (o,)
    assert instantiated.condition == TimedCondition(
        TimeSpecifier.START, Predicate("p", o)
    )
    assert instantiated.effect == TimedEffect(TimeSpecifier.END, Predicate("p", o))


def test_durative_action_instantiate_wrong_length() -> None:
    """Test that instantiating with the wrong number of terms raises an error."""
    x = variables("x")[0]
    action = DurativeAction("move", parameters=(x,))
    with pytest.raises(ValueError):
        action.instantiate([])


def test_durative_action_repr() -> None:
    """Test the unambiguous string representation of a durative action."""
    x = variables("x")[0]
    action = DurativeAction("move", parameters=(x,))
    assert repr(action).startswith(
        "DurativeAction(move, parameters=?x, duration=None, condition=None, effect=None)"
    )
