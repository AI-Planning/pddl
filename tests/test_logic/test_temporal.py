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

"""Test pddl.logic.temporal module."""

import pytest

from pddl.logic import Constant, Predicate, Variable
from pddl.logic.base import Not
from pddl.logic.functions import DurationVariable, NumericFunction, NumericValue
from pddl.logic.temporal import (
    Duration,
    SimpleDurationConstraint,
    TimedCondition,
    TimedEffect,
    TimedInitialLiteral,
    TimeSpecifier,
)
from pddl.parser.symbols import Symbols


def test_time_specifier_str() -> None:
    """Test the string representation of time specifiers."""
    assert str(TimeSpecifier.START) == "start"
    assert str(TimeSpecifier.END) == "end"
    assert str(TimeSpecifier.OVER_ALL) == "over all"


def test_timed_condition_str() -> None:
    """Test the string representation of timed conditions."""
    p = Predicate("p")
    assert str(TimedCondition(TimeSpecifier.START, p)) == "(at start (p))"
    assert str(TimedCondition(TimeSpecifier.END, p)) == "(at end (p))"
    assert str(TimedCondition(TimeSpecifier.OVER_ALL, p)) == "(over all (p))"


def test_timed_condition_equality_and_hash() -> None:
    """Test equality and hashing of timed conditions."""
    p = Predicate("p")
    q = Predicate("q")
    assert TimedCondition(TimeSpecifier.START, p) == TimedCondition(
        TimeSpecifier.START, p
    )
    assert TimedCondition(TimeSpecifier.START, p) != TimedCondition(
        TimeSpecifier.END, p
    )
    assert TimedCondition(TimeSpecifier.START, p) != TimedCondition(
        TimeSpecifier.START, q
    )
    assert hash(TimedCondition(TimeSpecifier.START, p)) == hash(
        TimedCondition(TimeSpecifier.START, p)
    )


def test_timed_condition_instantiate() -> None:
    """Test instantiation of a time condition."""
    a = Variable("a")
    b = Constant("b")
    timed_condition = TimedCondition(TimeSpecifier.START, Predicate("p", a))
    assert timed_condition.instantiate({a: b}) == TimedCondition(
        TimeSpecifier.START, Predicate("p", b)
    )


def test_timed_effect_str() -> None:
    """Test the string representation of timed effects."""
    p = Predicate("p")
    assert str(TimedEffect(TimeSpecifier.START, p)) == "(at start (p))"
    assert str(TimedEffect(TimeSpecifier.END, Not(p))) == "(at end (not (p)))"


def test_timed_effect_over_all_not_allowed() -> None:
    """Test that 'over all' is not a valid time specifier for effects."""
    with pytest.raises(AssertionError):
        TimedEffect(TimeSpecifier.OVER_ALL, Predicate("p"))


def test_timed_effect_equality_and_hash() -> None:
    """Test equality and hashing of timed effects."""
    p = Predicate("p")
    assert TimedEffect(TimeSpecifier.START, p) == TimedEffect(TimeSpecifier.START, p)
    assert TimedEffect(TimeSpecifier.START, p) != TimedEffect(TimeSpecifier.END, p)
    assert hash(TimedEffect(TimeSpecifier.START, p)) == hash(
        TimedEffect(TimeSpecifier.START, p)
    )


def test_simple_duration_constraint_str() -> None:
    """Test the string representation of simple duration constraints."""
    assert (
        str(SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1)))
        == "(= ?duration 1)"
    )
    assert (
        str(SimpleDurationConstraint(Symbols.GREATER_EQUAL.value, NumericValue(1)))
        == "(>= ?duration 1)"
    )
    assert (
        str(SimpleDurationConstraint(Symbols.LESSER_EQUAL.value, NumericValue(1)))
        == "(<= ?duration 1)"
    )


def test_simple_duration_constraint_invalid_operator() -> None:
    """Test that an invalid duration operator is rejected."""
    with pytest.raises(AssertionError):
        SimpleDurationConstraint(Symbols.LESSER.value, NumericValue(1))
    with pytest.raises(AssertionError):
        SimpleDurationConstraint(Symbols.GREATER.value, NumericValue(1))


def test_simple_duration_constraint_instantiate() -> None:
    """Test instantiation of a simple duration constraint."""
    a = Variable("a")
    b = Constant("b")
    constraint = SimpleDurationConstraint(Symbols.EQUAL.value, NumericFunction("f", a))
    assert constraint.instantiate({a: b}) == SimpleDurationConstraint(
        Symbols.EQUAL.value, NumericFunction("f", b)
    )


def test_simple_duration_constraint_time_specifier_str() -> None:
    """Test the string representation of a time-specified duration constraint."""
    start = SimpleDurationConstraint(
        Symbols.GREATER_EQUAL.value, NumericValue(1), time_specifier=TimeSpecifier.START
    )
    assert str(start) == "(at start (>= ?duration 1))"
    end = SimpleDurationConstraint(
        Symbols.LESSER_EQUAL.value, NumericValue(10), time_specifier=TimeSpecifier.END
    )
    assert str(end) == "(at end (<= ?duration 10))"


def test_simple_duration_constraint_time_specifier_over_all_not_allowed() -> None:
    """Test that 'over all' is not a valid time specifier for duration constraints."""
    with pytest.raises(AssertionError):
        SimpleDurationConstraint(
            Symbols.EQUAL.value, NumericValue(1), time_specifier=TimeSpecifier.OVER_ALL
        )


def test_simple_duration_constraint_time_specifier_equality_and_hash() -> None:
    """Test equality and hashing of time-specified duration constraints."""
    c1 = SimpleDurationConstraint(
        Symbols.EQUAL.value, NumericValue(1), time_specifier=TimeSpecifier.START
    )
    c2 = SimpleDurationConstraint(
        Symbols.EQUAL.value, NumericValue(1), time_specifier=TimeSpecifier.START
    )
    c3 = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1))
    assert c1 == c2
    assert hash(c1) == hash(c2)
    assert c1 != c3


def test_simple_duration_constraint_time_specifier_instantiate() -> None:
    """Test instantiation of a time-specified duration constraint."""
    a = Variable("a")
    b = Constant("b")
    constraint = SimpleDurationConstraint(
        Symbols.EQUAL.value, NumericFunction("f", a), time_specifier=TimeSpecifier.START
    )
    assert constraint.instantiate({a: b}) == SimpleDurationConstraint(
        Symbols.EQUAL.value, NumericFunction("f", b), time_specifier=TimeSpecifier.START
    )


def test_duration_str() -> None:
    """Test the string representation of durations."""
    c1 = SimpleDurationConstraint(Symbols.GREATER_EQUAL.value, NumericValue(1))
    c2 = SimpleDurationConstraint(Symbols.LESSER_EQUAL.value, NumericValue(10))
    assert str(Duration()) == "()"
    assert str(Duration(c1)) == "(>= ?duration 1)"
    assert str(Duration(c1, c2)) == "(and (>= ?duration 1) (<= ?duration 10))"


def test_duration_equality_and_hash() -> None:
    """Test equality and hashing of durations."""
    c1 = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1))
    c2 = SimpleDurationConstraint(Symbols.GREATER_EQUAL.value, NumericValue(5))
    assert Duration(c1) == Duration(c1)
    assert Duration(c1) != Duration(c2)
    assert hash(Duration(c1)) == hash(Duration(c1))


def test_duration_instantiate() -> None:
    """Test instantiation of a duration."""
    a = Variable("a")
    b = Constant("b")
    constraint = SimpleDurationConstraint(Symbols.EQUAL.value, NumericFunction("f", a))
    assert Duration(constraint).instantiate({a: b}) == Duration(
        SimpleDurationConstraint(Symbols.EQUAL.value, NumericFunction("f", b))
    )


def test_timed_initial_literal_str() -> None:
    """Test the string representation of timed initial literals."""
    assert str(TimedInitialLiteral(5, Predicate("p"))) == "(at 5 (p))"
    assert str(TimedInitialLiteral(2.5, Not(Predicate("p")))) == "(at 2.5 (not (p)))"


def test_timed_initial_literal_equality_and_hash() -> None:
    """Test equality and hashing of timed initial literals."""
    p = Predicate("p")
    assert TimedInitialLiteral(5, p) == TimedInitialLiteral(5, p)
    assert TimedInitialLiteral(5, p) != TimedInitialLiteral(1, p)
    assert hash(TimedInitialLiteral(5, p)) == hash(TimedInitialLiteral(5, p))


def test_timed_initial_literal_instantiate() -> None:
    """Test that instantiating a timed initial literal returns itself."""
    til = TimedInitialLiteral(5, Predicate("p"))
    assert til.instantiate({}) is til


def test_duration_variable() -> None:
    """Test the 'duration' variable expression."""
    dv = DurationVariable()
    assert str(dv) == "?duration"
    assert repr(dv) == "DurationVariable()"
    assert dv == DurationVariable()
    assert hash(dv) == hash(DurationVariable())
    assert dv.instantiate({}) is dv


def test_timed_condition_repr() -> None:
    """Test the repr of a timed condition."""
    assert (
        repr(TimedCondition(TimeSpecifier.START, Predicate("p")))
        == "TimedCondition(start, (p))"
    )


def test_timed_condition_ordering() -> None:
    """Test ordering of timed conditions, including the non-matching-type branch."""
    p = Predicate("p")
    assert TimedCondition(TimeSpecifier.START, p) < TimedCondition(
        TimeSpecifier.OVER_ALL, p
    )
    assert TimedCondition(TimeSpecifier.START, p).__lt__(object()) is NotImplemented


def test_timed_effect_repr() -> None:
    """Test the repr of a timed effect."""
    assert (
        repr(TimedEffect(TimeSpecifier.END, Not(Predicate("p"))))
        == "TimedEffect(end, (not (p)))"
    )


def test_timed_effect_ordering() -> None:
    """Test ordering of timed effects, including the non-matching-type branch."""
    p = Predicate("p")
    assert TimedEffect(TimeSpecifier.END, p) < TimedEffect(TimeSpecifier.START, p)
    assert TimedEffect(TimeSpecifier.START, p).__lt__(object()) is NotImplemented


def test_simple_duration_constraint_repr() -> None:
    """Test the repr of simple duration constraints, with and without a time specifier."""
    c = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1))
    assert repr(c) == "SimpleDurationConstraint(=, 1)"
    c_start = SimpleDurationConstraint(
        Symbols.EQUAL.value, NumericValue(1), time_specifier=TimeSpecifier.START
    )
    assert repr(c_start) == "SimpleDurationConstraint(=, 1, time_specifier=start)"


def test_simple_duration_constraint_ordering() -> None:
    """Test ordering of simple duration constraints."""
    c1 = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1))
    c2 = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(2))
    assert c1 < c2
    assert c1.__lt__(object()) is NotImplemented


def test_duration_repr() -> None:
    """Test the repr of a duration."""
    c = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1))
    assert repr(Duration(c)) == "Duration(SimpleDurationConstraint(=, 1))"


def test_duration_ordering() -> None:
    """Test ordering of durations."""
    c1 = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(1))
    c2 = SimpleDurationConstraint(Symbols.EQUAL.value, NumericValue(2))
    assert Duration(c1) < Duration(c2)
    assert Duration(c1).__lt__(object()) is NotImplemented


def test_timed_initial_literal_repr() -> None:
    """Test the repr of a timed initial literal."""
    assert repr(TimedInitialLiteral(5, Predicate("p"))) == "TimedInitialLiteral(5, (p))"


def test_timed_initial_literal_ordering() -> None:
    """Test ordering of timed initial literals."""
    p = Predicate("p")
    assert TimedInitialLiteral(1, p) < TimedInitialLiteral(5, p)
    assert TimedInitialLiteral(5, p).__lt__(object()) is NotImplemented
