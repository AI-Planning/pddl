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

"""This module contains tests for PDDL functions."""

import pytest

from pddl.logic.base import And, Or
from pddl.logic.functions import (
    Divide,
    Metric,
    Minus,
    NumericFunction,
    NumericValue,
    Plus,
    Times,
    UnaryMinus,
)
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate
from pddl.parser.symbols import Symbols


class TestNumericFunction:
    """Test simple numeric function."""

    def setup_method(self):
        """Set up the tests."""
        self.a, self.b = variables("a b", types=["type1"])
        self.c = constants("c")[0]
        self.function = NumericFunction("func", self.a, self.b)
        self.function2 = NumericFunction("func2", self.a, self.c)

    def test_name(self):
        """Test name getter."""
        assert self.function.name == "func"

    def test_variables(self):
        """Test terms getter."""
        assert self.function.terms == (self.a, self.b)

    def test_arity(self):
        """Test arity property."""
        assert self.function.arity == 2

    def test_to_equal(self):
        """Test to equal."""
        other = NumericFunction("func", self.a, self.b)
        assert self.function == other

    def test_to_str(self):
        """Test to string."""
        assert str(self.function) == f"({self.function.name} {self.a} {self.b})"
        assert str(self.function2) == f"({self.function2.name} {self.a} {self.c})"

    def test_to_repr(self):
        """Test to repr."""
        assert (
            repr(self.function)
            == f"NumericFunction({self.function.name}, {self.a}, {self.b})"
        )


class TestTotalCost:
    """Test total cost function."""

    def setup_method(self):
        """Set up the tests."""
        self.total_cost = NumericFunction(Symbols.TOTAL_COST.value)

    def test_name(self):
        """Test name getter."""
        assert self.total_cost.name == "total-cost"


class TestMetric:
    """Test metric."""

    def setup_method(self):
        """Set up the tests."""
        self.a, self.b = variables("a b")
        self.function = NumericFunction("func", self.a, self.b)
        self.maximize_metric = Metric(self.function, Metric.MAXIMIZE)
        self.minimize_metric = Metric(self.function, Metric.MINIMIZE)

    def test_function_maximize(self):
        """Test function getter for maximize metric."""
        assert self.maximize_metric.expression == self.function

    def test_function_minimize(self):
        """Test function getter for minimize metric."""
        assert self.minimize_metric.expression == self.function

    def test_optimization_maximize(self):
        """Test optimization getter for maximize metric."""
        assert self.maximize_metric.optimization == Metric.MAXIMIZE

    def test_optimization_minimize(self):
        """Test optimization getter for minimize metric."""
        assert self.minimize_metric.optimization == Metric.MINIMIZE

    def test_wrong_optimization(self):
        """Test wrong optimization."""
        with pytest.raises(
            AssertionError,
            match="Optimization metric not recognized.",
        ):
            Metric(self.function, "other")

    def test_to_equal(self):
        """Test to equal."""
        other = Metric(NumericFunction("func", self.a, self.b), Metric.MINIMIZE)
        assert self.minimize_metric == other

    def test_to_str(self):
        """Test to string."""
        assert (
            str(self.maximize_metric)
            == f"{self.maximize_metric.optimization} {self.maximize_metric.expression}"
        )

    def test_to_repr(self):
        """Test to repr."""
        assert (
            repr(self.maximize_metric)
            == f"Metric({self.maximize_metric.expression}, {self.maximize_metric.optimization})"
        )


class TestNumericValue:
    """Test the numeric value."""

    def setup_method(self):
        """Set up the tests."""
        self.numeric_value = NumericValue(3)

    def test_value(self):
        """Test the name getter."""
        assert self.numeric_value.value == 3


class TestUnaryMinus:
    """Test the unary minus operator."""

    def setup_method(self):
        """Set up the tests."""
        self.function = NumericFunction("func")
        self.unary_minus = UnaryMinus(self.function)

    def test_operand(self):
        """Test the operand getter."""
        assert self.unary_minus.operand == self.function

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.unary_minus.SYMBOL == Symbols.MINUS

    def test_to_equal(self):
        """Test the equal operator."""
        other = UnaryMinus(NumericFunction("func"))
        assert self.unary_minus == other

    def test_not_equal_to_minus(self):
        """Test that UnaryMinus is distinct from binary Minus."""
        binary_minus = Minus(self.function, NumericFunction("other"))
        assert self.unary_minus != binary_minus

    def test_to_str(self):
        """Test the str operator."""
        assert str(self.unary_minus) == f"({Symbols.MINUS.value} {self.function})"

    def test_to_repr(self):
        """Test the repr operator."""
        assert repr(self.unary_minus) == f"UnaryMinus({repr(self.function)})"

    def test_hash(self):
        """Test the hash operator."""
        assert hash(self.unary_minus) == hash(UnaryMinus(NumericFunction("func")))

    def test_instantiate(self):
        """Test the instantiate method."""
        x = variables("x")[0]
        a = constants("a")[0]
        unary_minus = UnaryMinus(NumericFunction("func", x))
        assert unary_minus.instantiate({x: a}) == UnaryMinus(NumericFunction("func", a))


class TestNumericOperatorStructure:
    """Test that numeric operators preserve operand structure (no flattening/dedup)."""

    def setup_method(self):
        """Set up the tests."""
        self.f = NumericFunction("f")
        self.g = NumericFunction("g")

    def test_right_nested_minus_preserved(self):
        """Right-nested binary minus keeps its tree structure."""
        expr = Minus(self.f, Minus(self.f, NumericValue(2)))
        assert str(expr) == "(- (f) (- (f) 2))"
        assert isinstance(expr.operands[0], NumericFunction)
        assert isinstance(expr.operands[1], Minus)
        assert isinstance(expr.operands[1].operands[0], NumericFunction)
        assert isinstance(expr.operands[1].operands[1], NumericValue)
        assert expr.operands[1].operands == (self.f, NumericValue(2))

    def test_left_nested_minus_preserved(self):
        """Left-nested binary minus keeps its tree structure and order."""
        expr = Minus(Minus(NumericValue(2), NumericValue(3)), NumericValue(1))
        assert str(expr) == "(- (- 2 3) 1)"
        assert isinstance(expr.operands[0], Minus)
        assert isinstance(expr.operands[0].operands[0], NumericValue)
        assert isinstance(expr.operands[0].operands[1], NumericValue)
        assert isinstance(expr.operands[1], NumericValue)

    def test_nested_plus_not_flattened(self):
        """Nested plus is not auto-flattened."""
        expr = Plus(Plus(self.f, self.g), self.f)
        assert str(expr) == "(+ (+ (f) (g)) (f))"
        assert isinstance(expr.operands[0], Plus)
        assert isinstance(expr.operands[0].operands[0], NumericFunction)
        assert isinstance(expr.operands[0].operands[1], NumericFunction)
        assert isinstance(expr.operands[1], NumericFunction)

    def test_nary_plus_accepted(self):
        """Plus still accepts more than two operands."""
        expr = Plus(self.f, self.g, self.f)
        assert len(expr.operands) == 3
        assert str(expr) == "(+ (f) (g) (f))"
        assert isinstance(expr, Plus)
        assert isinstance(expr.operands[0], NumericFunction)
        assert isinstance(expr.operands[1], NumericFunction)
        assert isinstance(expr.operands[2], NumericFunction)

    def test_nested_times_preserved(self):
        """Nested times keeps its tree structure."""
        expr = Times(Times(self.f, self.g), self.f)
        assert str(expr) == "(* (* (f) (g)) (f))"
        assert isinstance(expr, Times)
        assert isinstance(expr.operands[0], Times)
        assert isinstance(expr.operands[0].operands[0], NumericFunction)
        assert isinstance(expr.operands[0].operands[1], NumericFunction)
        assert isinstance(expr.operands[1], NumericFunction)

    def test_nested_divide_preserved(self):
        """Nested divide keeps its tree structure."""
        expr = Divide(NumericValue(1), Divide(NumericValue(2), NumericValue(3)))
        assert str(expr) == "(/ 1 (/ 2 3))"
        assert isinstance(expr.operands[1], Divide)
        assert isinstance(expr.operands[0], NumericValue)
        assert isinstance(expr.operands[1].operands[0], NumericValue)
        assert isinstance(expr.operands[1].operands[1], NumericValue)


class TestLogicOperatorSimplification:
    """That that And/Or keep their monotone simplification behaviour."""

    def setup_method(self):
        """Set up the tests."""
        self.p = Predicate("p")
        self.q = Predicate("q")

    def test_and_idempotency(self):
        """And(a, a) collapses to a."""
        assert And(self.p, self.p) == self.p

    def test_or_idempotency(self):
        """Or(a, a) collapses to a."""
        assert Or(self.p, self.p) == self.p

    def test_and_flattening(self):
        """Nested And is flattened (associative)."""
        r = Predicate("f")
        expr = And(self.p, And(self.q, r))
        assert tuple(expr.operands) == (self.p, self.q, r)
