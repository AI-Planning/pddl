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
"""This module contains tests for PDDL function operators."""
from pddl.logic import variables
from pddl.logic.functions import (
    EqualTo,
    GreaterEqualThan,
    GreaterThan,
    LesserEqualThan,
    LesserThan,
    NumericFunction,
)
from pddl.parser.symbols import Symbols


class TestFunctionEqualTo:
    """Test the function equal to."""

    def setup_method(self):
        """Set up the tests."""
        x, y = variables("x y", types=["type1"])
        z = variables("z", types=["type2"])
        self.function_1 = NumericFunction("function_1", x, y)
        self.function_2 = NumericFunction("function_2", *z)
        self.equal_to = EqualTo(self.function_1, self.function_2)

    def test_function(self):
        """Test the function getter."""
        assert self.equal_to.operands == (self.function_1, self.function_2)

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.equal_to.SYMBOL == Symbols.EQUAL

    def test_str(self):
        """Test the str operator."""
        assert (
            str(self.equal_to)
            == f"({self.equal_to.SYMBOL.value} {self.function_1} {self.function_2})"
        )

    def test_repr(self):
        """Test the repr operator."""
        assert (
            repr(self.equal_to)
            == f"EqualTo(({repr(self.function_1)}, {repr(self.function_2)}))"
        )

    def test_equal(self):
        """Test the equal operator."""
        other = EqualTo(self.function_1, self.function_2)
        assert self.equal_to == other


class TestFunctionLesser:
    """Test the function lesser than."""

    def setup_method(self):
        """Set up the tests."""
        x, y = variables("x y", types=["type1"])
        z = variables("z", types=["type2"])
        self.function_1 = NumericFunction("function_1", x, y)
        self.function_2 = NumericFunction("function_2", *z)
        self.lesser = LesserThan(self.function_1, self.function_2)

    def test_function(self):
        """Test the function getter."""
        assert self.lesser.operands == (self.function_1, self.function_2)

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.lesser.SYMBOL == Symbols.LESSER

    def test_str(self):
        """Test the str operator."""
        assert (
            str(self.lesser)
            == f"({self.lesser.SYMBOL.value} {self.function_1} {self.function_2})"
        )

    def test_repr(self):
        """Test the repr operator."""
        assert (
            repr(self.lesser)
            == f"LesserThan(({repr(self.function_1)}, {repr(self.function_2)}))"
        )

    def test_equal(self):
        """Test the equal operator."""
        other = LesserThan(self.function_1, self.function_2)
        assert self.lesser == other


class TestFunctionLesserOrEqual:
    """Test the function lesser or equal than."""

    def setup_method(self):
        """Set up the tests."""
        x, y = variables("x y", types=["type1"])
        z = variables("z", types=["type2"])
        self.function_1 = NumericFunction("function_1", x, y)
        self.function_2 = NumericFunction("function_2", *z)
        self.lesser_or_equal = LesserEqualThan(self.function_1, self.function_2)

    def test_function(self):
        """Test the function getter."""
        assert self.lesser_or_equal.operands == (self.function_1, self.function_2)

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.lesser_or_equal.SYMBOL == Symbols.LESSER_EQUAL

    def test_str(self):
        """Test the str operator."""
        assert (
            str(self.lesser_or_equal)
            == f"({self.lesser_or_equal.SYMBOL.value} {self.function_1} {self.function_2})"
        )

    def test_repr(self):
        """Test the repr operator."""
        assert (
            repr(self.lesser_or_equal)
            == f"LesserEqualThan(({repr(self.function_1)}, {repr(self.function_2)}))"
        )

    def test_equal(self):
        """Test the equal operator."""
        other = LesserEqualThan(self.function_1, self.function_2)
        assert self.lesser_or_equal == other


class TestFunctionGreater:
    """Test the function greater than."""

    def setup_method(self):
        """Set up the tests."""
        x, y = variables("x y", types=["type1"])
        z = variables("z", types=["type2"])
        self.function_1 = NumericFunction("function_1", x, y)
        self.function_2 = NumericFunction("function_2", *z)
        self.greater = GreaterThan(self.function_1, self.function_2)

    def test_function(self):
        """Test the function getter."""
        assert self.greater.operands == (self.function_1, self.function_2)

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.greater.SYMBOL == Symbols.GREATER

    def test_str(self):
        """Test the str operator."""
        assert (
            str(self.greater)
            == f"({self.greater.SYMBOL.value} {self.function_1} {self.function_2})"
        )

    def test_repr(self):
        """Test the repr operator."""
        assert (
            repr(self.greater)
            == f"GreaterThan(({repr(self.function_1)}, {repr(self.function_2)}))"
        )

    def test_equal(self):
        """Test the equal operator."""
        other = GreaterThan(self.function_1, self.function_2)
        assert self.greater == other


class TestFunctionGreaterOrEqual:
    """Test the function greater or equal than."""

    def setup_method(self):
        """Set up the tests."""
        x, y = variables("x y", types=["type1"])
        z = variables("z", types=["type2"])
        self.function_1 = NumericFunction("function_1", x, y)
        self.function_2 = NumericFunction("function_2", *z)
        self.greater_or_equal = GreaterEqualThan(self.function_1, self.function_2)

    def test_function(self):
        """Test the function getter."""
        assert self.greater_or_equal.operands == (self.function_1, self.function_2)

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.greater_or_equal.SYMBOL == Symbols.GREATER_EQUAL

    def test_str(self):
        """Test the str operator."""
        assert (
            str(self.greater_or_equal)
            == f"({self.greater_or_equal.SYMBOL.value} {self.function_1} {self.function_2})"
        )

    def test_repr(self):
        """Test the repr operator."""
        assert (
            repr(self.greater_or_equal)
            == f"GreaterEqualThan(({repr(self.function_1)}, {repr(self.function_2)}))"
        )

    def test_equal(self):
        """Test the equal operator."""
        other = GreaterEqualThan(self.function_1, self.function_2)
        assert self.greater_or_equal == other
