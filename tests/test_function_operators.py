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
from pddl.parser.symbols import Symbols

from pddl.logic import variables
from pddl.logic.functions import Function, FunctionOperator


class TestFunctionOperators:
    """Test the function operators."""

    def setup_method(self):
        """Set up the tests."""
        x, y = variables("x y", types=["type1"])
        z = variables("z", types=["type2"])
        self.function = Function("function_1")
        self.function_op = FunctionOperator(
            self.function,
            3,
            Symbols.EQUAL
        )

    def test_function(self):
        """Test the function getter."""
        assert self.function_op.function == self.function

    def test_symbol(self):
        """Test the symbol getter."""
        assert self.function_op.symbol == Symbols.EQUAL.value

    def test_value(self):
        """Test the value getter."""
        assert self.function_op.value == 3

    def test_equal(self):
        """Test the equal operator."""
        other = FunctionOperator(
            self.function,
            3,
            Symbols.EQUAL.value
        )
        assert self.function_op == other

    def test_str(self):
        """Test the str operator."""
        assert str(self.function_op) == f"({self.function_op.symbol.value} {self.function} {self.function_op.value})"

    def test_repr(self):
        """Test the repr operator."""
        assert repr(self.function_op) == f"FunctionOperator({self.function}, {self.function_op.value})"
