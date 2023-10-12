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
"""This module contains tests for a PDDL function."""

from pddl.logic import variables
from pddl.logic.functions import NumericFunction, NumericValue


class TestFunctionEmpty:
    """Test the empty function."""

    def setup_method(self):
        """Set up the tests."""
        self.function = NumericFunction("empty_function")

    def test_name(self):
        """Test the name getter."""
        assert self.function.name == "empty_function"

    def test_terms(self):
        """Test the parameters getter."""
        assert self.function.terms == ()

    def test_arity(self):
        """Test the arity getter."""
        assert self.function.arity == 0


def test_build_simple_function():
    """Test a simple PDDL action."""
    x, y, z = variables("x y z", types=["type1"])
    function = NumericFunction("simple_function", x, y, z)
    assert function


class TestNumericValue:
    """Test the numeric value."""

    def setup_method(self):
        """Set up the tests."""
        self.numeric_value = NumericValue(3)

    def test_value(self):
        """Test the name getter."""
        assert self.numeric_value.value == 3
