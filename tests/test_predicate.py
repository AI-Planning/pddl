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

"""This module contains tests for PDDL predicates."""
from pddl.core import Predicate
from pddl.logic.base import And
from pddl.logic.helpers import variables
from pddl.logic.predicates import DerivedPredicate, EqualTo


class TestPredicateSimpleInitialisation:
    """Test simple predicate initialisation."""

    def setup_method(self):
        """Set up the tests."""
        self.a, self.b = variables("a b")
        self.predicate = Predicate("P", self.a, self.b)

    def test_name(self):
        """Test name getter."""
        assert self.predicate.name == "P"

    def test_variables(self):
        """Test terms getter."""
        assert self.predicate.terms == (self.a, self.b)

    def test_arity(self):
        """Test arity property."""
        assert self.predicate.arity == 2

    def test_to_equal(self):
        """Test to equal."""
        other = Predicate("P", self.a, self.b)
        assert self.predicate == other

    def test_to_str(self):
        """Test to string."""
        assert str(self.predicate) == f"({self.predicate.name} {self.a} {self.b})"

    def test_to_repr(self):
        """Test to repr."""
        assert (
            repr(self.predicate)
            == f"Predicate({self.predicate.name}, {self.a}, {self.b})"
        )


class TestEqualToPredicate:
    """Test the eaual to predicate."""

    def setup_method(self):
        """Set up the tests."""
        self.left, self.right = variables("l r")
        self.equal_to = EqualTo(self.left, self.right)

    def test_left(self):
        """Test left getter."""
        assert self.equal_to.left == self.left

    def test_right(self):
        """Test right getter."""
        assert self.equal_to.right == self.right

    def test_to_equal(self):
        """Test to equal."""
        other = EqualTo(self.left, self.right)
        assert self.equal_to == other

    def test_to_str(self):
        """Test to string."""
        assert str(self.equal_to) == f"(= {str(self.left)} {str(self.right)})"

    def test_to_repr(self):
        """Test to repr."""
        assert repr(self.equal_to) == f"EqualTo({repr(self.left)}, {repr(self.right)})"


class TestDerivedPredicate:
    """Test the derived predicate."""

    def setup_method(self):
        """Set up the tests."""
        self.a, self.b = variables("a b")
        self.predicate = Predicate("dp", self.a, self.b)
        self.condition = And(self.a, self.b)
        self.derived = DerivedPredicate(self.predicate, self.condition)

    def test_predicate(self):
        """Test predicate getter."""
        assert self.derived.predicate == self.predicate

    def test_condition(self):
        """Test condition getter."""
        assert self.derived.condition == self.condition

    def test_to_equal(self):
        """Test to equal."""
        other = DerivedPredicate(self.predicate, self.condition)
        assert self.derived == other

    def test_to_str(self):
        """Test to string."""
        assert str(self.derived) == f"(:derived {self.predicate} {self.condition})"

    def test_to_repr(self):
        """Test to repr."""
        assert (
            repr(self.derived)
            == f"DerivedPredicate({repr(self.predicate)}, {repr(self.condition)})"
        )
