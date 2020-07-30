# -*- coding: utf-8 -*-

"""This module contains tests for PDDL predicates."""
from pddl.core import Predicate


class TestPredicateSimpleInitialisation:
    """Test simple predicate initialisation."""

    def setup(self):
        """Set up the tests."""
        self.predicate = Predicate("P", ["a", "b"])

    def test_name(self):
        """Test name getter."""
        assert self.predicate.name == "P"

    def test_variables(self):
        """Test variables getter."""
        assert self.predicate.variables

    def test_arity(self):
        """Test arity property."""
        assert self.predicate.arity == 2
