# -*- coding: utf-8 -*-

"""This module contains tests for PDDL predicates."""
from pddl.core import Predicate
from pddl.logic.helpers import variables


class TestPredicateSimpleInitialisation:
    """Test simple predicate initialisation."""

    def setup(self):
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
