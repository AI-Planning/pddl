# -*- coding: utf-8 -*-
"""This module contains tests for a PDDL domain."""
from pddl.core import Domain


class TestDomainEmpty:
    """Test the empty domain."""

    def setup(self):
        """Set up the tests."""
        self.domain = Domain("empty_domain")

    def test_name(self):
        """Test the name getter."""
        assert self.domain.name == "empty_domain"

    def test_requirements(self):
        """Test the requirements getter."""
        assert self.domain.requirements == set()

    def test_constants(self):
        """Test the constants getter."""
        assert self.domain.constants == set()

    def test_predicates(self):
        """Test the predicates getter."""
        assert self.domain.predicates == set()

    def test_actions(self):
        """Test the actions getter."""
        assert self.domain.actions == set()
