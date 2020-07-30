# -*- coding: utf-8 -*-

"""This module contains tests for PDDL actions."""

from pddl.core import Action


class TestActionSimpleInitialization:
    """Test simple action initialization."""

    def setup(self):
        """Set up the tests."""
        self.action = Action("action", [])

    def test_name(self):
        """Test the name getter."""
        assert self.action.name == "action"

    def test_parameters(self):
        """Test the parameters getter."""
        assert self.action.parameters == tuple()

    def test_preconditions(self):
        """Test the preconditions getter."""
        assert self.action.preconditions == set()

    def test_effects(self):
        """Test the effects getter."""
        assert self.action.effects == set()
