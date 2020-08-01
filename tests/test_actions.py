# -*- coding: utf-8 -*-

"""This module contains tests for PDDL actions."""

from pddl.core import Action
from pddl.logic.base import TrueFormula


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

    def test_precondition(self):
        """Test the precondition getter."""
        assert self.action.precondition == TrueFormula()

    def test_effects(self):
        """Test the effects getter."""
        assert self.action.effects == set()
