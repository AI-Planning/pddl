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

"""This module contains tests for PDDL actions."""

from pddl.action import Action


class TestActionSimpleInitialization:
    """Test simple action initialization."""

    def setup_method(self):
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
        assert self.action.precondition is None

    def test_effects(self):
        """Test the effects getter."""
        assert self.action.effect is None
