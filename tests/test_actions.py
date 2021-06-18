# -*- coding: utf-8 -*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl.  If not, see <https://www.gnu.org/licenses/>.
#

"""This module contains tests for PDDL actions."""

from pddl.core import Action
from pddl.logic.base import FalseFormula


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
        assert self.action.precondition == FalseFormula()

    def test_effects(self):
        """Test the effects getter."""
        assert self.action.effect == FalseFormula()
