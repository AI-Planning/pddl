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
