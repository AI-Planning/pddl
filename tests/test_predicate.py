# -*- coding: utf-8 -*-
#
# Copyright 2021-2022 WhiteMech
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
