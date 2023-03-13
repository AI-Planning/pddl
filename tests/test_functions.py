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

"""This module contains tests for PDDL functions."""
from pddl.core import Function
from pddl.logic.helpers import variables


class TestFunctionsimpleInitialisation:
    """Test simple function initialisation."""

    def setup_method(self):
        """Set up the tests."""
        self.a, self.b = variables("a b")
        self.function = Function("P", self.a, self.b)

    def test_name(self):
        """Test name getter."""
        assert self.function.name == "P"

    def test_variables(self):
        """Test terms getter."""
        assert self.function.terms == (self.a, self.b)

    def test_arity(self):
        """Test arity property."""
        assert self.function.arity == 2
