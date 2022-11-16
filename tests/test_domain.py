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

"""This module contains tests for a PDDL domain."""
from pddl.core import Action, Domain
from pddl.logic.base import Not
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate


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


def test_build_simple_domain():
    """Test a simple PDDL domain."""
    a, b, c = constants("a b c")
    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    action_1 = Action("action_1", [x, y, z], precondition=p, effect=Not(p))
    domain = Domain(
        "simple_domain", constants={a, b, c}, predicates={p}, actions={action_1}
    )

    assert domain
