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
