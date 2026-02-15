#
# Copyright 2021-2025 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""This module contains tests for a PDDL plan."""

import pytest

from pddl import parse_domain
from pddl.core import Plan
from pddl.logic.helpers import constants
from tests.conftest import DOMAIN_FILES


class TestPlanEmpty:
    """Test the empty plan."""

    def setup_method(self):
        """Set up the tests."""
        self.plan = Plan(actions=[])

    def test_str(self):
        """Test the name getter."""
        assert str(self.plan) == ""

    def test_actions(self):
        """Test the actions getter."""
        assert self.plan.actions == []


def test_build_simple_plan():
    """Test a simple PDDL plan."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])

    assert plan


def test_plan_equality():
    """Test plan equality."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan1 = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])
    plan2 = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])
    plan3 = Plan(actions=[(a1_name, [a]), (a2_name, [b, c])])

    assert plan1 == plan2
    assert plan1 != plan3


def test_bad_plan_and_domain():
    """Test instantiation error handling."""
    a, b, c = constants("a b c")
    a1_name = "action_1"
    a2_name = "action_2"
    plan = Plan(actions=[(a1_name, [a]), (a2_name, [b, c]), (a1_name, [c])])
    domain = parse_domain(DOMAIN_FILES[0])

    with pytest.raises(ValueError):
        plan.instantiate(domain)
