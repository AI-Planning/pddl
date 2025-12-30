#
# Author: Dillon Z. Chen
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
from pddl.core import Plan
from pddl.logic.helpers import constants, variables


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
    x, y, z = variables("x y z")
    a1_name = "action_1"
    a2_name = "action_2"
    plan = Plan(
        actions=[
            (a1_name, [a, x]),
            (a2_name, [b, c, y, z]),
            (a1_name, [c, x]),
            (a2_name, [z, y, b, z]),
        ]
    )

    assert plan
