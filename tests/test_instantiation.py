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

"""This module contains tests for action instantiation."""
import os

import pytest

from pddl import parse_domain, parse_plan, parse_problem
from pddl.action import Action
from tests.conftest import PLAN_FILES


@pytest.mark.parametrize("plan_file", PLAN_FILES)
def test_parse_instantiate_plans(plan_file):
    """Test parsing and instantiating plans from PDDL files."""
    domain_file = os.path.dirname(plan_file) + "/domain.pddl"
    problem_name = os.path.basename(plan_file).split(".")[0]
    problem_file = os.path.dirname(plan_file) + f"/{problem_name}.pddl"

    domain = parse_domain(domain_file)
    problem = parse_problem(problem_file)
    plan = parse_plan(plan_file)

    assert domain
    assert problem
    assert plan

    for action in plan.instantiate(domain):
        assert isinstance(action, Action)
        assert "?" not in str(action)
