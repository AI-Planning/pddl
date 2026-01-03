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
from calendar import c

import pytest

from pddl import parse_domain, parse_plan, parse_problem
from pddl.action import Action
from pddl.logic.base import ExistsCondition
from pddl.logic.functions import Metric, NumericFunction, Times
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import DerivedPredicate, EqualTo, Predicate
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


def test_partially_instantiate_predicate():
    """Test partially instantiating a predicate."""
    a, b = constants("a b")
    x, y, z = variables("x y z")
    p = Predicate("on", x, y, z)
    mapping = {x: a, z: b}
    p_instantiated = p.instantiate(mapping)

    p_expected = Predicate("on", a, y, b)
    assert p_instantiated == p_expected


def test_instantiate_partially_instantiated_predicate():
    """Test instantiating a partially instantiated predicate."""
    a, b = constants("a b")
    x, y = variables("x y")
    p = Predicate("on", x, y)
    mapping1 = {x: a}
    p_partially_instantiated = p.instantiate(mapping1)

    mapping2 = {y: b}
    p_fully_instantiated = p_partially_instantiated.instantiate(mapping2)

    p_expected = Predicate("on", a, b)
    assert p_fully_instantiated == p_expected


def test_partially_instantiate_function():
    """Test partially instantiating a function."""
    a, b = constants("a b")
    x, y, z = variables("x y z")
    f = NumericFunction("distance", x, y, z)
    mapping = {x: a}
    f_instantiated = f.instantiate(mapping)

    f_expected = NumericFunction("distance", a, y, z)
    assert f_instantiated == f_expected


def test_instantiate_partially_instantiated_function():
    """Test instantiating a partially instantiated function."""
    a, b = constants("a b")
    x, y = variables("x y")
    f = NumericFunction("distance", x, y)
    mapping1 = {x: a}
    f_partially_instantiated = f.instantiate(mapping1)

    mapping2 = {y: b}
    f_fully_instantiated = f_partially_instantiated.instantiate(mapping2)

    f_expected = NumericFunction("distance", a, b)
    assert f_fully_instantiated == f_expected


def test_instantiate_equalto_predicate():
    """Test instantiating an EqualTo predicate."""
    x, y = variables("x y")
    a, b = constants("a b")
    eq = EqualTo(x, y)
    mapping = {x: a, y: b}
    eq_instantiated = eq.instantiate(mapping)

    eq_expected = EqualTo(a, b)
    assert eq_instantiated == eq_expected


def test_partially_instantiate_equalto_predicate():
    """Test instantiating an EqualTo predicate."""
    x, y = variables("x y")
    a, b = constants("a b")
    eq = EqualTo(x, y)
    mapping = {x: a}
    eq_instantiated = eq.instantiate(mapping)

    eq_expected = EqualTo(a, y)
    assert eq_instantiated == eq_expected

    mapping2 = {y: b}
    eq_fully_instantiated = eq_instantiated.instantiate(mapping2)

    eq_expected = EqualTo(a, b)
    assert eq_fully_instantiated == eq_expected


def test_instantiate_derived_predicate():
    """Test instantiating a derived predicate."""
    a, b = constants("a b")
    x, y, z = variables("x y z")

    p = Predicate("above", x, y)
    condition = ExistsCondition(
        variables={z}, cond=Predicate("above", x, z) & Predicate("above", z, y)
    )
    dp = DerivedPredicate(predicate=p, condition=condition)

    mapping = {x: a, y: b}
    dp_instantiated = dp.instantiate(mapping)

    p_expected = Predicate("above", a, b)
    condition_expected = ExistsCondition(
        variables={z}, cond=Predicate("above", a, z) & Predicate("above", z, b)
    )
    dp_expected = DerivedPredicate(predicate=p_expected, condition=condition_expected)

    assert dp_instantiated == dp_expected


def test_bad_action_instantiation_parameters():
    """Test action instantiation error handling."""
    a, b, c = constants("a b c")
    x, y = variables("x y")
    action = Action(
        name="move",
        parameters=[x, y],
        precondition=Predicate("at", x),
        effect=Predicate("at", y),
    )

    with pytest.raises(ValueError):
        action.instantiate([a])  # Missing one parameter

    with pytest.raises(ValueError):
        action.instantiate([a, b, c])  # One extra parameter


def test_instantiate_metric():
    """Test instantiating a metric."""
    a, b, c = constants("a b c")
    expression = Times(NumericFunction("distance", a, b), NumericFunction("cost", c))
    metric = Metric(expression)

    mapping = {}
    metric_instantiated = metric.instantiate(mapping)

    assert metric_instantiated == metric
