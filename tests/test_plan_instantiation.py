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

"""Tests for the typing of terms produced by Plan.instantiate()."""

import os
from collections import defaultdict

import pytest

from pddl import parse_domain, parse_plan, parse_problem
from pddl.action import Action
from pddl.core import Domain, Plan, Problem
from pddl.logic.base import And, Not
from pddl.logic.effects import Forall
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate
from pddl.logic.terms import Constant, Variable
from pddl.requirements import Requirements
from tests.conftest import BLOCKSWORLD_FILES, PLAN_FILES


def _typed_domain() -> Domain:
    """Build a small typed domain.

    Types: block, special - block, surface.
    Constant: table - surface.
    Actions:
      pick(?x - block)                 pre (clear ?x)          eff (holding ?x)
      put(?x - block)                  pre (holding ?x)        eff (on ?x table)
      push(?x - (either block surface)) pre (clear ?x)         eff (not (clear ?x))
    """
    x = Variable("x", ["block"])
    y = Variable("y", ["surface"])
    e = Variable("x", ["block", "surface"])
    table = Constant("table", "surface")
    clear = Predicate("clear", Variable("x", ["block"]))
    holding = Predicate("holding", Variable("x", ["block"]))
    on = Predicate("on", Variable("x", ["block"]), y)

    pick = Action(
        "pick", [x], precondition=Predicate("clear", x), effect=Predicate("holding", x)
    )
    put = Action(
        "put",
        [x],
        precondition=Predicate("holding", x),
        effect=Predicate("on", x, table),
    )
    push = Action(
        "push",
        [e],
        precondition=Predicate("clear", e),
        effect=Not(Predicate("clear", e)),
    )
    return Domain(
        "typed",
        requirements={Requirements.TYPING},
        types={"block": None, "special": "block", "surface": None},
        constants={table},
        predicates={clear, holding, on},
        actions={pick, put, push},
    )


def _untyped_domain() -> Domain:
    """Build a small domain with no types at all."""
    (x,) = variables("x")
    p = Predicate("p", x)
    act = Action("act", [x], precondition=p, effect=Not(p))
    return Domain("untyped", predicates={p}, actions={act})


def _atoms(formula):
    """Yield the predicates in a conjunction of (possibly negated) predicates."""
    operands = formula.operands if isinstance(formula, And) else [formula]
    for op in operands:
        op = op.argument if isinstance(op, Not) else op
        if isinstance(op, Predicate):
            yield op


# --- arity -----------------------------------------------------------------


@pytest.mark.parametrize("params", ["a b c", "a"])
def test_instantiate_wrong_arity_raises(params):
    """Too many or too few parameters must still raise, not be silently truncated."""
    domain = parse_domain(BLOCKSWORLD_FILES / "domain.pddl")  # pick-up takes 2
    plan = Plan(actions=[("pick-up", list(constants(params)))])
    with pytest.raises(ValueError):
        plan.instantiate(domain)


# --- typing of parameters ----------------------------------------------------


def test_untyped_input_gets_schema_type():
    """Untyped constants (as produced by parse_plan) pick up the schema's type."""
    domain = parse_domain(BLOCKSWORLD_FILES / "domain.pddl")
    b1, b2 = constants("b1 b2")
    (action,) = Plan(actions=[("pick-up", [b1, b2])]).instantiate(domain)
    assert [p.type_tag for p in action.parameters] == ["block", "block"]


def test_untyped_input_still_equal_to_plan_constants():
    """Constant equality ignores type tags, so plan params == instantiated params."""
    domain = parse_domain(BLOCKSWORLD_FILES / "domain.pddl")
    b1, b2 = constants("b1 b2")
    (action,) = Plan(actions=[("pick-up", [b1, b2])]).instantiate(domain)
    assert action.parameters == (b1, b2)
    assert Constant("b1") == Constant("b1", "block")


def test_typed_input_keeps_its_own_type():
    """A constant already typed with a subtype must not be widened to the schema type."""
    domain = _typed_domain()
    s = Constant("s", "special")
    (action,) = Plan(actions=[("pick", [s])]).instantiate(domain)
    assert action.parameters[0].type_tag == "special"


def test_either_typed_parameter_is_untyped():
    """With an `either` parameter there is no single type to assign."""
    domain = _typed_domain()
    (a,) = constants("a")
    (action,) = Plan(actions=[("push", [a])]).instantiate(domain)
    assert action.parameters[0].type_tag is None


def test_untyped_domain_leaves_terms_untyped():
    """No types in the domain: no types on the instantiated terms, none printed."""
    domain = _untyped_domain()
    (a,) = constants("a")
    (action,) = Plan(actions=[("act", [a])]).instantiate(domain)
    assert action.parameters[0].type_tag is None
    assert " - " not in str(action)


# --- typing inside the body --------------------------------------------------


def test_body_terms_are_typed():
    """Terms inside the instantiated precondition and effect carry the type too."""
    domain = _typed_domain()
    (a,) = constants("a")
    (action,) = Plan(actions=[("pick", [a])]).instantiate(domain)
    for atom in [*_atoms(action.precondition), *_atoms(action.effect)]:
        assert [t.type_tag for t in atom.terms] == ["block"]


def test_domain_constant_in_body_keeps_its_type():
    """A domain constant used in an action body keeps its declared type."""
    domain = _typed_domain()
    (a,) = constants("a")
    (action,) = Plan(actions=[("put", [a])]).instantiate(domain)
    (on,) = _atoms(action.effect)
    assert [t.type_tag for t in on.terms] == ["block", "surface"]


# --- printing ----------------------------------------------------------------


def test_instantiated_action_parameters_str():
    """Instantiated parameters print as `name - type`, without a `?`."""
    domain = parse_domain(BLOCKSWORLD_FILES / "domain.pddl")
    b1, b2 = constants("b1 b2")
    (action,) = Plan(actions=[("pick-up", [b1, b2])]).instantiate(domain)
    assert ":parameters (b1 - block b2 - block)" in str(action)


def test_schema_parameters_str_unchanged():
    """Action schemas still print their variables with a `?`."""
    domain = parse_domain(BLOCKSWORLD_FILES / "domain.pddl")
    (schema,) = [a for a in domain.actions if a.name == "pick-up"]
    assert ":parameters (?b1 - block ?b2 - block)" in str(schema)


def test_forall_effect_str_unchanged():
    """Forall shares _typed_parameters; its variables must still print with `?`."""
    x = Variable("x", ["t"])
    effect = Forall(Predicate("p", x), [x])
    assert str(effect) == "(forall (?x - t) (p ?x))"


# --- fixture plans -----------------------------------------------------------


def _load(plan_file):
    directory = os.path.dirname(plan_file)
    problem_name = os.path.basename(plan_file).split(".")[0]
    domain = parse_domain(os.path.join(directory, "domain.pddl"))
    problem = parse_problem(os.path.join(directory, f"{problem_name}.pddl"))
    return domain, problem, parse_plan(plan_file)


@pytest.mark.parametrize("plan_file", PLAN_FILES)
def test_fixture_plans_object_types_are_consistent(plan_file):
    """Each object has one type across the whole instantiated plan.

    Typing from the action schema assigns an object the type of whichever
    parameter it fills, so e.g. barman's `shot4 - shot` comes out as
    `container` in some actions and `shot` in others.
    """
    domain, problem, plan = _load(plan_file)
    seen = defaultdict(set)
    for action in plan.instantiate(domain, problem):
        for term in action.parameters:
            seen[term.name].add(term.type_tag)
    inconsistent = {name: tags for name, tags in seen.items() if len(tags) > 1}
    assert not inconsistent


@pytest.mark.parametrize("plan_file", PLAN_FILES)
def test_fixture_plans_use_declared_object_types(plan_file):
    """With the problem given, each term has the type declared for that object."""
    domain, problem, plan = _load(plan_file)
    declared = {o.name: o.type_tag for o in [*domain.constants, *problem.objects]}
    for action in plan.instantiate(domain, problem):
        for term in action.parameters:
            assert term.type_tag == declared[term.name], f"{term} in {action.name}"


def test_problem_type_beats_schema_type():
    """An untyped plan constant takes its type from the problem, not the schema."""
    domain = _typed_domain()
    s = Constant("s", "special")
    problem = Problem("p", domain, objects={s})
    (action,) = Plan(actions=[("pick", [Constant("s")])]).instantiate(domain, problem)
    assert action.parameters[0].type_tag == "special"


@pytest.mark.parametrize("plan_file", PLAN_FILES)
def test_fixture_plans_parameters_are_typed(plan_file):
    """Wherever the schema parameter has a single type, the instantiated term has one."""
    domain, _, plan = _load(plan_file)
    name_to_schema = {a.name: a for a in domain.actions}
    for action in plan.instantiate(domain):
        schema = name_to_schema[action.name]
        for term, var in zip(action.parameters, schema.parameters):
            if len(var.type_tags) == 1:
                assert term.type_tag is not None, f"{term} untyped in {action.name}"
