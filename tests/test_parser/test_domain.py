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

"""This module contains the tests for the domain parser."""
from textwrap import dedent
from typing import cast

import lark
import pytest

from pddl.logic.base import And, ExistsCondition, ForallCondition, Not
from pddl.logic.functions import (
    BinaryFunction,
    GreaterEqualThan,
    Increase,
    NumericFunction,
    NumericValue,
)
from pddl.logic.predicates import EqualTo, Predicate
from pddl.logic.terms import Variable
from pddl.parser.domain import DomainParser
from pddl.parser.symbols import Symbols
from tests.conftest import TEXT_SYMBOLS


def test_hierarchical_types() -> None:
    """Test correct parsing of hierarchical types (see https://github.com/AI-Planning/pddl/issues/70)."""
    domain_str = dedent(
        """
    (define (domain logistics)
        (:requirements :strips :typing)
        (:types truck airplane - vehicle
            package vehicle - physobj
            airport location - place
            city place physobj - object)
        (:predicates (in-city ?loc - place ?city - city)
            (at ?obj - physobj ?loc - place)
            (in ?pkg - package ?veh - vehicle))
        (:action LOAD-TRUCK
            :parameters   (?pkg - package ?truck - truck ?loc - place)
            :precondition (and (at ?truck ?loc) (at ?pkg ?loc))
            :effect       (and (not (at ?pkg ?loc)) (in ?pkg ?truck)))
    )
    """
    )
    domain = DomainParser()(domain_str)

    assert domain.types == {
        "truck": "vehicle",
        "airplane": "vehicle",
        "package": "physobj",
        "vehicle": "physobj",
        "airport": "place",
        "location": "place",
        "city": None,
        "place": None,
        "physobj": None,
    }


def test_hierarchical_types_2() -> None:
    """Test correct parsing of hierarchical types, Storage domain."""
    domain_str = dedent(
        """
    (define (domain logistics)
        (:requirements :strips :typing)
        (:types hoist surface place area - object
            container depot - place
            storearea transitarea - area
            crate - surface)
    )
    """
    )
    domain = DomainParser()(domain_str)
    assert domain.types == {
        "hoist": None,
        "surface": None,
        "place": None,
        "area": None,
        "container": "place",
        "depot": "place",
        "storearea": "area",
        "transitarea": "area",
        "crate": "surface",
    }


def test_types_repetition_in_simple_typed_lists_not_allowed() -> None:
    """Check types repetition in simple typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types a b c a)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=r".*error while parsing tokens \['a', 'b', 'c', 'a'\]: "
        "duplicate name 'a' in typed list already present",
    ):
        DomainParser()(domain_str)


def test_types_repetition_in_typed_lists_not_allowed() -> None:
    """Check types repetition in typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types a - t1 b c - t2 a - t3)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=r".*error while parsing tokens \['a', '-', 't1', 'b', 'c', '-', 't2', 'a', '-', 't3'\]: "
        r"duplicate name 'a' in typed list already inherits from types \['t1'\]",
    ):
        DomainParser()(domain_str)


def test_typing_requirement_under_other_domain_requirements() -> None:
    """Check :typing requirement does not throw error if other domain requirements that includes it are detected."""
    domain_str = dedent(
        """
(define (domain test)
  (:requirements :adl)
  (:types a b c)
  (:predicates
    (predicate1 ?x - a)
    (predicate2 ?x - b)
    (predicate3 ?x - c)
    )
  )
    """
    )

    domain = DomainParser()(domain_str)
    assert domain.types == {
        "a": None,
        "b": None,
        "c": None,
    }


@pytest.mark.parametrize("keyword", TEXT_SYMBOLS - {Symbols.OBJECT.value})
def test_keyword_usage_not_allowed_as_name(keyword) -> None:
    """Check keywords usage as names is detected and a parsing error is raised."""
    domain_str = dedent(
        f"""
    (define (domain test)
        (:requirements :typing)
        (:types {keyword})
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=f".*invalid name '{keyword}': it is a keyword",
    ):
        DomainParser()(domain_str)


@pytest.mark.parametrize("keyword", TEXT_SYMBOLS - {Symbols.OBJECT.value})
def test_keyword_usage_not_allowed_as_type(keyword) -> None:
    """Check keywords usage as types is detected and a parsing error is raised."""
    domain_str = dedent(
        f"""
    (define (domain test)
        (:requirements :typing)
        (:types t1 - {keyword})
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=f".*invalid type '{keyword}': it is a keyword",
    ):
        DomainParser()(domain_str)


def test_constants_repetition_in_simple_typed_lists_not_allowed() -> None:
    """Check constants repetition in simple typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1)
        (:constants c1 c2 c3 c1)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['c1', 'c2', 'c3', 'c1'\\]: "
        "duplicate name 'c1' in typed list already present",
    ):
        DomainParser()(domain_str)


def test_constants_repetition_in_typed_lists_not_allowed() -> None:
    """Check constants repetition in typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1 t2)
        (:constants c1 - t1 c1 - t2)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['c1', '-', 't1', 'c1', '-', 't2'\\]: "
        "duplicate name 'c1' in typed list already inherits from types \\['t1'\\]",
    ):
        DomainParser()(domain_str)


def test_variables_repetition_in_simple_typed_lists_allowed() -> None:
    """Check variables repetition in simple typed lists is allowed."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:predicates (p ?x ?y ?z ?x))
    )
    """
    )

    DomainParser()(domain_str)


def test_variables_repetition_in_typed_lists_not_allowed() -> None:
    """Check variables repetition in typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1 t2)
        (:predicates (p ?x - (either t1 t2) ?x - t3))
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=r".*error while parsing tokens \['x', '-', \['t1', 't2'\], 'x', '-', \['t3'\]\]: "
        r"invalid types for item \'x\': previous known tags were \['t1', 't2'\], got \['t3'\]",
    ):
        DomainParser()(domain_str)


def test_variables_typed_with_not_available_types() -> None:
    """Check variables with non-available types raises a parsing error."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1)
        (:predicates (p ?x - t2))
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=r"types \['t2'\] of term Variable\(x\) are not in available types \{'t1'\}",
    ):
        DomainParser()(domain_str)


def test_variables_repetition_allowed_if_same_type() -> None:
    """Check variables repetition in typed lists is allowed."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing :existential-preconditions :universal-preconditions)
        (:types t1 t2)
        (:predicates (p ?x - (either t1 t2) ?x - (either t1 t2)))
        (:action a
            :parameters (?x - t1 ?x - t1)
            :precondition (and (exists (?x - t1) (p ?x ?x)) (forall (?x - t1) (p ?x ?x)))
            :effect (p ?x ?x)
        )
    )
    """
    )
    DomainParser()(domain_str)


def test_action_parameter_type_propagation_in_precondition_and_effect() -> None:
    """Test that the type tags of vars in single action parameters propagate correctly in precondition/effect terms."""
    # single param ?v - mytype; ensure propagation to precondition and effect
    domain_str = dedent(
        """
    (define (domain action_type_propagation)
        (:requirements :typing)
        (:types mytype - object)
        (:predicates
           (P ?x - mytype)
           (Q ?x)                ; untyped here on purpose
        )
        (:action use_v
          :parameters (?v - mytype)
          :precondition (Q ?v)
          :effect (P ?v))
    )
    """
    )
    domain = DomainParser()(domain_str)

    action = next(iter(domain.actions))
    assert action.name == "use_v"

    param_v = action.parameters[0]
    assert param_v.type_tags == {"mytype"}

    assert type(action.precondition) is Predicate
    v_in_pre = action.precondition.terms[0]

    assert type(action.effect) is Predicate
    v_in_eff = action.effect.terms[0]

    # type should be propagated to both occurrences within the action's scope
    assert v_in_pre.type_tags == {"mytype"}
    assert v_in_eff.type_tags == {"mytype"}

    # and the variables should be "the same variable" within the same scope
    assert param_v == v_in_pre
    assert param_v == v_in_eff
    assert v_in_pre == v_in_eff


def test_action_multiple_parameters_each_propagates_independently() -> None:
    """Test that the type tags of vars in many actions parameters propagate correctly in precondition/effect terms."""
    # two params ?x - mytype1 and ?y - mytype2; ensure each keeps its own types
    domain_str = dedent(
        """
    (define (domain action_two_params)
        (:requirements :typing)
        (:types mytype1 mytype2)
        (:predicates
           (R ?x - mytype1 ?y - mytype2)
           (Q ?x)  ; untyped, to check propagation from parameters
           (S ?y)  ; untyped, to check propagation from parameters
        )
        (:action use_xy
          :parameters (?x - mytype1 ?y - mytype2)
          :precondition (Q ?x)
          :effect (R ?x ?y))
        (:action use_yx
          :parameters (?x - mytype1 ?y - mytype2)
          :precondition (S ?y)
          :effect (R ?x ?y))
    )
    """
    )
    domain = DomainParser()(domain_str)

    # ---- Check variable types in predicate *definitions* ----
    preds = {p.name: p for p in domain.predicates}
    pred_R = preds["R"]
    pred_Q = preds["Q"]
    pred_S = preds["S"]

    x_in_R_def = pred_R.terms[0]
    y_in_R_def = pred_R.terms[1]
    assert x_in_R_def.type_tags == {"mytype1"}
    assert y_in_R_def.type_tags == {"mytype2"}

    x_in_Q_def = pred_Q.terms[0]
    y_in_S_def = pred_S.terms[0]
    assert x_in_Q_def.type_tags == set()
    assert y_in_S_def.type_tags == set()
    # ---------------------------------------------------------

    # Sort for deterministic order
    actions = sorted(domain.actions, key=lambda a: a.name)
    action1 = actions[0]  # use_xy
    action2 = actions[1]  # use_yx
    assert action1.name == "use_xy"
    assert action2.name == "use_yx"

    # ---------- Action 1: use_xy ----------
    x_param1, y_param1 = action1.parameters
    assert x_param1.type_tags == {"mytype1"}
    assert y_param1.type_tags == {"mytype2"}

    # Precondition uses only ?x
    assert type(action1.precondition) is Predicate
    x_in_pre1 = action1.precondition.terms[0]
    assert x_in_pre1.type_tags == {"mytype1"}
    assert x_in_pre1 == x_param1

    # Effect uses both ?x and ?y, in order
    assert type(action1.effect) is Predicate
    x_in_eff1 = action1.effect.terms[0]
    y_in_eff1 = action1.effect.terms[1]
    assert x_in_eff1.type_tags == {"mytype1"}
    assert y_in_eff1.type_tags == {"mytype2"}

    # Identity preserved per variable
    assert x_in_eff1 == x_param1
    assert y_in_eff1 == y_param1
    assert x_in_eff1 != y_in_eff1

    # ---------- Action 2: use_yx ----------
    x_param2, y_param2 = action2.parameters
    assert x_param2.type_tags == {"mytype1"}
    assert y_param2.type_tags == {"mytype2"}

    # Precondition uses only ?y
    assert type(action2.precondition) is Predicate
    y_in_pre2 = action2.precondition.terms[0]
    assert y_in_pre2.type_tags == {"mytype2"}
    assert y_in_pre2 == y_param2

    # Effect uses both ?x and ?y, in order
    assert type(action2.effect) is Predicate
    x_in_eff2 = action2.effect.terms[0]
    y_in_eff2 = action2.effect.terms[1]
    assert x_in_eff2.type_tags == {"mytype1"}
    assert y_in_eff2.type_tags == {"mytype2"}

    # Identity preserved per variable
    assert x_in_eff2 == x_param2
    assert y_in_eff2 == y_param2
    assert x_in_eff2 != y_in_eff2


def test_variables_types_propagated_in_derived_predicate() -> None:
    """Test that variables occurring in definition of derived predicate propagated in its condition."""
    domain_str = dedent(
        """
    (define (domain samevariabledifferent)
        (:requirements :typing)
        (:types mytype)
        (:predicates
           (P ?v - mytype)
           (Q ?v)
        )
        (:derived (P ?v)
          (Q ?v))
        )
    """
    )
    domain = DomainParser()(domain_str)

    # check that type of variable ?v in predicates is parsed correctly
    _predicates_sorted_by_name = sorted(domain.predicates, key=lambda p: p.name)
    predicate_p = _predicates_sorted_by_name[0]
    predicate_q = _predicates_sorted_by_name[1]
    var_v_in_pred_p = predicate_p.terms[0]
    var_v_in_pred_q = predicate_q.terms[0]
    assert var_v_in_pred_p.type_tags == {"mytype"}
    assert var_v_in_pred_q.type_tags == set()
    assert var_v_in_pred_p != var_v_in_pred_q

    # check that type of variable ?v in derived predicate is propagated even if type does not occur
    axiom = next(iter(domain.derived_predicates))
    assert type(axiom.predicate) is Predicate
    var1 = axiom.predicate.terms[0]
    assert type(axiom.condition) is Predicate
    var2 = axiom.condition.terms[0]
    assert var1.type_tags == var2.type_tags == {"mytype"}
    assert var1 == var2


def test_variables_types_propagated_in_derived_predicate_complex_condition() -> None:
    """Test that variables occurring in definition of derived predicate propagated in its complex condition."""
    domain_str = dedent(
        """
    (define (domain samevariabledifferent)
        (:requirements :typing :equality :quantified-preconditions)
        (:types mytype mytype2 mytype3)
        (:predicates
           (P ?v - mytype)
           (Q ?v)
           (R ?v)
        )
        (:derived (P ?v)
          (and
            (Q ?v)
            (not (R ?v))
            (exists (?v - mytype2) (Q ?v))
            (forall (?v - mytype3) (Q ?v))
            (= ?v ?v)
            (>= 1 0)
          )
        )
    )
    """
    )
    domain = DomainParser()(domain_str)

    # check that type of variable ?v in predicates is parsed correctly
    mytype = "mytype"
    mytype2 = "mytype2"
    mytype3 = "mytype3"
    _predicates_sorted_by_name = sorted(domain.predicates, key=lambda p: p.name)
    predicate_p = _predicates_sorted_by_name[0]
    predicate_q = _predicates_sorted_by_name[1]
    predicate_r = _predicates_sorted_by_name[2]
    var_v_in_pred_p = predicate_p.terms[0]
    var_v_in_pred_q = predicate_q.terms[0]
    var_v_in_pred_r = predicate_r.terms[0]
    assert var_v_in_pred_p.type_tags == {mytype}
    assert var_v_in_pred_q.type_tags == set()
    assert var_v_in_pred_r.type_tags == set()
    assert var_v_in_pred_p != var_v_in_pred_q

    # check that type of variable ?v in derived predicate is propagated even if type does not occur
    axiom = next(iter(domain.derived_predicates))
    assert type(axiom.predicate) is Predicate
    var1 = axiom.predicate.terms[0]

    assert type(axiom.condition) is And
    operands = axiom.condition.operands
    pred_q, not_pred_r, exists_pred_v2, forall_pred_v3, eq_v_v, ge_pred_1_0 = operands

    assert type(pred_q) is Predicate
    q_var = pred_q.terms[0]
    assert q_var.type_tags == {mytype}
    assert var1 == q_var

    assert type(not_pred_r) is Not
    assert type(not_pred_r.argument) is Predicate
    not_var = not_pred_r.argument.terms[0]
    assert not_var.type_tags == {mytype}
    assert var1 == not_var

    assert type(exists_pred_v2) is ExistsCondition
    assert list(exists_pred_v2.variables)[0].type_tags == {mytype2}
    assert type(exists_pred_v2.condition) is Predicate
    exists_var = exists_pred_v2.condition.terms[0]
    assert exists_var.type_tags == {mytype2}
    assert var1 != exists_var

    assert type(forall_pred_v3) is ForallCondition
    assert list(forall_pred_v3.variables)[0].type_tags == {mytype3}
    assert type(forall_pred_v3.condition) is Predicate
    forall_var = forall_pred_v3.condition.terms[0]
    assert forall_var.type_tags == {mytype3}
    assert var1 != forall_var

    assert type(eq_v_v) is EqualTo
    eq_v1 = eq_v_v.left
    eq_v2 = eq_v_v.right
    assert eq_v1.type_tags == {mytype}
    assert var1 == eq_v1
    assert eq_v2.type_tags == {mytype}
    assert var1 == eq_v2

    assert type(ge_pred_1_0) is GreaterEqualThan


def test_variables_types_propagated_in_derived_predicate_complex_hierarchy() -> None:
    """Test that variables occurring in definition of derived predicate propagated in its condition."""
    domain_str = dedent(
        """
    (define (domain samevariabledifferent)
        (:requirements :typing)
        (:types child1 - root_type
                child2 - child1
                child3 - root_type)
        (:predicates
           (P ?v - root_type)
           (Q ?v)
           (R ?v)
        )
        (:derived (P ?v - child1) (Q ?v))
        (:derived (P ?v - child3) (R ?v))
    )
    """
    )
    domain = DomainParser()(domain_str)
    axioms = sorted(
        domain.derived_predicates, key=lambda dp: cast(Predicate, dp.condition).name
    )
    axiom_pq = axioms[0]
    axiom_pr = axioms[1]

    assert axiom_pq.predicate.terms[0].type_tags == {"child1"}
    assert cast(Predicate, axiom_pq.condition).terms[0].type_tags == {"child1"}

    assert axiom_pr.predicate.terms[0].type_tags == {"child3"}
    assert cast(Predicate, axiom_pr.condition).terms[0].type_tags == {"child3"}


def test_check_action_costs_requirement_with_total_cost() -> None:
    """Check action costs requirement when total-cost is specified."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1 t2)
        (:predicates (p ?x - t1 ?y - t2))
        (:functions (total-cost))
        (:action a
            :parameters (?x - t1 ?y - t2)
            :precondition (and (p ?x ?x))
            :effect (and (p ?x ?x) (increase (total-cost) 1))
        )
    )
    """
    )
    with pytest.raises(
        lark.exceptions.VisitError,
        match=r"action costs requirement is not specified, but the total-cost function is specified.",
    ):
        DomainParser()(domain_str)


def test_variable_types_in_strips_action_definition() -> None:
    """Check typing for predicate variables in action preconditions and effects."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1 t2)
        (:predicates (p ?x - t1 ?y - t2))
        (:action a
            :parameters (?x - t1 ?y - t2)
            :precondition (p ?x ?y)
            :effect (p ?x ?y)
        )
    )
    """
    )
    domain = DomainParser()(domain_str)
    action = next(iter(domain.actions))
    x = Variable("x", {"t1"})
    y = Variable("y", {"t2"})
    assert action.parameters == (x, y)
    assert isinstance(action.precondition, Predicate)
    assert action.precondition.terms == (x, y)
    assert isinstance(action.effect, Predicate)
    assert action.effect.terms == (x, y)


def test_variable_types_in_numeric_action_definition() -> None:
    """Check typing for function variables in action preconditions and effects."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing :numeric-fluents)
        (:types t1 t2)
        (:functions (f ?x - t1 ?y - t2))
        (:action a
            :parameters (?x - t1 ?y - t2)
            :precondition (<= 1 (f ?x ?y))
            :effect (increase (f ?x ?y) 1)
        )
    )
    """
    )
    domain = DomainParser()(domain_str)
    action = next(iter(domain.actions))
    x = Variable("x", {"t1"})
    y = Variable("y", {"t2"})
    assert action.parameters == (x, y)
    assert isinstance(action.precondition, BinaryFunction)
    assert isinstance(action.precondition.operands[1], NumericFunction)
    assert action.precondition.operands[1].terms == (x, y)
    assert isinstance(action.effect, Increase)
    assert isinstance(action.effect.operands[0], NumericFunction)
    assert action.effect.operands[0].terms == (x, y)


def test_number_parsing() -> None:
    """Check parsing of numbers in  action preconditions and effects."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing :numeric-fluents)
        (:types t1 t2)
        (:functions (f ?x - t1 ?y - t2) (g ?x - t2))
        (:action a
            :parameters (?x - t1 ?y - t2)
            :precondition (and (<= 10 (f ?x ?y)) (> (g ?y) 2.5))
            :effect (and (decrease (f ?x ?y) 42) (increase (g ?y) 0.5))
        )
    )
    """
    )
    domain = DomainParser()(domain_str)
    action = next(iter(domain.actions))
    x = Variable("x", {"t1"})
    y = Variable("y", {"t2"})
    assert action.parameters == (x, y)
    assert isinstance(action.precondition, And)
    f_precond = action.precondition.operands[0]
    assert isinstance(f_precond, BinaryFunction)
    g_precond = action.precondition.operands[1]
    assert isinstance(g_precond, BinaryFunction)
    assert type(f_precond.operands[0]) is NumericValue
    assert type(f_precond.operands[0].value) is int
    assert f_precond.operands[0] == NumericValue(10)
    assert isinstance(f_precond.operands[1], NumericFunction)
    assert f_precond.operands[1].terms == (x, y)
    assert type(g_precond.operands[1]) is NumericValue
    assert type(g_precond.operands[1].value) is float
    assert g_precond.operands[1] == NumericValue(2.5)
    assert isinstance(g_precond.operands[0], NumericFunction)
    assert g_precond.operands[0].terms == (y,)
    assert isinstance(action.effect, And)
    f_effect = action.effect.operands[0]
    assert isinstance(f_effect, BinaryFunction)
    assert isinstance(f_effect.operands[0], NumericFunction)
    assert isinstance(f_effect.operands[1], NumericValue)
    g_effect = action.effect.operands[1]
    assert isinstance(g_effect, BinaryFunction)
    assert isinstance(g_effect.operands[0], NumericFunction)
    assert isinstance(g_effect.operands[1], NumericValue)
    assert f_effect.operands[0].terms == (x, y)
    assert type(f_effect.operands[1]) is NumericValue
    assert type(f_effect.operands[1].value) is int
    assert f_effect.operands[1] == NumericValue(42)
    assert g_effect.operands[0].terms == (y,)
    assert type(g_effect.operands[1]) is NumericValue
    assert type(g_effect.operands[1].value) is float
    assert g_effect.operands[1] == NumericValue(0.5)
