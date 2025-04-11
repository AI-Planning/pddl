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

import lark
import pytest

from pddl.logic.functions import BinaryFunction, Increase, NumericFunction
from pddl.logic.predicates import Predicate
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
