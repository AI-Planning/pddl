#
# Copyright 2021-2023 WhiteMech
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
import copy
import pickle  # nosec
import re

import pytest

from pddl.action import Action
from pddl.core import Domain
from pddl.exceptions import PDDLValidationError
from pddl.logic import Constant, Variable
from pddl.logic.base import And, Not
from pddl.logic.functions import Decrease
from pddl.logic.functions import EqualTo as FunctionEqualTo
from pddl.logic.functions import (
    GreaterEqualThan,
    GreaterThan,
    Increase,
    LesserEqualThan,
    LesserThan,
    NumericFunction,
    NumericValue,
)
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import DerivedPredicate, Predicate
from pddl.parser.symbols import Symbols
from pddl.requirements import Requirements
from tests.conftest import pddl_objects_domains


@pytest.mark.parametrize("domain_obj", pddl_objects_domains)
def test_pickle_domain(domain_obj: Domain) -> None:
    """Test that domain objects can be pickled correctly."""
    domain_obj_bytes = pickle.dumps(domain_obj)  # nosec
    actual_domain_obj = pickle.loads(domain_obj_bytes)  # nosec
    assert domain_obj == actual_domain_obj


@pytest.mark.parametrize("domain_obj", pddl_objects_domains)
def test_deepcopy_domain(domain_obj: Domain) -> None:
    """Test that domain objects can be deepcopied correctly."""
    new_domain_obj = copy.deepcopy(domain_obj)
    assert domain_obj == new_domain_obj


class TestDomainEmpty:
    """Test the empty domain."""

    def setup_method(self):
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


def test_build_simple_domain_with_derived_predicates():
    """Test a simple PDDL domain with derived predicates."""
    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    q = Predicate("q")
    r = Predicate("r")
    dp = DerivedPredicate(p, And(q, r))
    action_1 = Action("action_1", [x, y, z], precondition=p, effect=Not(p))
    domain = Domain(
        "simple_domain",
        predicates={p},
        derived_predicates={dp},
        actions={action_1},
    )
    assert domain


def test_build_domain_with_numeric_fluents():
    """Test a PDDL domain with simple numeric fluents."""
    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    q = Predicate("q")
    r = Predicate("r")
    func1 = NumericFunction("f1", x, y)
    func2 = NumericFunction("f2")
    func3 = NumericFunction("f3")
    action_1 = Action(
        "action_1",
        [x, y, z],
        precondition=p
        & FunctionEqualTo(func1, NumericValue(0))
        & GreaterThan(func2, NumericValue(1))
        & LesserThan(func3, NumericValue(5)),
        effect=Not(p) | q,
    )
    action_2 = Action(
        "action_2",
        [x, y, z],
        precondition=r
        & GreaterEqualThan(func1, NumericValue(1))
        & LesserEqualThan(func2, NumericValue(5)),
        effect=Not(p) | q,
    )
    domain = Domain(
        "domain_with_numeric",
        requirements={Requirements.NUMERIC_FLUENTS},
        predicates={p},
        functions={func1: None, func2: None, func3: None},
        actions={action_1, action_2},
    )
    assert domain


def test_build_domain_with_action_cost():
    """Test a PDDL domain with action costs."""
    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    q = Predicate("q")
    r = Predicate("r")
    cost1 = NumericFunction("cost1", x, y)
    cost2 = NumericFunction("cost2")
    action_1 = Action(
        "action_1",
        [x, y, z],
        precondition=p
        & FunctionEqualTo(cost1, NumericValue(0))
        & GreaterThan(cost2, NumericValue(1)),
        effect=Not(p) & Increase(cost1, NumericValue(1)),
    )
    action_2 = Action(
        "action_2",
        [x, y, z],
        precondition=r & GreaterEqualThan(cost1, NumericValue(1)),
        effect=(Not(p) | q) & Decrease(cost2, NumericValue(1)),
    )
    domain = Domain(
        "domain_with_numeric",
        requirements={Requirements.NUMERIC_FLUENTS},
        predicates={p},
        functions={cost1: None, cost2: None},
        actions={action_1, action_2},
    )
    assert domain


def test_build_domain_with_total_cost():
    """Test a PDDL domain with total costs."""
    x, y, z = variables("x y z")
    p = Predicate("p", x, y, z)
    q = Predicate("q")
    total_cost = NumericFunction(Symbols.TOTAL_COST.value)
    action_1 = Action(
        "action_1",
        [x, y, z],
        precondition=p & q,
        effect=Not(p) & Increase(total_cost, NumericValue(1)),
    )
    domain = Domain(
        "domain_with_total_cost",
        requirements={Requirements.ACTION_COSTS},
        predicates={p},
        functions={total_cost: None},
        actions={action_1},
    )
    assert domain


def test_cycles_in_type_defs_not_allowed() -> None:
    """Test that type defs with cycles are not allowed."""
    with pytest.raises(
        PDDLValidationError, match="cycle detected in the type hierarchy: A -> B -> C"
    ):
        Domain(
            "dummy",
            requirements={Requirements.TYPING},
            types={"A": "B", "B": "C", "C": "A"},
        )


def test_object_must_not_be_subtype() -> None:
    """Test that when the `object` type is used as subtype we raise error."""
    my_type = "my_type"
    type_set = {Symbols.OBJECT.value: my_type}

    with pytest.raises(
        PDDLValidationError,
        match=rf"object must not have supertypes, but got 'object' is a subtype of '{my_type}'",
    ):
        Domain("test", requirements={Requirements.TYPING}, types=type_set)  # type: ignore


def test_constants_type_not_available() -> None:
    """Test that when a type of a constant is not declared we raise error."""
    a = Constant("a", type_tag="t1")

    my_type = "my_type"
    type_set = {my_type: None}

    with pytest.raises(
        PDDLValidationError,
        match=rf"types \['t1'\] of term {re.escape(repr(a))} are not in available types {{'{my_type}'}}",
    ):
        Domain("test", requirements={Requirements.TYPING}, constants={a}, types=type_set)  # type: ignore


def test_predicate_variable_type_not_available() -> None:
    """Test that when a type of a predicate variable is not declared we raise error."""
    x = Variable("a", type_tags={"t1", "t2"})
    p = Predicate("p", x)

    my_type = "my_type"
    type_set = {my_type: None}

    with pytest.raises(
        PDDLValidationError,
        match=rf"types \['t1', 't2'\] of term {re.escape(repr(x))} are not in available types {{'{my_type}'}}",
    ):
        Domain("test", requirements={Requirements.TYPING}, predicates={p}, types=type_set)  # type: ignore


def test_action_parameter_type_not_available() -> None:
    """Test that when a type of a action parameter is not declared we raise error."""
    x = Variable("a", type_tags={"t1", "t2"})
    action = Action("p", [x])

    my_type = "my_type"
    type_set = {my_type: None}

    with pytest.raises(
        PDDLValidationError,
        match=rf"types \['t1', 't2'\] of term {re.escape(repr(x))} are not in available types {{'{my_type}'}}",
    ):
        Domain("test", requirements={Requirements.TYPING}, actions={action}, types=type_set)  # type: ignore


def test_derived_predicate_type_not_available() -> None:
    """Test that when a type of a term of a derived predicate is not declared we raise error."""
    x = Variable("a", type_tags={"t1", "t2"})
    p = Predicate("p", x)
    dp = DerivedPredicate(p, And())

    my_type = "my_type"
    type_set = {my_type: None}

    with pytest.raises(
        PDDLValidationError,
        match=rf"type '(t1|t2)' of term {re.escape(repr(x))} in atomic expression {re.escape(repr(p))} is not in "
        f"available types {{'{my_type}'}}",
    ):
        Domain("test", requirements={Requirements.TYPING}, derived_predicates={dp}, types=type_set)  # type: ignore
