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

"""This test module contains the fixtures for 'blocksworld-ipc08' domain and problem."""
import pytest

from pddl.action import Action
from pddl.core import Domain, Problem
from pddl.logic import Constant
from pddl.logic.base import And, OneOf
from pddl.logic.effects import When
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import EqualTo, Predicate
from pddl.requirements import Requirements


@pytest.fixture(scope="session")
def blocksworld_fond_domain():
    """The 'blocksworld' FOND domain."""
    # terms
    x, y, z, b = variables("x y z b")

    # constants:
    table = Constant("Table")

    # predicates
    on = Predicate("on", x, y)
    clear = Predicate("clear", x)
    block = Predicate("block", b)
    predicates = {on, clear, block}

    # actions
    # put-on
    put_on_name = "puton"
    put_on_parameters = [x, y, z]
    put_on_precondition = (
        on(x, z)
        & clear(x)
        & clear(y)
        & ~EqualTo(y, z)
        & ~EqualTo(x, z)
        & ~EqualTo(x, y)
        & ~EqualTo(x, table)
    )
    put_on_effect = OneOf(
        And(
            on(x, y),
            ~on(x, z),
            When(~EqualTo(z, table), clear(z)),
            When(~EqualTo(y, table), ~clear(y)),
        ),
        And(
            on(x, table),
            When(~EqualTo(z, table), And(~on(x, z), clear(z))),
            When(~EqualTo(y, table), ~clear(y)),
        ),
    )
    put_on = Action(put_on_name, put_on_parameters, put_on_precondition, put_on_effect)

    name = "blocks-world-domain"
    requirements = {
        Requirements.STRIPS,
        Requirements.EQUALITY,
        Requirements.NON_DETERMINISTIC,
        Requirements.CONDITIONAL_EFFECTS,
    }
    actions = {put_on}
    domain = Domain(
        name=name,
        requirements=requirements,
        constants={table},
        predicates=predicates,
        actions=actions,
    )
    return domain


@pytest.fixture(scope="session")
def blocksworld_fond_01():
    """Blocksworld FOND problem 01."""
    # objects
    objects = [A, B, C] = constants("A B C")

    Table = Constant("Table")

    # predicates
    block = Predicate("block", A)
    on = Predicate("on", C, A)
    clear = Predicate("clear", B)

    init = {
        block(A),
        block(B),
        block(C),
        block(Table),
        on(C, A),
        on(A, Table),
        on(B, Table),
        clear(C),
        clear(B),
        clear(Table),
    }

    goal = on(B, A)

    problem_name = "sussman-anomaly"

    problem = Problem(
        problem_name,
        domain_name="blocks-world-domain",
        objects=objects,
        init=init,
        goal=goal,
    )
    return problem
