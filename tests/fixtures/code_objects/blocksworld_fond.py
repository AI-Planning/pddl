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

"""This test module contains the fixtures for 'blocksworld-ipc08' domain and problem."""
import pytest

from pddl.core import Action, Domain, Problem, Requirements
from pddl.logic import Constant
from pddl.logic.base import And, OneOf
from pddl.logic.effects import When, AndEffect
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import EqualTo, Predicate


@pytest.fixture(scope="session")
def blocksworld_fond_domain():
    """The 'blocksworld' FOND domain."""
    # terms
    x, y, z, b= variables("x y z b")

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
    put_on_precondition = on(x, z) & clear(x) & clear(y) & ~EqualTo(y, z) & ~EqualTo(x, z) & ~EqualTo(x, y) & ~EqualTo(
        x, table)
    put_on_effect = OneOf(
        AndEffect(on(x, y), ~on(x, z),
                  When(~EqualTo(z, table), clear(z)), When(~EqualTo(y, table), ~clear(y))),
        AndEffect(on(x, table),
                  When(~EqualTo(z, table), ~on(x, z) & clear(z)), When(~EqualTo(y, table), ~clear(y))),
    )
    put_on = Action(put_on_name, put_on_parameters, put_on_precondition, put_on_effect)

    name = "blocks-world-domain"
    requirements = {
        Requirements.STRIPS,
        Requirements.EQUALITY,
        Requirements.NON_DETERMINISTIC,
        Requirements.CONDITIONAL_EFFECTS
    }
    actions = {
        put_on
    }
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
        clear(Table)
    }

    goal = (And())

    problem_name = "sussman-anomaly"

    problem = Problem(
        problem_name,
        domain_name="blocks-world-domain",
        objects=objects,
        init=init,
        goal=goal,
    )
    return problem
