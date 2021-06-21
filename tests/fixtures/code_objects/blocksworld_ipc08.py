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
from pddl.logic.base import And, OneOf
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import EqualTo, Predicate


@pytest.fixture(scope="session")
def blocksworld_domain():
    """The 'blocksworld' domain."""
    # types
    block = "block"

    # terms
    b, b1, b2, b3 = variables("b b1 b2 b3", types=[block])

    # constants:
    constants = None

    # predicates
    holding = Predicate("holding", b)
    emptyhand = Predicate("emptyhand")
    on_table = Predicate("on-table", b)
    on = Predicate("on", b1, b2)
    clear = Predicate("clear", b)
    predicates = {holding, emptyhand, on_table, on, clear}

    # actions
    # pick-up
    pick_up_name = "pick-up"
    pick_up_parameters = [b1, b2]
    pick_up_precondition = ~EqualTo(b1, b2) & emptyhand & clear(b1) & on(b1, b2)
    pick_up_effect = OneOf(
        holding(b1) & clear(b2) & ~emptyhand & ~clear(b1) & ~on(b1, b2),
        clear(b2) & on_table(b1) & ~on(b1, b2),
    )
    pick_up = Action(
        pick_up_name, pick_up_parameters, pick_up_precondition, pick_up_effect
    )

    # pick-up-from-table
    pick_up_from_table_name = "pick-up-from-table"
    pick_up_from_table_parameters = [b]
    pick_up_from_table_precondition = emptyhand & clear(b) & on_table(b)
    pick_up_from_table_effect = OneOf(And(), holding(b) & ~emptyhand & ~on_table(b))
    pick_up_from_table = Action(
        pick_up_from_table_name,
        pick_up_from_table_parameters,
        pick_up_from_table_precondition,
        pick_up_from_table_effect,
    )

    # put-on-block
    put_on_block_name = "put-on-block"
    put_on_block_parameters = [b1, b2]
    put_on_block_precondition = holding(b1) & clear(b2)
    put_on_block_effect = OneOf(
        on(b1, b2) & emptyhand & clear(b1) & ~holding(b1) & ~clear(b2),
        on_table(b1) & emptyhand & clear(b1) & ~holding(b1),
    )
    put_on_block = Action(
        put_on_block_name,
        put_on_block_parameters,
        put_on_block_precondition,
        put_on_block_effect,
    )

    # put-down
    put_down_name = "put-down"
    put_down_parameters = [b]
    put_down_precondition = holding(b)
    put_down_effect = on_table(b) & emptyhand & clear(b) & ~holding(b)
    put_down = Action(
        put_down_name, put_down_parameters, put_down_precondition, put_down_effect
    )
    # pick-tower
    pick_tower_name = "pick-tower"
    pick_tower_parameters = [b1, b2, b3]
    pick_tower_precondition = emptyhand & on(b1, b2) & on(b2, b3)
    pick_tower_effect = OneOf(And(), holding(b2) & clear(b3) & ~emptyhand & ~on(b2, b3))
    pick_tower = Action(
        pick_tower_name,
        pick_tower_parameters,
        pick_tower_precondition,
        pick_tower_effect,
    )

    # put-tower-on-block
    put_tower_on_block_name = "put-tower-on-block"
    put_tower_on_block_parameters = [b1, b2, b3]
    put_tower_on_block_precondition = holding(b2) & on(b1, b2) & clear(b3)
    put_tower_on_block_effect = OneOf(
        on(b2, b3) & emptyhand & ~holding(b2) & ~clear(b3),
        on_table(b2) & emptyhand & ~holding(b2),
    )
    put_tower_on_block = Action(
        put_tower_on_block_name,
        put_tower_on_block_parameters,
        put_tower_on_block_precondition,
        put_tower_on_block_effect,
    )

    # put-tower-down
    put_tower_down_name = "put-tower-down"
    put_tower_down_parameters = [b1, b2]
    put_tower_down_precondition = holding(b2) & on(b1, b2)
    put_tower_down_effect = on_table(b2) & emptyhand & ~holding(b2)
    put_tower_down = Action(
        put_tower_down_name,
        put_tower_down_parameters,
        put_tower_down_precondition,
        put_tower_down_effect,
    )

    name = "blocks-domain"
    requirements = {
        Requirements.EQUALITY,
        Requirements.NON_DETERMINISTIC,
        Requirements.TYPING,
        Requirements.NEG_PRECONDITION,
    }
    types = {block}
    actions = {
        pick_up,
        pick_up_from_table,
        put_on_block,
        put_down,
        pick_tower,
        put_tower_on_block,
        put_tower_down,
    }
    domain = Domain(
        name=name,
        requirements=requirements,
        types=types,
        constants=constants,
        predicates=predicates,
        actions=actions,
    )
    return domain


@pytest.fixture(scope="session")
def blocksworld_problem_01():
    """Blocksworld ipc08 problem 01."""
    # objects
    objects = [b1, b2, b3, b4, b5] = constants("b1 b2 b3 b4 b5", types=["block"])

    # predicates
    emptyhand = Predicate("emptyhand")
    on_table = Predicate("on-table", b1)
    on = Predicate("on", b1, b2)
    clear = Predicate("clear", b1)

    init = {
        emptyhand,
        on(b1, b3),
        on(b2, b1),
        on_table(b3),
        on_table(b4),
        on(b5, b4),
        clear(b2),
        clear(b5),
    }

    goal = (
        emptyhand
        & on(b1, b2)
        & on(b2, b5)
        & on_table(b3)
        & on_table(b4)
        & on_table(b5)
        & clear(b1)
        & clear(b3)
        & clear(b4)
    )

    problem_name = "bw_5_1"

    problem = Problem(
        problem_name,
        domain_name="blocks-domain",
        objects=objects,
        init=init,
        goal=goal,
    )
    return problem
