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

"""This test module contains the fixtures for 'triangle-tireworld' domain and problem."""
import pytest

from pddl.action import Action
from pddl.core import Domain, Problem
from pddl.logic.base import And, OneOf
from pddl.logic.helpers import constants, variables
from pddl.logic.predicates import Predicate
from pddl.requirements import Requirements


@pytest.fixture(scope="session")
def triangle_tireworld_domain():
    """The 'triangle-tireworld' domain."""
    # types
    location = "location"

    # terms
    to, from_, loc = variables("to from loc", types=[location])

    # constants:
    constants = None

    # predicates
    vehicleat = Predicate("vehicleat", loc)
    spare_in = Predicate("spare-in", loc)
    road = Predicate("road", from_, to)
    not_flattire = Predicate("not-flattire")
    predicates = {vehicleat, spare_in, road, not_flattire}

    # actions
    # move-car
    move_car_name = "move-car"
    move_car_parameters = [from_, to]
    move_car_precondition = vehicleat(from_) & road(from_, to) & not_flattire
    move_car_effect = And(
        OneOf(
            And(vehicleat(to), ~vehicleat(from_)),
            And(vehicleat(to), ~vehicleat(from_), ~not_flattire),
        )
    )
    move_car = Action(
        move_car_name, move_car_parameters, move_car_precondition, move_car_effect
    )

    # changetire
    changetire_name = "changetire"
    changetire_parameters = [loc]
    changetire_precondition = spare_in(loc) & vehicleat(loc)
    changetire_effect = And(~spare_in(loc), not_flattire)
    changetire = Action(
        changetire_name,
        changetire_parameters,
        changetire_precondition,
        changetire_effect,
    )

    name = "triangle-tire"
    requirements = {
        Requirements.STRIPS,
        Requirements.NON_DETERMINISTIC,
        Requirements.TYPING,
    }
    types = dict.fromkeys([location])
    actions = {
        move_car,
        changetire,
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
def triangle_tireworld_problem_01():
    """Triangle-tireworld problem 01."""
    # objects
    objects = [l1, l2, l3, l4, l5, l6, l7, l8, l9] = constants(
        "l1 l2 l3 l4 l5 l6 l7 l8 l9", type_="location"
    )

    # predicates
    vehicleat = Predicate("vehicleat", l1)
    spare_in = Predicate("spare-in", l1)
    road = Predicate("road", l1, l2)
    not_flattire = Predicate("not-flattire")

    init = {
        vehicleat(l1),
        road(l1, l2),
        road(l2, l3),
        road(l1, l4),
        road(l2, l5),
        road(l4, l2),
        road(l5, l3),
        road(l4, l7),
        road(l7, l5),
        spare_in(l4),
        spare_in(l5),
        spare_in(l7),
        spare_in(l7),
    }

    goal = vehicleat(l3)

    problem_name = "triangle-tire-1"

    problem = Problem(
        problem_name,
        domain_name="triangle-tire",
        objects=objects,
        init=init,
        goal=goal,
    )
    return problem
