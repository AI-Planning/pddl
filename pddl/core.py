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

"""
Core module of the package.

It contains the class definitions to build and modify PDDL domains or problems.
"""
import functools
from enum import Enum
from typing import AbstractSet, Collection, Optional, Sequence, cast

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names
from pddl.helpers.base import assert_, ensure, ensure_sequence, ensure_set
from pddl.logic.base import FalseFormula, Formula, TrueFormula, is_literal
from pddl.logic.predicates import Predicate
from pddl.logic.terms import Constant, Variable


class Domain:
    """A class for a PDDL domain file."""

    def __init__(
        self,
        name: namelike,
        requirements: Optional[Collection["Requirements"]] = None,
        types: Optional[Collection[namelike]] = None,
        constants: Optional[Collection[Constant]] = None,
        predicates: Optional[Collection[Predicate]] = None,  # TODO cannot be empty
        actions: Optional[Collection["Action"]] = None,
    ):
        """
        Initialize a PDDL domain.

        :param name: the name of the domain.
        :param requirements: the requirements supported.
        :param types: the list of supported types.
        :param constants: the constants.
        :param predicates: the predicates.
        :param actions: the actions.
        """
        self._name = name_type(name)
        self._requirements = ensure_set(requirements)
        self._types = set(to_names(ensure_set(types)))
        self._constants = ensure_set(constants)
        self._predicates = ensure_set(predicates)
        self._actions = ensure_set(actions)

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def requirements(self) -> AbstractSet["Requirements"]:
        """Get the PDDL requirements for this domain."""
        return self._requirements

    @property
    def constants(self) -> AbstractSet[Constant]:
        """Get the constants."""
        return self._constants

    @property
    def predicates(self) -> AbstractSet[Predicate]:
        """Get the predicates."""
        return self._predicates

    @property
    def actions(self) -> AbstractSet["Action"]:
        """Get the actions."""
        return self._actions

    @property
    def types(self) -> AbstractSet[name_type]:
        """Get the type definitions, if defined. Else, raise error."""
        return self._types

    def __eq__(self, other):
        """Compare with another object."""
        return (
            isinstance(other, Domain)
            and self.name == other.name
            and self.requirements == other.requirements
            and self.types == other.types
            and self.constants == other.constants
            and self.predicates == other.predicates
            and self.actions == other.actions
        )


class Problem:
    """A class for a PDDL problem file."""

    def __init__(
        self,
        name: namelike,
        domain: Optional[Domain] = None,
        domain_name: Optional[str] = None,
        requirements: Optional[Collection["Requirements"]] = None,
        objects: Optional[Collection["Constant"]] = None,
        init: Optional[Collection[Formula]] = None,
        goal: Optional[Formula] = None,
    ):
        """
        Initialize the PDDL problem.

        :param name: the name of the PDDL problem.
        :param domain: the PDDL domain.
        :param domain: the domain name. Must match with the domain object.
        :param requirements: the set of PDDL requirements.
        :param objects: the set of objects.
        :param init: the initial condition.
        :param goal: the goal condition.
        """
        self._name: str = name_type(name)
        self._domain: Optional[Domain] = domain
        self._domain_name = domain_name
        self._requirements: AbstractSet[Requirements] = ensure_set(requirements)
        self._objects: AbstractSet[Constant] = ensure_set(objects)
        self._init: AbstractSet[Formula] = ensure_set(init)
        self._goal: Formula = ensure(goal, TrueFormula())
        assert_(
            all(map(is_literal, self.init)),
            "Not all formulas of initial condition are literals!",
        )

        self._check_consistency()

    def _check_consistency(self):
        assert_(
            self._domain is not None or self._domain_name is not None,
            "At least one between 'domain' and 'domain_name' must be set.",
        )
        assert_(
            self._domain is None
            or self._domain_name is None
            or self._domain.name == self._domain_name
        )

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def domain(self) -> Domain:
        """Get the domain."""
        assert_(self._domain is not None, "Domain is not set.")
        return cast(Domain, self._domain)

    @domain.setter
    def domain(self, domain: Domain) -> None:
        """Set the domain."""
        if self._domain_name is not None:
            assert_(
                self._domain_name == domain.name,
                f"Domain names don't match. Expected {self._domain_name}, got {domain.name}.",
            )
        self._domain = domain

    @property
    def domain_name(self) -> str:
        """Get the domain name."""
        if self._domain is not None:
            return self._domain.name

        assert_(self._domain_name is not None, "Domain name is not set.")
        return cast(str, self._domain_name)

    @property
    def requirements(self) -> AbstractSet["Requirements"]:
        """Get the requirements."""
        return self._requirements

    @property
    def objects(self) -> AbstractSet["Constant"]:
        """Get the set of objects."""
        return self._objects

    @property
    def init(self) -> AbstractSet[Formula]:
        """Get the initial state."""
        return self._init

    @property
    def goal(self) -> Formula:
        """Get the goal."""
        return self._goal

    def __eq__(self, other):
        """Compare with another object."""
        return (
            isinstance(other, Problem)
            and self.name == other.name
            and self._domain == other._domain
            and self.domain_name == other.domain_name
            and self.requirements == other.requirements
            and self.objects == other.objects
            and self.init == other.init
            and self.goal == other.goal
        )


class Action:
    """A class for the PDDL Action."""

    def __init__(
        self,
        name: namelike,
        parameters: Sequence[Variable],
        precondition: Optional[Formula] = None,
        effect: Optional[Formula] = None,
    ):
        """
        Initialize the action.

        :param name: the action name.
        :param parameters: the action parameters.
        :param precondition: the action precondition.
        :param effect: the action effect.
        """
        self._name: str = name_type(name)
        self._parameters: Sequence[Variable] = ensure_sequence(parameters)
        self._precondition: Formula = ensure(precondition, FalseFormula())
        self._effect: Formula = ensure(effect, FalseFormula())

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Sequence[Variable]:
        """Get the parameters."""
        return self._parameters

    @property
    def precondition(self) -> Formula:
        """Get the precondition."""
        return self._precondition

    @property
    def effect(self) -> Formula:
        """Get the effect."""
        return self._effect

    def __str__(self):
        """Get the string."""
        operator_str = "({0}\n".format(self.name)
        operator_str += f"    :parameters ({' '.join(map(str, self.parameters))})\n"
        operator_str += f"    :precondition {str(self.precondition)}\n"
        operator_str += f"    :effect {str(self.effect)}\n"
        operator_str += ")"
        return operator_str

    def __eq__(self, other):
        """Check equality between two Actions."""
        return (
            isinstance(other, Action)
            and self.name == other.name
            and self.parameters == other.parameters
            and self.precondition == other.precondition
            and self.effect == other.effect
        )

    def __hash__(self):
        """Get the hash."""
        return hash((self.name, self.parameters, self.precondition, self.effect))


@functools.total_ordering
class Requirements(Enum):
    """Enum class for the requirements."""

    STRIPS = "strips"
    EQUALITY = "equality"
    TYPING = "typing"
    ADL = "adl"
    NON_DETERMINISTIC = "non-deterministic"
    NEG_PRECONDITION = "negative-preconditions"
    DIS_PRECONDITION = "disjunctive-preconditions"

    def __str__(self) -> str:
        """Get the string representation."""
        return f":{self.value}"

    def __repr__(self) -> str:
        """Get an unambiguous representation."""
        return f"Requirements{self.name}"

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Requirements):
            return self.value <= other.value
        else:
            return super().__lt__(other)
