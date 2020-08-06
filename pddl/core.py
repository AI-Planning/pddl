# -*- coding: utf-8 -*-

"""
Core module of the package.

It contains the class definitions to build and modify PDDL domains or problems.
"""
from enum import Enum
from typing import AbstractSet, Collection, Optional, Sequence, Set

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names
from pddl.helpers import _assert, ensure_sequence, ensure_set
from pddl.logic.base import Atomic, Formula, is_literal
from pddl.logic.predicates import Predicate
from pddl.logic.terms import Constant, Variable


class Domain:
    """A class for a PDDL domain file."""

    def __init__(
        self,
        name: namelike,
        requirements: Optional[Collection["Requirements"]] = None,
        constants: Optional[Collection[Constant]] = None,
        predicates: Optional[Collection[Predicate]] = None,  # TODO cannot be empty
        actions: Optional[Collection["Action"]] = None,
        types: Optional[Collection[namelike]] = None,
    ):
        """
        Initialize a PDDL domain.

        :param name: the name of the domain.
        :param requirements: the requirements supported.
        :param constants: the constants.
        :param predicates: the predicates.
        :param actions: the actions.
        :param types: the list of supported types.
        """
        self._name = name_type(name)
        self._requirements = ensure_set(requirements)
        self._constants = ensure_set(constants)
        self._predicates = ensure_set(predicates)
        self._actions = ensure_set(actions)
        self._types = set(to_names(ensure_set(types)))

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


class Problem:
    """A class for a PDDL problem file."""

    def __init__(
        self,
        name: namelike,
        domain: Domain,
        requirements: Optional[AbstractSet["Requirements"]] = None,
        objects: Optional[AbstractSet["Object"]] = None,
        init: Optional[AbstractSet[Formula]] = None,
        goal: Optional[AbstractSet[Atomic]] = None,
    ):
        """Initialize the PDDL problem."""
        self._name = name_type(name)
        self._domain = domain
        self._requirements = ensure_set(requirements)
        self._objects = set(to_names(ensure_set(objects)))
        self._init = ensure_set(init)
        self._goal = ensure_set(goal)
        _assert(
            all(map(is_literal, self.init)),
            "Not all formulas of initial condition are literals!",
        )

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def domain(self) -> Domain:
        """Get the domain."""
        return self._domain

    @property
    def requirements(self) -> AbstractSet["Requirements"]:
        """Get the requirements."""
        return self._requirements

    @property
    def objects(self) -> AbstractSet[name_type]:
        """Get the set of objects."""
        return self._objects

    @property
    def init(self) -> AbstractSet[Formula]:
        """Get the initial state."""
        return self._init

    @property
    def goal(self) -> AbstractSet[Atomic]:
        """Get the goal."""
        return self._goal


class Action:
    """A class for the PDDL Action."""

    # TODO support for other requirements
    # TODO 'effect' should be a formula
    def __init__(
        self,
        name: namelike,
        parameters: Sequence[Variable],
        precondition: Optional[Set[Atomic]] = None,
        effect: Optional[Set[Formula]] = None,
    ):
        """Initialize the formula."""
        self._name = name_type(name)
        self._parameters = ensure_sequence(parameters)
        self._precondition = ensure_set(precondition)
        self._effect = ensure_set(effect)
        _assert(all(map(is_literal, self.effect)), "Some effects are not literals!")

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Sequence[Variable]:
        """Get the parameters."""
        return self._parameters

    @property
    def precondition(self) -> AbstractSet[Atomic]:
        """Get the precondition."""
        return self._precondition

    @property
    def effect(self) -> AbstractSet[Formula]:
        """Get the effect."""
        return self._effect

    def __str__(self):
        """Get the string."""
        operator_str = "{0}\n".format(self.name)
        operator_str += "\t:parameters ({0})\n".format(
            " ".join(map(str, self.parameters))
        )
        operator_str += "\t:precondition {0}\n".format(self.precondition)
        operator_str += "\t:effect {0}\n".format(self.effect)
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


class Object:
    """A PDDL object."""

    def __init__(
        self, name: namelike, type_tags: Optional[Collection[namelike]] = None
    ):
        """
        Init an object.

        :param name: the object name.
        :param type_tags: the type tags.
        """
        self._name = name_type(name)
        self._type_tags = set(to_names(ensure_set(type_tags)))

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def type_tags(self) -> AbstractSet[name_type]:
        """Get a set of type tags for this object."""
        return self._type_tags

    def __str__(self):
        """Get the string representation."""
        return self.name

    def __repr__(self):
        """Get an unambiguous string representation."""
        return f"Object({self.name}, {self.type_tags})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, Object)
            and self.name == other.name
            and self.type_tags == other.type_tags
        )

    def __hash__(self) -> int:
        """Get the hash."""
        return hash((Object, self.name, self.type_tags))


class Requirements(Enum):
    """Enum class for the requirements."""

    EQUALITY = "equality"
    TYPING = "typing"
