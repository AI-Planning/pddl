# -*- coding: utf-8 -*-

"""
Core module of the package.

It contains the class definitions to build and modify PDDL domains or problems.
"""
from enum import Enum
from typing import Optional, Sequence, Set, Tuple

from pddl.helpers import ensure_set
from pddl.logic.base import Formula, ensure_formula
from pddl.logic.predicates import Predicate
from pddl.logic.terms import Constant, Variable
from pddl.types import name as name_type
from pddl.types import namelike


class Domain:
    """A class for a PDDL domain file."""

    def __init__(
        self,
        name: namelike,
        requirements: Optional[Set["Requirements"]] = None,
        constants: Optional[Set[Constant]] = None,
        predicates: Optional[Set["Predicate"]] = None,  # TODO cannot be non-empty
        actions: Optional[Set["Action"]] = None,
    ):
        """
        Initialize a PDDL domain.

        :param name: the name of the domain.
        :param requirements: the requirements supported.
        :param constants: the constants.
        :param predicates: the predicates.
        :param actions: the actions.
        """
        self._name = name_type(name)
        self._requirements = ensure_set(requirements)
        self._constants = ensure_set(constants)
        self._predicates = ensure_set(predicates)
        self._actions = ensure_set(actions)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def requirements(self) -> Set["Requirements"]:
        """Get the PDDL requirements for this domain."""
        return self._requirements

    @property
    def constants(self) -> Set:
        """Get the constants."""
        return self._constants

    @property
    def predicates(self) -> Set:
        """Get the predicates."""
        return self._predicates

    @property
    def actions(self) -> Set:
        """Get the actions."""
        return self._actions


class Problem:
    """A class for a PDDL problem file."""

    def __init__(
        self,
        name: namelike,
        domain: Domain,
        requirements: Optional[Set["Requirements"]] = None,
        objects: Optional[Set[str]] = None,
        init: Set["Literal"] = None,
        goal: Set["Predicate"] = None,
    ):
        """Initialize the PDDL problem."""
        self._name = name_type(name)
        self._domain = domain
        self._requirements = ensure_set(requirements)
        self._objects = ensure_set(objects)
        self._init = ensure_set(init)
        self._goal = ensure_set(goal)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def domain(self) -> Domain:
        """Get the domain."""
        return self._domain

    @property
    def requirements(self) -> Set["Requirements"]:
        """Get the requirements."""
        return self._requirements

    @property
    def objects(self) -> Set[str]:
        """Get the set of objects."""
        return self._objects

    @property
    def init(self) -> Set["Literal"]:
        """Get the initial state."""
        return self._init

    @property
    def goal(self) -> Set["Predicate"]:
        """Get the goal."""
        return self._goal


class Action:
    """A class for the PDDL Action."""

    # TODO support for other requirements
    # TODO add not for effects
    def __init__(
        self,
        name: namelike,
        parameters: Sequence[Variable],
        precondition: Optional[Formula] = None,
        effects: Optional[Set[Predicate]] = None,
    ):
        """Initialize the formula."""
        self._name = name_type(name)
        self._parameters = parameters
        self._precondition = ensure_formula(precondition, is_none_true=True)
        self._effects = ensure_set(effects)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Tuple[Variable, ...]:
        """Get the parameters."""
        return tuple(self._parameters)

    @property
    def precondition(self) -> Formula:
        """Get the precondition."""
        return self._precondition

    @property
    def effects(self) -> Set[Predicate]:
        """Get the effects."""
        return self._effects

    def __str__(self):
        """Get the string."""
        operator_str = "{0}\n".format(self.name)
        operator_str += "\t:parameters ({0})\n".format(
            " ".join(map(str, self.parameters))
        )
        operator_str += "\t:precondition {0}\n".format(self.precondition)
        operator_str += "\t:effect {0}\n".format(self.effects)
        return operator_str

    def __eq__(self, other):
        """Check equality between two Actions."""
        return (
            isinstance(other, Action)
            and self.name == other.name
            and self.parameters == other.parameters
            and self.precondition == other.precondition
            and self.effects == other.effects
        )

    def __hash__(self):
        """Get the hash."""
        return hash((self.name, self.parameters, self.precondition, self.effects))


class Literal:
    """A class for a Literal."""

    def __init__(self, predicate: Predicate, value: bool = True):
        """Initialize the Literal."""
        self.predicate = predicate
        self._value = value

    @property
    def is_positive(self) -> bool:
        """Check if the Literal is positive."""
        return self._value

    @classmethod
    def positive(cls, predicate):
        """Return a positive Literal."""
        return Literal(predicate, True)

    @classmethod
    def negative(cls, predicate):
        """Return a negative Literal."""
        return Literal(predicate, False)

    @property
    def variables(self) -> Tuple[Variable, ...]:
        """Get the variables."""
        return self.predicate.variables

    def __repr__(self):
        """Get the representation."""
        return str(self)

    def __str__(self):
        """Represent the Literal as string."""
        if self.is_positive:
            return str(self.predicate)
        if not self.is_positive and self.predicate.name == "=":
            lhs = str(self.variables[0])
            rhs = str(self.variables[1])
            return "(not (= {0} {1}))".format(lhs, rhs)
        if not self.is_positive:
            return "(not {})".format(str(self.predicate))

    def __eq__(self, other):
        """Check the equality between two Literals."""
        return (
            isinstance(other, Literal)
            and self.predicate == other.predicate
            and self.is_positive == other.is_positive
        )

    def __hash__(self):
        """Get the hash of a Literal."""
        return hash((self.predicate, self.is_positive))


# TODO add other requirements
class Requirements(Enum):
    """Enum class for the requirements."""

    pass
