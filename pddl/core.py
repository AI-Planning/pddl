# -*- coding: utf-8 -*-

"""
Core module of the package.

It contains the class definitions to build and modify PDDL domains or problems.
"""
from enum import Enum
from typing import Optional, Sequence, Set, Tuple

from pddl.helpers import ensure_set


class Domain:
    """A class for a PDDL domain file."""

    def __init__(
        self,
        name: str,
        requirements: Set["Requirements"] = None,
        constants: Optional[Set[str]] = None,
        predicates: Optional[Set["Predicate"]] = None,
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
        self._name = name
        self._requirements = ensure_set(requirements)
        self._constants = ensure_set(constants)
        self._predicates = ensure_set(predicates)
        self._actions = ensure_set(actions)

    @property
    def name(self) -> str:
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


class Predicate:
    """A class for a Predicate in PDDL."""

    def __init__(self, name: str, variables: Sequence[str]):
        """Initialize the predicate."""
        self._name = name
        self._variables = variables

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def variables(self) -> Tuple[str]:
        """Get the variable names."""
        return tuple(self._variables)

    @property
    def arity(self) -> int:
        """Get the arity of the predicate."""
        return len(self.variables)

    def __str__(self) -> str:
        """Get the string."""
        if self.name == "=":
            return "(= {0} {1})".format(str(self.variables[0]), str(self.variables[1]))
        elif self.arity == 0:
            return "(" + self.name + ")"
        else:
            return "({0} {1})".format(self.name, " ".join(map(str, self.variables)))

    def __eq__(self, other):
        """Override equal operator."""
        return (
            isinstance(other, Predicate)
            and self.name == other.name
            and self.arity == other.arity
        )

    def __hash__(self):
        """Get the has of a Predicate."""
        return hash((self.name, self.arity))


class Action:
    """A class for the PDDL Action."""

    # TODO support for other requirements
    # TODO add not for effects
    def __init__(
        self,
        name: str,
        parameters: Sequence[str],
        preconditions: Optional[Set[Predicate]] = None,
        effects: Optional[Set[Predicate]] = None,
    ):
        """Initialize the formula."""
        self._name = name
        self._parameters = parameters
        self._preconditions = ensure_set(preconditions)
        self._effects = ensure_set(effects)

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Tuple[str]:
        """Get the parameters."""
        return tuple(self._parameters)

    @property
    def preconditions(self) -> Set[Predicate]:
        """Get the preconditions."""
        return self._preconditions

    @property
    def effects(self) -> Set[Predicate]:
        """Get the effects."""
        return self._effects


# TODO add other requirements
class Requirements(Enum):
    """Enum class for the requirements."""

    pass
