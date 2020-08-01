# -*- coding: utf-8 -*-

"""This class implements PDDL predicates."""
from typing import Sequence, Tuple

from pddl.logic.base import Atomic
from pddl.logic.terms import Term, Variable
from pddl.types import name as name_type


class Predicate(Atomic):
    """A class for a Predicate in PDDL."""

    def __init__(self, name: name_type, variables: Sequence[Variable]):
        """Initialize the predicate."""
        self._name = name_type(name)
        self._variables = variables

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def variables(self) -> Tuple[Variable, ...]:
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
            and self.variables == other.variables
        )

    def __hash__(self):
        """Get the has of a Predicate."""
        return hash((self.name, self.arity))


class EqualTo(Atomic):
    """Equality predicate."""

    def __init__(self, left: Term, right: Term):
        """
        Initialize the equality predicate.

        :param left: the left term.
        :param right: the right term.
        """
        self._left = left
        self._right = right

    @property
    def left(self) -> Term:
        """Get the left operand."""
        return self._left

    @property
    def right(self) -> Term:
        """Get the right operand."""
        return self._right

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, EqualTo)
            and self.left == other.left
            and self.right == other.right
        )
