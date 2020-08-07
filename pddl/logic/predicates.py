# -*- coding: utf-8 -*-

"""This class implements PDDL predicates."""
from typing import Sequence

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike
from pddl.helpers import _assert
from pddl.logic.base import Atomic
from pddl.logic.terms import Term


class Predicate(Atomic):
    """A class for a Predicate in PDDL."""

    def __init__(self, name: namelike, *terms: Term):
        """Initialize the predicate."""
        self._name = name_type(name)
        self._terms = terms

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return tuple(self._terms)

    @property
    def arity(self) -> int:
        """Get the arity of the predicate."""
        return len(self.terms)

    # TODO check whether it's a good idea...
    # TODO allow also for keyword-based replacement
    # TODO allow skip replacement with None arguments.
    def __call__(self, *terms: Term):
        """Replace terms."""
        _assert(len(terms) == self.arity, "Number of terms not correct.")
        _assert(
            all(t1.type_tags == t2.type_tags for t1, t2 in zip(self.terms, terms)),
            "Types of replacements is not correct.",
        )
        return Predicate(self.name, *terms)

    def __str__(self) -> str:
        """Get the string."""
        if self.arity == 0:
            return self.name
        else:
            return f"({self.name} {' '.join(map(str, self.terms))})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({self.name}, {', '.join(map(str, self.terms))})"

    def __eq__(self, other):
        """Override equal operator."""
        return (
            isinstance(other, Predicate)
            and self.name == other.name
            and self.terms == other.terms
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

    def __hash__(self) -> int:
        """Get the hash."""
        return hash((EqualTo, self.left, self.right))

    def __str__(self) -> str:
        """Get the string representation."""
        return f"(= {self.left} {self.right})"

    def __repr__(self) -> str:
        """Get the string representation."""
        return f"{type(self).__name__}({self.left}, {self.right})"
