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

"""This class implements PDDL predicates."""
import functools
from typing import Sequence

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike
from pddl.helpers.base import assert_
from pddl.helpers.cache_hash import cache_hash
from pddl.logic.base import Atomic, Formula
from pddl.logic.terms import Term
from pddl.parser.symbols import Symbols


@cache_hash
@functools.total_ordering
class Predicate(Atomic):
    """A class for a Predicate in PDDL."""

    def __init__(self, name: namelike, *terms: Term):
        """Initialize the predicate."""
        self._name = name_type(name)
        self._terms = tuple(terms)

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return self._terms

    @property
    def arity(self) -> int:
        """Get the arity of the predicate."""
        return len(self.terms)

    # TODO check whether it's a good idea...
    # TODO allow also for keyword-based replacement
    # TODO allow skip replacement with None arguments.
    def __call__(self, *terms: Term):
        """Replace terms."""
        assert_(len(terms) == self.arity, "Number of terms not correct.")
        assert_(
            all(t1.type_tags == t2.type_tags for t1, t2 in zip(self.terms, terms)),
            "Types of replacements is not correct.",
        )
        return Predicate(self.name, *terms)

    def __str__(self) -> str:
        """Get the string."""
        if self.arity == 0:
            return f"({self.name})"
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
        return hash((self.name, self.arity, self.terms))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Predicate):
            return (self.name, self.terms) < (other.name, other.terms)
        return super().__lt__(other)


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
        return f"({Symbols.EQUAL} {self.left} {self.right})"

    def __repr__(self) -> str:
        """Get the string representation."""
        return f"{type(self).__name__}({self.left}, {self.right})"


@cache_hash
@functools.total_ordering
class DerivedPredicate(Atomic):
    """A class for a Derived Predicates in PDDL."""

    def __init__(self, predicate: Predicate, condition: Formula) -> None:
        """
        Initialize the derived predicate.

        :param predicate: the predicate
        :param condition: the condition
        """
        self._predicate = predicate
        self._condition = condition

    @property
    def predicate(self) -> Predicate:
        """Get the predicate."""
        return self._predicate

    @property
    def condition(self) -> Formula:
        """Get the condition."""
        return self._condition

    def __hash__(self) -> int:
        """Get the hash."""
        return hash((DerivedPredicate, self.predicate, self.condition))

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({Symbols.DERIVED.value} {self.predicate} {self.condition})"

    def __repr__(self) -> str:
        """Get the string representation."""
        return f"{type(self).__name__}({self.predicate}, {self.condition})"

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, DerivedPredicate):
            return (self.predicate, self._predicate) < (
                other.predicate,
                other.condition,
            )
        return super().__lt__(other)
