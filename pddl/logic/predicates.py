#
# Copyright 2021-2023 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""This class implements PDDL predicates."""
import functools
from typing import Collection, Dict, Sequence, Set, Tuple

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, parse_name
from pddl.helpers.base import assert_, check
from pddl.helpers.cache_hash import cache_hash
from pddl.logic.base import Atomic, Formula
from pddl.logic.terms import Constant, Term, _print_tag_set
from pddl.parser.symbols import Symbols


class _TermsList:
    """
    A class wrapper for validating sequences of terms.

    Note that this is only for internal validation of terms, and not specific to any PDDL domain type hierarchy.
    """

    def __init__(self, terms_list: Collection[Term]) -> None:
        """Initialize the terms list."""
        self._terms = tuple(terms_list)

        self._is_ground: bool = all(isinstance(v, Constant) for v in self._terms)

        self._check_terms_consistency()

    @property
    def terms(self) -> Tuple[Term, ...]:
        """Get the terms sequence."""
        return self._terms

    @property
    def is_ground(self) -> bool:
        """Check whether the predicate is ground."""
        return self._is_ground

    def _check_terms_consistency(self):
        """
        Check that the term sequence have consistent type tags.

        In particular, terms with the same name must have the same type tags.
        """
        seen: Dict[name_type, Set[name_type]] = {}
        for term in self._terms:
            if term.name not in seen:
                seen[term.name] = set(term.type_tags)
            else:
                check(
                    seen[term.name] == set(term.type_tags),
                    f"Term {term} has inconsistent type tags: "
                    f"previous type tags {_print_tag_set(seen[term.name])}, "
                    f"new type tags {_print_tag_set(term.type_tags)}",
                    exception_cls=ValueError,
                )


@cache_hash
@functools.total_ordering
class Predicate(Atomic):
    """A class for a Predicate in PDDL."""

    def __init__(self, predicate_name: namelike, *terms: Term):
        """Initialize the predicate."""
        self._name = parse_name(predicate_name)
        self._terms_list = _TermsList(terms)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return self._terms_list.terms

    @property
    def arity(self) -> int:
        """Get the arity of the predicate."""
        return len(self.terms)

    @property
    def is_ground(self) -> bool:
        """Check whether the predicate is ground."""
        return self._terms_list.is_ground

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

        self._terms_list = _TermsList([self._left, self._right])

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
        return f"({Symbols.EQUAL.value} {self.left} {self.right})"

    def __repr__(self) -> str:
        """Get the string representation."""
        return f"{type(self).__name__}({repr(self.left)}, {repr(self.right)})"


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
        return f"{type(self).__name__}({repr(self.predicate)}, {repr(self.condition)})"

    def __eq__(self, other):
        """Override equal operator."""
        return (
            isinstance(other, DerivedPredicate)
            and self.predicate == other.predicate
            and self.condition == other.condition
        )

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, DerivedPredicate):
            return (self.predicate, self._predicate) < (
                other.predicate,
                other.condition,
            )
        return super().__lt__(other)
