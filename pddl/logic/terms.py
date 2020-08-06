# -*- coding: utf-8 -*-
"""This modules implements PDDL terms."""

from abc import ABC
from typing import AbstractSet, Collection, Optional

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names
from pddl.helpers import ensure_set


class Term(ABC):
    """A term in a formula."""

    def __init__(self, type_tags: Optional[Collection[namelike]] = None):
        """
        Initialize a term.

        :param type_tags: the type tags associated to this term.
        """
        self._type_tags = set(to_names(ensure_set(type_tags)))

    @property
    def type_tags(self) -> AbstractSet[name_type]:
        """Get a set of type tags for this term."""
        return self._type_tags


# TODO check correctness
class Constant(Term):
    """A constant term."""

    def __init__(
        self, name: namelike, type_tags: Optional[Collection[namelike]] = None
    ):
        """
        Initialize a constant.

        :param name: the name.
        """
        super().__init__(type_tags=type_tags)
        self._name = name_type(name)

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    def __str__(self) -> str:
        """Get the string representation."""
        return self._name

    def __repr__(self):
        """Get a unique representation of the object."""
        return f"Constant({self.name})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return isinstance(other, Constant) and self.name == other.name

    def __hash__(self):
        """Get the hash."""
        return hash((Constant, self._name))


class Variable(Term):
    """A variable term."""

    def __init__(
        self, name: namelike, type_tags: Optional[Collection[namelike]] = None
    ):
        """
        Initialize the variable.

        :param name: the name.
        """
        super().__init__(type_tags=type_tags)
        self._name = name_type(name)

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    def __str__(self) -> str:
        """Get the stirng representation."""
        return self._name

    def __repr__(self):
        """Get a unique representation of the object."""
        return f"Variable({self.name})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return isinstance(other, Variable) and self.name == other.name

    def __hash__(self) -> int:
        """Get the hash."""
        return hash((Variable, self._name))
