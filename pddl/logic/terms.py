# -*- coding: utf-8 -*-
"""This modules implements PDDL terms."""

from abc import ABC

from pddl.types import name as name_type
from pddl.types import namelike


class Term(ABC):
    """A term in a formula."""


# TODO check correctness
class Constant(Term):
    """A constant term."""

    def __init__(self, name: namelike):
        """
        Initialize a constant.

        :param name: the name.
        """
        self._name = name_type(name)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    def __str__(self) -> str:
        """Get the stirng representation."""
        return self._name

    def __repr__(self):
        """Get a unique representation of the object."""
        return f"Constant({self.name})"

    def __hash__(self):
        """Get the hash."""
        return hash((Constant, self._name))


class Variable(Term):
    """A variable term."""

    def __init__(self, name: namelike):
        """
        Initialize the variable.

        :param name: the name.
        """
        self._name = name_type(name)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    def __str__(self) -> str:
        """Get the stirng representation."""
        return self._name

    def __repr__(self):
        """Get a unique representation of the object."""
        return f"Variable({self.name})"

    def __hash__(self):
        """Get the hash."""
        return hash((Variable, self._name))
