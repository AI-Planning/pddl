# -*- coding: utf-8 -*-
"""This modules implements PDDL terms."""

from abc import ABC


class Term(ABC):
    """A term in a formula."""


# TODO check correctness
class Constant(Term):
    """A constant term."""

    def __init__(self, name: str):
        """
        Initialize a constant.

        :param name: the name.
        """
        self._name = name

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    def __str__(self) -> str:
        """Get the stirng representation."""
        return self._name

    def __hash__(self):
        """Get the hash."""
        return hash((Constant, self._name))


class Variable(Term):
    """A variable term."""

    def __init__(self, name: str):
        """
        Initialize the variable.

        :param name: the name.
        """
        self._name = name

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    def __str__(self) -> str:
        """Get the stirng representation."""
        return self._name

    def __hash__(self):
        """Get the hash."""
        return hash((Variable, self._name))
