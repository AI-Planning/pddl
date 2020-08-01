# -*- coding: utf-8 -*-

# -*- coding: utf-8 -*-

"""Base classes for PDDL logic formulas."""
from abc import ABC


class Formula(ABC):
    """Base class for all the formulas."""

    def __neg__(self) -> "Formula":
        """Negate the formula."""
        return Not(self)

    def __and__(self, other: "Formula") -> "Formula":
        """Put in and with another formula."""
        return And(self, other)

    def __or__(self, other: "Formula") -> "Formula":
        """Put in or with another formula."""
        return Or(self, other)

    def __rshift__(self, other: "Formula") -> "Formula":
        """Define A implies B."""
        return Or(Not(self), other)


class BinaryOp(Formula):
    """Binary operator."""

    SYMBOL: str

    def __init__(self, left: Formula, right: Formula):
        """
        Init a binary operator.

        :param left: left operand.
        :param right: right operand.
        """
        self._left = left
        self._right = right

    @property
    def left(self) -> Formula:
        """Get the left operand."""
        return self._left

    @property
    def right(self) -> Formula:
        """Get the right operand."""
        return self._right


class UnaryOp(Formula):
    """Unary operator."""

    def __init__(self, arg: Formula):
        """
        Initialize the unary operator.

        :param arg: the argument.
        """
        self._arg = arg

    @property
    def argument(self) -> Formula:
        """Get the argument."""
        return self._arg


class Atomic(Formula):
    """Atomic formula."""


class And(BinaryOp):
    """And operator."""

    SYMBOL = "&"


class Or(BinaryOp):
    """Or operator."""

    SYMBOL = "|"


class Not(UnaryOp):
    """Not operator."""

    SYMBOL = "~"
