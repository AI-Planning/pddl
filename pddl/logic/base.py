# -*- coding: utf-8 -*-

# -*- coding: utf-8 -*-

"""Base classes for PDDL logic formulas."""
from abc import ABC
from typing import Optional


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

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({self.SYMBOL} {self.left} {self.right})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({repr(self.left)}, {repr(self.right)})"


class UnaryOp(Formula):
    """Unary operator."""

    SYMBOL: str

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

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({self.SYMBOL} {self.argument})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({repr(self.argument)})"


class Atomic(Formula):
    """Atomic formula."""


class TrueFormula(Formula):
    """A tautology."""

    def __str__(self) -> str:
        """Get the string representation."""
        return "(true)"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return "TrueFormula()"

    def __eq__(self, other):
        """Compare with another object."""
        return isinstance(other, TrueFormula)

    def __hash__(self):
        """Hash the object."""
        return hash(TrueFormula)


class FalseFormula(Formula):
    """A contradiction."""

    def __str__(self) -> str:
        """Get the string representation."""
        return "(false)"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return "FalseFormula()"

    def __eq__(self, other):
        """Compare with another object."""
        return isinstance(other, FalseFormula)

    def __hash__(self):
        """Hash the object."""
        return hash(FalseFormula)


class And(BinaryOp):
    """And operator."""

    SYMBOL = "and"


class Or(BinaryOp):
    """Or operator."""

    SYMBOL = "or"


class OneOf(BinaryOp):
    """OneOf operator."""

    SYMBOL = "oneof"


class Not(UnaryOp):
    """Not operator."""

    SYMBOL = "not"


def ensure_formula(f: Optional[Formula], is_none_true: bool) -> Formula:
    """
    Ensure the argument is a formula.

    :param f: the formula, or None.
    :param is_none_true: if true, None reduces to TrueFormula; FalseFormula otherwise.
    :return: the same set, or an empty set if the arg was None.
    """
    return f if f is not None else TrueFormula() if is_none_true else FalseFormula()


def is_literal(formula: Formula) -> bool:
    """
    Check whether a formula is a literal.

    That is, whether it is one of the following:
    - an atomic formula,
    - a Not formula whose argument is an atomic formula.

    :param formula: the formula.
    :return True if the formula is a literal; False otherwise.
    """
    return (
        isinstance(formula, Atomic)
        or isinstance(formula, Not)
        and isinstance(formula.argument, Atomic)
    )
