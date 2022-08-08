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

"""Base classes for PDDL logic formulas."""
from typing import Optional, Sequence

from pddl.helpers.cache_hash import cache_hash


@cache_hash
class Formula:
    """Base class for all the formulas."""

    def __invert__(self) -> "Formula":
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

    def __init__(self, *operands: Formula):
        """
        Init a binary operator.

        :param operands: the operands.
        """
        self._operands = list(operands)

    @property
    def operands(self) -> Sequence[Formula]:
        """Get the operands."""
        return tuple(self._operands)

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({self.SYMBOL} {' '.join(map(str, self.operands))})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({repr(self._operands)})"

    def __eq__(self, other):
        """Compare with another object."""
        return isinstance(other, type(self)) and self.operands == other.operands

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.operands))


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

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return isinstance(other, type(self)) and self.argument == other.argument

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.argument))


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

    def __neg__(self) -> Formula:
        """Negate."""
        return FALSE


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

    def __neg__(self) -> Formula:
        """Negate."""
        return TRUE


TRUE = TrueFormula()
FALSE = FalseFormula()


class MonotoneOp(type):
    """Metaclass to simplify monotone operator instantiations."""

    _absorbing: Optional[Formula] = None

    def __call__(cls, *args, **kwargs):
        """Init the subclass object."""
        operands = _simplify_monotone_op_operands(cls, *args)
        if len(operands) == 1:
            return operands[0]

        return super(MonotoneOp, cls).__call__(*operands, **kwargs)


class And(BinaryOp, metaclass=MonotoneOp):
    """And operator."""

    _absorbing = FALSE
    SYMBOL = "and"


class Or(BinaryOp, metaclass=MonotoneOp):
    """Or operator."""

    _absorbing = TRUE
    SYMBOL = "or"


class OneOf(BinaryOp):
    """OneOf operator."""

    SYMBOL = "oneof"


class Imply(BinaryOp):
    """Imply operator."""

    SYMBOL = "imply"


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
    :return: True if the formula is a literal; False otherwise.
    """
    return (
        isinstance(formula, Atomic)
        or isinstance(formula, Not)
        and isinstance(formula.argument, Atomic)
    )


def _simplify_monotone_op_operands(cls, *operands):
    operands = list(dict.fromkeys(operands))
    if len(operands) == 0:
        return [~cls._absorbing]
    elif len(operands) == 1:
        return [operands[0]]
    elif cls._absorbing in operands:
        return cls._absorbing

    # shift-up subformulas with same operator. DFS on expression tree.
    new_operands = []
    stack = operands[::-1]  # it is reversed in order to preserve order.
    while len(stack) > 0:
        element = stack.pop()
        if not isinstance(element, cls):
            new_operands.append(element)
            continue
        stack.extend(reversed(element.operands))  # see above regarding reversed.

    return new_operands
