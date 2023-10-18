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

"""Base classes for PDDL logic formulas."""
import functools
from typing import AbstractSet, Any, Collection, List, Optional, Sequence

from pddl.helpers.base import ensure_set
from pddl.helpers.cache_hash import cache_hash
from pddl.logic.terms import Variable
from pddl.parser.symbols import Symbols


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


class BinaryOpMetaclass(type):
    """Metaclass to simplify monotone operator instantiations."""

    _absorbing: Optional[Formula] = None
    idempotency: bool = False

    def __call__(cls, *args, **kwargs):
        """Init the subclass object."""
        operands = _simplify_monotone_op_operands(
            cls, *args, idempotency=cls.idempotency
        )
        if len(operands) == 1 and cls.idempotency:
            return operands[0]

        return super(BinaryOpMetaclass, cls).__call__(*operands, **kwargs)


class And(BinaryOp, metaclass=BinaryOpMetaclass):
    """And operator."""

    _absorbing = False
    idempotency = True
    SYMBOL = "and"


class Or(BinaryOp, metaclass=BinaryOpMetaclass):
    """Or operator."""

    _absorbing = True
    idempotency = True
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


@cache_hash
@functools.total_ordering
class QuantifiedCondition(Formula):
    """Superclass for quantified conditions."""

    SYMBOL: str

    def __init__(
        self, cond: "Formula", variables: Optional[Collection[Variable]] = None
    ) -> None:
        """Initialize the quantified condition."""
        self._cond = cond
        self._variables = ensure_set(variables)

    @property
    def condition(self) -> "Formula":
        """Get the condition."""
        return self._cond

    @property
    def variables(self) -> AbstractSet[Variable]:
        """Get the variables."""
        return self._variables

    def __str__(self) -> str:
        """Get the string representation."""

        def build_tags(tags):
            if len(tags) == 0:
                return ""
            return f" - {' '.join(tags)}"

        var_block = " ".join([f"{v}{build_tags(v.type_tags)}" for v in self.variables])
        return f"({self.SYMBOL} ({var_block}) {self.condition})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({self.variables}, {self.condition})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, type(self))
            and self.variables == other.variables
            and self.condition == other.condition
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.variables, self.condition))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, QuantifiedCondition):
            return (self.variables, self.condition) < (other.variables, other.condition)
        return super().__lt__(other)


class ForallCondition(QuantifiedCondition):
    """Forall Condition."""

    SYMBOL = Symbols.FORALL.value


class ExistsCondition(QuantifiedCondition):
    """Exists Condition."""

    SYMBOL = Symbols.EXISTS.value


def ensure_formula(f: Optional[Formula], is_none_true: bool) -> Formula:
    """
    Ensure the argument is a formula.

    :param f: the formula, or None.
    :param is_none_true: if true, None reduces to And(); FalseFormula otherwise.
    :return: the same set, or an empty set if the arg was None.
    """
    return f if f is not None else And() if is_none_true else Or()


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


def _simplify_monotone_op_operands(cls, *operands, idempotency: bool = False):
    old_operands: List[Any] = (
        list(dict.fromkeys(operands)) if idempotency else list(operands)
    )
    if len(old_operands) == 0:
        return []
    elif len(old_operands) == 1:
        return [old_operands[0]]

    # shift-up subformulas with same operator. DFS on expression tree.
    seen = set()
    new_operands = []
    stack = old_operands[::-1]  # it is reversed in order to preserve order.
    while len(stack) > 0:
        element = stack.pop()
        if not isinstance(element, cls):
            if element not in seen:
                seen.add(element)
                new_operands.append(element)
            continue
        stack.extend(reversed(element.operands))  # see above regarding reversed.

    return new_operands
