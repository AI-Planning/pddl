# -*- coding: utf-8 -*-
#
# Copyright 2021-2022 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""This modules implements PDDL effects."""
import functools
from typing import AbstractSet, Collection, Generic, Optional, Sequence, TypeVar, Union

from pddl.helpers.base import ensure_set
from pddl.helpers.cache_hash import cache_hash
from pddl.logic import Variable
from pddl.logic.base import Atomic, Formula, Not, OneOf
from pddl.parser.symbols import Symbols

EffectType = TypeVar("EffectType")


@cache_hash
@functools.total_ordering
class AndEffect(Generic[EffectType]):
    """Conjunction of effects."""

    def __init__(self, *operands: EffectType):
        """
        Initialize a conjunction of (conditional) effects.

        :param operands: the operands.
        """
        self._operands = list(operands)

    @property
    def operands(self) -> Sequence[EffectType]:
        """Get the operands."""
        return tuple(self._operands)

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({Symbols.AND.value} {' '.join(map(str, self.operands))})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({repr(self._operands)})"

    def __eq__(self, other):
        """Compare with another object."""
        return isinstance(other, type(self)) and self.operands == other.operands

    def __lt__(self, other) -> bool:
        """Compare with another object."""
        if isinstance(other, AndEffect):
            return tuple(self.operands) < tuple(other.operands)
        return super().__lt__(other)  # type: ignore

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.operands))


@cache_hash
@functools.total_ordering
class When:
    """Represent the 'When' effect."""

    def __init__(self, condition: Formula, effect: "CondEffect") -> None:
        """
        Initialize the effect.

        :param condition: the condition
        :param effect: the effect
        """
        self._condition = condition
        self._effect = effect

    @property
    def condition(self) -> Formula:
        """Get the condition."""
        return self._condition

    @property
    def effect(self) -> "CondEffect":
        """Get the effect."""
        return self._effect

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({Symbols.WHEN.value} {self._condition} {self.effect})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({self.condition}, {self.effect})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, type(self))
            and self.condition == other.condition
            and self.effect == other.effect
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.condition, self.effect))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, When):
            return (self.condition, self.effect) < (other.condition, other.effect)
        return super().__lt__(other)


@cache_hash
@functools.total_ordering
class Forall:
    """Represent the 'Forall' effect."""

    def __init__(
        self, effect: "Effect", variables: Optional[Collection[Variable]] = None
    ) -> None:
        """Initialize the 'forall' effect."""
        self._effect = effect
        self._variables = ensure_set(variables)

    @property
    def effect(self) -> "Effect":
        """Get the effect."""
        return self._effect

    @property
    def variables(self) -> AbstractSet[Variable]:
        """Get the variables."""
        return self._variables

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({Symbols.FORALL.value} ({' '.join(map(str, self.variables))}) {self.effect})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({self.variables}, {self.effect})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, type(self))
            and self.variables == other.variables
            and self.effect == other.effect
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.variables, self.effect))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Forall):
            return (self.variables, self.effect) < (other.variables, other.effect)
        return super().__lt__(other)


PEffect = Union[Atomic, Not]
CEffect = Union[Forall, When, OneOf, "PEffect"]
Effect = Union[AndEffect["CEffect"], CEffect]
CondEffect = Union[AndEffect["PEffect"], "PEffect"]
