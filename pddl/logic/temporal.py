#
# Copyright 2021-2025 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""This module implements PDDL temporal constructs for durative actions."""

import functools
from enum import Enum
from typing import Mapping, Optional, Tuple, Union, cast

from pddl.helpers.base import assert_
from pddl.helpers.cache_hash import cache_hash
from pddl.logic.base import Formula
from pddl.logic.functions import FunctionExpression
from pddl.logic.terms import Term, Variable
from pddl.parser.symbols import Symbols, TemporalSymbols


class TimeSpecifier(Enum):
    """Time specifier for temporal conditions and effects."""

    START = "start"
    END = "end"
    OVER_ALL = "over all"

    def __str__(self) -> str:
        """Get the string representation."""
        return self.value


@cache_hash
@functools.total_ordering
class TimedCondition(Formula):
    """A condition that holds at a time point (start/end) or over an interval (over all)."""

    def __init__(self, time_specifier: TimeSpecifier, condition: Formula) -> None:
        """Initialize a timed condition."""
        self._time_specifier = time_specifier
        self._condition = condition

    @property
    def time_specifier(self) -> TimeSpecifier:
        """Get the time specifier."""
        return self._time_specifier

    @property
    def condition(self) -> Formula:
        """Get the condition."""
        return self._condition

    def instantiate(self, mapping: Mapping[Variable, Term]) -> "TimedCondition":
        """Instantiate the formula with a mapping from variables to terms."""
        return TimedCondition(self.time_specifier, self.condition.instantiate(mapping))

    def __str__(self) -> str:
        """Get the string representation."""
        if self.time_specifier is TimeSpecifier.OVER_ALL:
            return f"({TemporalSymbols.OVER.value} {TemporalSymbols.ALL.value} {self.condition})"
        # 'over all' stands alone; start/end are wrapped as '(at start/end ...)'
        return f"({TemporalSymbols.AT.value} {self.time_specifier} {self.condition})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({self.time_specifier}, {self.condition})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, TimedCondition)
            and self.time_specifier == other.time_specifier
            and self.condition == other.condition
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.time_specifier, self.condition))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, TimedCondition):
            return str(self) < str(other)
        return NotImplemented


@cache_hash
@functools.total_ordering
class TimedEffect(Formula):
    """An effect that occurs at a time point (start/end)."""

    def __init__(self, time_specifier: TimeSpecifier, effect: Formula) -> None:
        """Initialize a timed effect."""
        assert_(
            time_specifier in {TimeSpecifier.START, TimeSpecifier.END},
            "A timed effect can only be at start or at end.",
        )
        self._time_specifier = time_specifier
        self._effect = effect

    @property
    def time_specifier(self) -> TimeSpecifier:
        """Get the time specifier."""
        return self._time_specifier

    @property
    def effect(self) -> Formula:
        """Get the effect."""
        return self._effect

    def instantiate(self, mapping: Mapping[Variable, Term]) -> "TimedEffect":
        """Instantiate the formula with a mapping from variables to terms."""
        return TimedEffect(self.time_specifier, self.effect.instantiate(mapping))

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({TemporalSymbols.AT.value} {self.time_specifier} {self.effect})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({self.time_specifier}, {self.effect})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, TimedEffect)
            and self.time_specifier == other.time_specifier
            and self.effect == other.effect
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.time_specifier, self.effect))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, TimedEffect):
            return str(self) < str(other)
        return NotImplemented


@cache_hash
@functools.total_ordering
class SimpleDurationConstraint(Formula):
    """A simple duration constraint, e.g., `(= ?duration 5)`."""

    def __init__(
        self,
        operator: str,
        expression: FunctionExpression,
        time_specifier: Optional[TimeSpecifier] = None,
    ) -> None:
        """Initialize a simple duration constraint."""
        # PDDL 3.1 permits only these three comparison ops.
        assert_(
            operator
            in {
                Symbols.EQUAL.value,
                Symbols.LESSER_EQUAL.value,
                Symbols.GREATER_EQUAL.value,
            },
            f"Invalid duration constraint operator: {operator}",
        )
        # Optional specifier binds constraint at 'at start/end'; 'over all' not permitted.
        assert_(
            time_specifier is None
            or time_specifier in {TimeSpecifier.START, TimeSpecifier.END},
            f"Invalid duration constraint time specifier: {time_specifier}",
        )
        self._operator = operator
        self._expression = expression
        self._time_specifier = time_specifier

    @property
    def operator(self) -> str:
        """Get the operator."""
        return self._operator

    @property
    def expression(self) -> FunctionExpression:
        """Get the expression."""
        return self._expression

    @property
    def time_specifier(self) -> Optional[TimeSpecifier]:
        """Get the time specifier, if any."""
        return self._time_specifier

    def instantiate(
        self, mapping: Mapping[Variable, Term]
    ) -> "SimpleDurationConstraint":
        """Instantiate the formula with a mapping from variables to terms."""
        return SimpleDurationConstraint(
            self.operator,
            cast(FunctionExpression, self.expression.instantiate(mapping)),
            time_specifier=self.time_specifier,
        )

    def __str__(self) -> str:
        """Get the string representation."""
        inner = (
            f"({self.operator} {TemporalSymbols.DURATION_VAR.value} {self.expression})"
        )
        if self.time_specifier is not None:
            return f"({TemporalSymbols.AT.value} {self.time_specifier} {inner})"
        return inner

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        if self.time_specifier is None:
            return f"{type(self).__name__}({self.operator}, {self.expression})"
        return (
            f"{type(self).__name__}({self.operator}, {self.expression}, "
            f"time_specifier={self.time_specifier})"
        )

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, SimpleDurationConstraint)
            and self.operator == other.operator
            and self.expression == other.expression
            and self.time_specifier == other.time_specifier
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.operator, self.expression, self.time_specifier))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, SimpleDurationConstraint):
            return str(self) < str(other)
        return NotImplemented


@cache_hash
@functools.total_ordering
class Duration(Formula):
    """The `:duration` of a durative action."""

    def __init__(self, *constraints: SimpleDurationConstraint) -> None:
        """Initialize the duration."""
        self._constraints: Tuple[SimpleDurationConstraint, ...] = tuple(constraints)

    @property
    def constraints(self) -> Tuple[SimpleDurationConstraint, ...]:
        """Get the constraints."""
        return self._constraints

    def instantiate(self, mapping: Mapping[Variable, Term]) -> "Duration":
        """Instantiate the formula with a mapping from variables to terms."""
        return Duration(*(c.instantiate(mapping) for c in self.constraints))

    def __str__(self) -> str:
        """Get the string representation."""
        if len(self.constraints) == 0:
            return "()"
        if len(self.constraints) == 1:
            return str(self.constraints[0])
        return f"({Symbols.AND.value} {' '.join(map(str, self.constraints))})"

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return f"{type(self).__name__}({', '.join(map(repr, self.constraints))})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return isinstance(other, Duration) and self.constraints == other.constraints

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.constraints))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Duration):
            return str(self) < str(other)
        return NotImplemented


@cache_hash
@functools.total_ordering
class TimedInitialLiteral(Formula):
    """A timed initial literal, e.g., `(at 5 (light-on))`."""

    def __init__(self, time: Union[int, float], literal: Formula) -> None:
        """Initialize a timed initial literal."""
        self._time = time
        self._literal = literal

    @property
    def time(self) -> Union[int, float]:
        """Get the time."""
        return self._time

    @property
    def literal(self) -> Formula:
        """Get the literal."""
        return self._literal

    def instantiate(self, mapping: Mapping[Variable, Term]) -> "TimedInitialLiteral":
        """Instantiate the formula (no variables, returns self)."""
        return self

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({TemporalSymbols.AT.value} {self.time} {self.literal})"

    def __repr__(self) -> str:
        """Get the unambiguous string representation."""
        return f"{type(self).__name__}({self.time}, {self.literal})"

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, TimedInitialLiteral)
            and self.time == other.time
            and self.literal == other.literal
        )

    def __hash__(self) -> int:
        """Compute the hash of the object."""
        return hash((type(self), self.time, self.literal))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, TimedInitialLiteral):
            return (self.time, str(self.literal)) < (other.time, str(other.literal))
        return NotImplemented
