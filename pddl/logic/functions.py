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

"""This class implements PDDL functions."""
import functools
from typing import Sequence

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike
from pddl.helpers.base import assert_
from pddl.helpers.cache_hash import cache_hash
from pddl.logic.base import Atomic, Number
from pddl.logic.terms import Term
from pddl.parser.symbols import Symbols


@cache_hash
@functools.total_ordering
class Function(Atomic):
    """A class for a Function in PDDL."""

    def __init__(self, name: namelike, *terms: Term):
        """Initialize the function."""
        self._name = name_type(name)
        self._terms = tuple(terms)

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return self._terms

    @property
    def arity(self) -> int:
        """Get the arity of the function."""
        return len(self.terms)

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
        return Function(self.name, *terms)

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
            isinstance(other, Function)
            and self.name == other.name
            and self.terms == other.terms
        )

    def __hash__(self):
        """Get the has of a Function."""
        return hash((self.name, self.arity, self.terms))

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Function):
            return (self.name, self.terms) < (other.name, other.terms)
        return super().__lt__(other)


class FunctionOperator(Atomic):
    """Operator for to numerical fluent."""

    def __init__(self, function: Function, value: Number, symbol: Symbols):
        """
        Initialize the function operator.

        :param func: function to operate on.
        :param value: value of the operator.
        :param symbol: symbol of the operator.
        """
        self._function = function
        self._value = value
        self._symbol = symbol

    @property
    def function(self) -> Term:
        """Get the numerical fluent."""
        return self._function

    @property
    def symbol(self) -> Term:
        """Get the operation symbol."""
        return self._symbol

    @property
    def value(self) -> Term:
        """Get the value of the operation."""
        return self._value

    def __eq__(self, other) -> bool:
        """Compare with another object."""
        return (
            isinstance(other, self.__class__)
            and self.function == other.function
            and self.value == other.value
        )

    def __hash__(self) -> int:
        """Get the hash."""
        return hash((self, self.function, self.value))

    def __str__(self) -> str:
        """Get the string representation."""
        return f"({self.symbol} {self.function} {self.value})"

    def __repr__(self) -> str:
        """Get the string representation."""
        return f"{type(self).__name__}({self.function}, {self.value})"


class EqualTo(FunctionOperator):
    """Check if numerical fluent is equal to value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the EqualTo operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.EQUAL)


class LesserThan(FunctionOperator):
    """Check if numerical fluent is lesser than value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the LesserThan operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.LESSER)


class LesserEqualThan(FunctionOperator):
    """Check if numerical fluent is lesser or equal than value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the LesserEqualThan operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.LESSER_EQUAL)


class GreaterThan(FunctionOperator):
    """Check if numerical fluent is greater than value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the GreaterThan operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.GREATER)


class GreaterEqualThan(FunctionOperator):
    """Check if numerical fluent is greater or equal than value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the GreaterEqualThan operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.GREATER_EQUAL)


class AssignTo(FunctionOperator):
    """Assign value to numerical fluent."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the AssignTo operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.ASSIGN)


class Increase(FunctionOperator):
    """Increase numerical fluent by value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the Increase operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.INCREASE)


class Decrease(FunctionOperator):
    """Decrease numerical fluent by value."""

    def __init__(self, function: Function, value: Number):
        """
        Initialize the Decrease operator.

        :param func: function to operate on.
        :param value: value of the operator.
        """
        super().__init__(function, value, Symbols.DECREASE)
