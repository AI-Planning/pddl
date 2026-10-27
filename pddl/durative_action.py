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

"""This module contains the definition of the PDDL durative action class."""

from typing import Optional, Sequence

from pddl.custom_types import namelike, parse_name
from pddl.helpers.base import _typed_parameters, ensure_sequence
from pddl.logic.base import Formula
from pddl.logic.temporal import Duration
from pddl.logic.terms import Term, Variable


class DurativeAction:
    """A class for the PDDL Durative Action."""

    def __init__(
        self,
        name: namelike,
        parameters: Sequence[Term],
        duration: Optional[Duration] = None,
        condition: Optional[Formula] = None,
        effect: Optional[Formula] = None,
    ):
        """
        Initialize the durative action.

        :param name: the action name.
        :param parameters: the action parameters.
        :param duration: the action duration constraint.
        :param condition: the durative action condition (timed goal descriptors).
        :param effect: the action effect.
        """
        self._name: str = parse_name(name)
        self._parameters: Sequence[Variable] = ensure_sequence(parameters)
        self._duration = duration
        self._condition = condition
        self._effect = effect

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Sequence[Variable]:
        """Get the parameters."""
        return self._parameters

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return self.parameters

    @property
    def duration(self) -> Optional[Duration]:
        """Get the duration."""
        return self._duration

    @property
    def condition(self) -> Optional[Formula]:
        """Get the condition."""
        return self._condition

    @property
    def effect(self) -> Optional[Formula]:
        """Get the effect."""
        return self._effect

    def instantiate(self, mapping: Sequence[Term]) -> "DurativeAction":
        """Instantiate the action by replacing its parameters according to the mapping."""
        if len(mapping) != len(self.parameters):
            raise ValueError(
                f"Mapping length {len(mapping)} does not match number of parameters {len(self.parameters)}."
            )
        instantiation = {
            param: const for param, const in zip(self.parameters, mapping, strict=False)
        }
        return DurativeAction(
            name=self.name,
            parameters=mapping,
            duration=(
                self.duration.instantiate(instantiation)
                if self.duration is not None
                else None
            ),
            condition=(
                self.condition.instantiate(instantiation)
                if self.condition is not None
                else None
            ),
            effect=(
                self.effect.instantiate(instantiation)
                if self.effect is not None
                else None
            ),
        )

    def __str__(self):
        """Get the string."""
        operator_str = "(:durative-action {0}\n".format(self.name)
        operator_str += f"    :parameters ({_typed_parameters(self.parameters)})\n"
        if self.duration is not None:
            operator_str += f"    :duration {str(self.duration)}\n"
        if self.condition is not None:
            operator_str += f"    :condition {str(self.condition)}\n"
        if self.effect is not None:
            operator_str += f"    :effect {str(self.effect)}\n"
        operator_str += ")"
        return operator_str

    def __eq__(self, other):
        """Check equality between two DurativeActions."""
        return (
            isinstance(other, DurativeAction)
            and self.name == other.name
            and self.parameters == other.parameters
            and self.duration == other.duration
            and self.condition == other.condition
            and self.effect == other.effect
        )

    def __hash__(self):
        """Get the hash."""
        return hash(
            (self.name, self.parameters, self.duration, self.condition, self.effect)
        )

    def __repr__(self) -> str:
        """Get the unambiguous string representation."""
        return (
            f"{type(self).__name__}({self.name}, parameters={', '.join(map(str, self.parameters))}, "
            f"duration={self.duration}, condition={self.condition}, effect={self.effect})"
        )
