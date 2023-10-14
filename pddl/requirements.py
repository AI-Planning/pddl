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

"""This module contains the definition of the PDDL requirements."""
import functools
from enum import Enum
from typing import AbstractSet, Set

from pddl.parser.symbols import RequirementSymbols as RS


@functools.total_ordering
class Requirements(Enum):
    """Enum class for the requirements."""

    STRIPS = RS.STRIPS.strip()
    TYPING = RS.TYPING.strip()
    NEG_PRECONDITION = RS.NEG_PRECONDITION.strip()
    DIS_PRECONDITION = RS.DIS_PRECONDITION.strip()
    UNIVERSAL_PRECONDITION = RS.UNIVERSAL_PRECONDITION.strip()
    EXISTENTIAL_PRECONDITION = RS.EXISTENTIAL_PRECONDITION.strip()
    QUANTIFIED_PRECONDITION = RS.QUANTIFIED_PRECONDITION.strip()
    EQUALITY = RS.EQUALITY.strip()
    CONDITIONAL_EFFECTS = RS.CONDITIONAL_EFFECTS.strip()
    ADL = RS.ADL.strip()
    DERIVED_PREDICATES = RS.DERIVED_PREDICATES.strip()
    NON_DETERMINISTIC = RS.NON_DETERMINISTIC.strip()
    FLUENTS = RS.FLUENTS.strip()
    OBJECT_FLUENTS = RS.OBJECT_FLUENTS.strip()
    NUMERIC_FLUENTS = RS.NUMERIC_FLUENTS.strip()
    ACTION_COSTS = RS.ACTION_COSTS.strip()

    @classmethod
    def quantified_precondition_requirements(cls) -> Set["Requirements"]:
        """Get the quantified precondition requirements."""
        return {
            Requirements.UNIVERSAL_PRECONDITION,
            Requirements.EXISTENTIAL_PRECONDITION,
        }

    @classmethod
    def adl_requirements(cls) -> Set["Requirements"]:
        """Get the ADL requirements."""
        return {
            Requirements.STRIPS,
            Requirements.TYPING,
            Requirements.NEG_PRECONDITION,
            Requirements.DIS_PRECONDITION,
            Requirements.EQUALITY,
            Requirements.CONDITIONAL_EFFECTS,
        }.union(cls.quantified_precondition_requirements())

    @classmethod
    def fluents_requirements(cls) -> Set["Requirements"]:
        """Get the fluents requirements."""
        return {
            Requirements.OBJECT_FLUENTS,
            Requirements.NUMERIC_FLUENTS,
        }

    def __str__(self) -> str:
        """Get the string representation."""
        return f":{self.value}"

    def __repr__(self) -> str:
        """Get an unambiguous representation."""
        return f"Requirements.{self.name}"

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Requirements):
            return self.value <= other.value
        else:
            return super().__lt__(other)


def _extend_domain_requirements(
    requirements: AbstractSet[Requirements],
) -> Set[Requirements]:
    """Extend the requirements with the domain requirements."""
    extended_requirements = set(requirements)
    if Requirements.QUANTIFIED_PRECONDITION in requirements:
        extended_requirements.update(
            Requirements.quantified_precondition_requirements()
        )
    if Requirements.ADL in requirements:
        extended_requirements.update(Requirements.adl_requirements())
    if Requirements.FLUENTS in requirements:
        extended_requirements.update(Requirements.fluents_requirements())
    return extended_requirements
