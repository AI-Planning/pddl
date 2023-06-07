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
from typing import Set

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

    @classmethod
    def strips_requirements(cls) -> Set["Requirements"]:
        """Get the STRIPS requirements."""
        return {
            Requirements.TYPING,
            Requirements.NEG_PRECONDITION,
            Requirements.DIS_PRECONDITION,
            Requirements.EQUALITY,
            Requirements.CONDITIONAL_EFFECTS,
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
