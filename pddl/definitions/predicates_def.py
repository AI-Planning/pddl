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

"""This module implements the ConstantsDef class to handle the constants of a PDDL domain."""
from typing import AbstractSet, Collection, Dict, Optional

from pddl.custom_types import name as name_type
from pddl.definitions.base import TypesDef, _Definition
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import ensure_set
from pddl.logic.predicates import Predicate
from pddl.requirements import Requirements
from pddl.validation.terms import TermsValidator


class PredicatesDef(_Definition):
    """A set of predicates of a PDDL domain."""

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: TypesDef,
        predicates: Optional[Collection[Predicate]],
    ) -> None:
        """Initialize the PDDL constants section validator."""
        super().__init__(requirements, types)

        self._predicates: AbstractSet[Predicate] = ensure_set(predicates)

        self._check_consistency()

    @property
    def predicates(self) -> AbstractSet[Predicate]:
        """Get the predicates."""
        return self._predicates

    def _check_consistency(self) -> None:
        """Check consistency of the predicates definition."""
        seen_predicates_by_name: Dict[name_type, Predicate] = {}
        for p in self._predicates:
            # check that no two predicates have the same name
            if p.name in seen_predicates_by_name:
                raise PDDLValidationError(
                    f"these predicates have the same name: {p}, {seen_predicates_by_name[p.name]}"
                )
            seen_predicates_by_name[p.name] = p

            # check that the terms of the predicate are consistent
            TermsValidator(self._requirements, self._types).check_terms(p.terms)
