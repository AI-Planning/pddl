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

"""
This module contains the main PDDL definitions in PDDL domains and problems (predicates, actions etc.)."

Such definitions are useful to wrap consistency checks over the domain components.
"""
from abc import ABC
from typing import AbstractSet, Collection, Dict, Optional

from pddl._validation.base import TermsChecker, Types
from pddl._validation.formula_checker import FormulaChecker
from pddl.custom_types import name as name_type
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import ensure_set
from pddl.logic import Constant, Predicate
from pddl.logic.predicates import DerivedPredicate
from pddl.requirements import Requirements


class _Definition(ABC):
    """Abstract class for a PDDL definition."""

    def __init__(self, requirements: AbstractSet[Requirements], types: Types) -> None:
        """Initialize the PDDL definition."""
        self._requirements = requirements
        self._types = types


class _Constants(_Definition):
    """A set of constants of a PDDL domain."""

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: Types,
        constants: Optional[Collection[Constant]],
    ) -> None:
        """Initialize the PDDL constants section validator."""
        super().__init__(requirements, types)
        self._constants = ensure_set(constants)

        TermsChecker(requirements, types).check(self._constants)

    @property
    def constants(self) -> AbstractSet[Constant]:
        """Get the constants."""
        return self._constants


class _Predicates(_Definition):
    """A set of predicates of a PDDL domain."""

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: Types,
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
            if p.name in seen_predicates_by_name:
                raise PDDLValidationError(
                    f"these predicates have the same name: {p}, {seen_predicates_by_name[p.name]}"
                )
            seen_predicates_by_name[p.name] = p
            TermsChecker(
                self._requirements, self._types, check_repetitions=False
            ).check(p.terms)


class _DerivedPredicates(_Definition):
    """A set of derived predicates of a PDDL domain."""

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: Types,
        constants_def: _Constants,
        predicates_def: _Predicates,
        derived_predicates: Optional[Collection[DerivedPredicate]],
    ) -> None:
        """Initialize the PDDL constants section validator."""
        super().__init__(requirements, types)

        self._constants_def = constants_def
        self._predicates_def = predicates_def
        self._derived_predicates: AbstractSet[DerivedPredicate] = ensure_set(
            derived_predicates
        )

        self._check_consistency()

    @property
    def derived_predicates(self) -> AbstractSet[DerivedPredicate]:
        """Get the predicates."""
        return self._derived_predicates

    def _check_consistency(self) -> None:
        """Check consistency of the derived predicates definition."""
        seen_predicates_by_name: Dict[name_type, Predicate] = {
            p.name: p for p in self._predicates_def.predicates
        }
        for dp in self._derived_predicates:
            self._check_derived_predicate(dp, seen_predicates_by_name)

    def _check_derived_predicate(
        self, dp: DerivedPredicate, seen: Dict[name_type, Predicate]
    ) -> None:
        if dp.predicate.name in seen:
            other_p = seen[dp.predicate.name]
            raise PDDLValidationError(
                f"the name of derived predicate {dp} has been already used by {other_p}"
            )
        seen[dp.predicate.name] = dp.predicate
        TermsChecker(self._requirements, self._types, check_repetitions=False).check(
            dp.predicate.terms
        )
        variables = {t for t in dp.predicate.terms}
        FormulaChecker(
            self._requirements,
            self._types,
            self._constants_def.constants,
            variables,
            self._predicates_def.predicates,
            self._derived_predicates,
        ).check_formula(dp.condition)
