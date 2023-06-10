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

"""Module for validator of terms."""
from typing import AbstractSet, Collection

from pddl.definitions.base import TypesDef
from pddl.logic.predicates import _TermsList
from pddl.logic.terms import Term
from pddl.requirements import Requirements
from pddl.validation.base import BaseValidator


class TermsValidator(BaseValidator):
    """Class for validator of terms."""

    def __init__(
        self, requirements: AbstractSet[Requirements], types: TypesDef
    ) -> None:
        """Initialize the validator."""
        super().__init__(requirements, types)

    def check_terms(self, terms: Collection[Term]) -> None:
        """Check the terms."""
        terms_iter = _TermsList.check_no_duplicate_iterator(terms)
        for term in terms_iter:
            self._check_typing_requirement(term.type_tags)
            self._check_types_are_available(term.type_tags, "terms")
