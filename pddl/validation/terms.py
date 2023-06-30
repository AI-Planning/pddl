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
from functools import partial
from typing import Collection, Dict, Generator, Set

from pddl.custom_types import name as name_type
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import check
from pddl.logic.terms import Term, _print_tag_set
from pddl.validation.base import BaseValidator


class TermsValidator(BaseValidator):
    """Class for validator of terms."""

    @classmethod
    def check_terms_consistency(cls, terms: Collection[Term]):
        """
        Check that there are no duplicates.

        This is the non-iterative version of '_check_terms_consistency_iterator'.
        """
        # consume the iterator
        list(cls._check_terms_consistency_iterator(terms))

    @classmethod
    def _check_terms_consistency_iterator(
        cls, terms: Collection[Term]
    ) -> Generator[Term, None, None]:
        """
        Iterate over terms and check that terms with the same name must have the same type tags.

        In particular:
        - terms with the same name must be of the same term type (variable or constant);
        - terms with the same name must have the same type tags.
        """
        seen: Dict[name_type, Set[name_type]] = {}
        for term in terms:
            if term.name not in seen:
                seen[term.name] = set(term.type_tags)
            else:
                expected_type_tags = seen[term.name]
                actual_type_tags = set(term.type_tags)
                check(
                    expected_type_tags == actual_type_tags,
                    f"Term {term} occurred twice with different type tags: "
                    f"previous type tags {_print_tag_set(expected_type_tags)}, "
                    f"new type tags {_print_tag_set(actual_type_tags)}",
                    exception_cls=PDDLValidationError,
                )
            yield term

    def check_terms(self, terms: Collection[Term]) -> None:
        """Check the terms."""
        terms_iter = self._check_terms_consistency_iterator(terms)
        for term in terms_iter:
            self._check_typing_requirement(term.type_tags)
            self._check_types_are_available(
                term.type_tags, partial(self._terms_to_string, terms)
            )

    @classmethod
    def _terms_to_string(cls, terms: Collection[Term]) -> str:
        """Convert terms to string for error messages."""
        return "terms ['" + "', '".join(map(str, terms)) + "']"
