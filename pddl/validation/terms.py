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
from typing import AbstractSet, Collection, Dict, Generator, Optional, Set, Type, Union

from pddl.custom_types import name as name_type
from pddl.definitions.base import TypesDef
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import check
from pddl.logic.terms import Constant, Term, Variable, _print_tag_set
from pddl.requirements import Requirements
from pddl.validation.base import BaseValidator


class TermsValidator(BaseValidator):
    """
    Class for validator of terms.

    Some machinery is required to make the code as much as reusable as possible.
    """

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: TypesDef,
        must_be_instances_of: Optional[Union[Type[Constant], Type[Variable]]] = None,
    ):
        """Initialize the validator."""
        super().__init__(requirements, types)

        # if none, then we don't care if constant or variable
        self._allowed_superclass = must_be_instances_of

    def check_terms_consistency(self, terms: Collection[Term]):
        """
        Check that there are no duplicates.

        This is the non-iterative version of '_check_terms_consistency_iterator'.
        """
        # consume the iterator
        list(self._check_terms_consistency_iterator(terms))

    def _check_terms_consistency_iterator(
        self, terms: Collection[Term]
    ) -> Generator[Term, None, None]:
        """
        Iterate over terms and check that terms with the same name must have the same type tags.

        In particular:
        - terms with the same name must be of the same term type (variable or constant);
        - terms with the same name must have the same type tags.
        """
        seen: Dict[name_type, Set[name_type]] = {}
        for term in terms:
            self._check_same_term_has_same_type_tags(term, seen)
            self._check_term_type(term, term_type=self._allowed_superclass)
            yield term

    @classmethod
    def _check_same_term_has_same_type_tags(
        cls, term: Term, seen: Dict[name_type, Set[name_type]]
    ) -> None:
        """
        Check if the term has already been seen and, if so, that it has the same type tags.

        This is an auxiliary method to simplify the implementation of '_check_terms_consistency_iterator'.
        """
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

    @classmethod
    def _check_term_type(
        cls,
        term: Term,
        term_type: Optional[Union[Type[Constant], Type[Variable]]] = None,
    ):
        """Check that the term is of the specified type."""
        if term_type is not None:
            check(
                isinstance(term, term_type),
                f"expected '{term}' being of type {term_type.__name__}; got {term.__class__.__name__} instead",
                exception_cls=PDDLValidationError,
            )

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
