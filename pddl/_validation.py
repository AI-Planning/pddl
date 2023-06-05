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

"""This module defines validation functions for PDDL data structures."""

from typing import Collection, Dict, Optional, Set, Tuple

from pddl.constants import OBJECT
from pddl.custom_types import name, to_names  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import find_cycle
from pddl.logic import Constant, Predicate
from pddl.logic.terms import Term


def _check_types_dictionary(type_dict: Dict[name, Optional[name]]) -> None:
    """
    Check the consistency of the types dictionary.

    1) Empty types dictionary is correct by definition:
    >>> _check_types_dictionary({})

    2) The `object` type cannot be a subtype:
    >>> a = name("a")
    >>> _check_types_dictionary({name("object"): a})
    Traceback (most recent call last):
    ...
    pddl.exceptions.PDDLValidationError: object must not have supertypes, but got object is a subtype of a

    3) If cycles in the type hierarchy graph are present, an error is raised:
    >>> a, b, c = to_names(["a", "b", "c"])
    >>> _check_types_dictionary({a: b, b: c, c: a})
    Traceback (most recent call last):
    ...
    pddl.exceptions.PDDLValidationError: cycle detected in the type hierarchy: a -> b -> c

    :param type_dict: the types dictionary
    """
    if len(type_dict) == 0:
        return

    # check `object` type
    object_name = name(OBJECT)
    if object_name in type_dict and type_dict[object_name] is not None:
        object_supertype = type_dict[object_name]
        raise PDDLValidationError(
            f"object must not have supertypes, but got object is a subtype of {object_supertype}"
        )

    # check cycles
    cycle = find_cycle(type_dict)  # type: ignore
    if cycle is not None:
        raise PDDLValidationError(
            "cycle detected in the type hierarchy: " + " -> ".join(cycle)
        )


def _find_inconsistencies_in_typed_terms(
    terms: Optional[Collection[Term]], all_types: Set[name]
) -> Optional[Tuple[Term, name]]:
    """
    Check that the terms in input all have legal types according to the list of available types.

    :param terms: the terms to check
    :param all_types: all available types
    :return: the type tag that raised the error, None otherwise
    """
    if terms is None:
        return None
    for term in terms:
        for type_tag in sorted(term.type_tags):
            if type_tag is not None and type_tag not in all_types:
                return term, type_tag
    return None


def _check_constant_types(
    constants: Optional[Collection[Constant]], all_types: Set[name]
) -> None:
    check_result = _find_inconsistencies_in_typed_terms(constants, all_types)
    if check_result is not None:
        constant, type_tag = check_result
        raise PDDLValidationError(
            f"type {repr(type_tag)} of constant {repr(constant)} is not in available types {all_types}"
        )


def _check_types_in_has_terms_objects(
    has_terms_objects: Optional[Collection[Predicate]],
    all_types: Set[name],
) -> None:
    """Check that the terms in the set of predicates all have legal types."""
    if has_terms_objects is None:
        return

    for has_terms in has_terms_objects:
        check_result = _find_inconsistencies_in_typed_terms(has_terms.terms, all_types)
        if check_result is not None:
            term, type_tag = check_result
            raise PDDLValidationError(
                f"type {repr(type_tag)} of term {repr(term)} in atomic expression "
                f"{repr(has_terms)} is not in available types {all_types}"
            )
