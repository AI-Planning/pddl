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
from typing import AbstractSet, Collection, Dict, Optional, Set, Tuple, cast

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names, to_names_types  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import check, ensure, ensure_set, find_cycle
from pddl.logic import Constant, Predicate
from pddl.logic.terms import Term
from pddl.parser.symbols import ALL_SYMBOLS, Symbols
from pddl.requirements import Requirements


def validate(condition: bool, message: str = "") -> None:
    """
    Validate a condition regarding PDDL.

    If the condition is not satisfied, a PDDLValidationError is raised.
    """
    check(condition, message, PDDLValidationError)


def _find_inconsistencies_in_typed_terms(
    terms: Optional[Collection[Term]], all_types: Set[name_type]
) -> Optional[Tuple[Term, name_type]]:
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
    constants: Optional[Collection[Constant]], all_types: Set[name_type]
) -> None:
    check_result = _find_inconsistencies_in_typed_terms(constants, all_types)
    if check_result is not None:
        constant, type_tag = check_result
        raise PDDLValidationError(
            f"type {repr(type_tag)} of constant {repr(constant)} is not in available types {all_types}"
        )


def _check_types_in_has_terms_objects(
    has_terms_objects: Optional[Collection[Predicate]],
    all_types: Set[name_type],
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


def _is_a_keyword(word: str, ignore: Optional[Set[str]] = None) -> bool:
    """Check that the word is not a keyword."""
    ignore_set = ensure_set(ignore)
    return word not in ignore_set and word in ALL_SYMBOLS


class Types:
    """A class for representing and managing the types available in a PDDL Domain."""

    def __init__(
        self,
        types: Optional[Dict[namelike, Optional[namelike]]] = None,
        requirements: Optional[AbstractSet[Requirements]] = None,
    ) -> None:
        """Initialize the Types object."""
        self._types = to_names_types(ensure(types, dict()))

        self._all_types = self._get_all_types()
        self._check_types_dictionary(self._types, ensure_set(requirements))

    @property
    def raw(self) -> Dict[name_type, Optional[name_type]]:
        """Get the raw types dictionary."""
        return self._types

    @property
    def all_types(self) -> Set[name_type]:
        """Get all available types."""
        return self._all_types

    def _get_all_types(self) -> Set[name_type]:
        """Get all types supported by the domain."""
        if self._types is None:
            return set()
        result = set(self._types.keys()) | set(self._types.values())
        result.discard(None)
        return cast(Set[name_type], result)

    @classmethod
    def _check_types_dictionary(
        cls,
        type_dict: Dict[name_type, Optional[name_type]],
        requirements: AbstractSet[Requirements],
    ) -> None:
        """
        Check the consistency of the types dictionary.

        1) Empty types dictionary is correct by definition:
        >>> Types._check_types_dictionary({}, set())

        2) There are supertypes, but :typing requirement not specified
        >>> a, b, c = to_names(["a", "b", "c"])
        >>> Types._check_types_dictionary({a: b, b: c}, set())
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: typing requirement is not specified, but types are used: 'b', 'c'

        3) The `object` type cannot be a subtype:
        >>> a = name_type("a")
        >>> Types._check_types_dictionary({name_type("object"): a}, {Requirements.TYPING})
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: object must not have supertypes, but got 'object' is a subtype of 'a'

        4) If cycles in the type hierarchy graph are present, an error is raised:
        >>> a, b, c = to_names(["a", "b", "c"])
        >>> Types._check_types_dictionary({a: b, b: c, c: a}, {Requirements.TYPING})
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: cycle detected in the type hierarchy: a -> b -> c

        :param type_dict: the types dictionary
        """
        if len(type_dict) == 0:
            return

        # check typing requirement
        supertypes = {t for t in type_dict.values() if t is not None}
        if len(supertypes) > 0 and Requirements.TYPING not in requirements:
            raise PDDLValidationError(
                "typing requirement is not specified, but types are used: '"
                + "', '".join(map(str, sorted(supertypes)))
                + "'"
            )

        # check `object` type
        object_name = name_type(Symbols.OBJECT.value)
        if object_name in type_dict and type_dict[object_name] is not None:
            object_supertype = type_dict[object_name]
            raise PDDLValidationError(
                f"object must not have supertypes, but got 'object' is a subtype of '{object_supertype}'"
            )

        # check cycles
        cycle = find_cycle(type_dict)  # type: ignore
        if cycle is not None:
            raise PDDLValidationError(
                "cycle detected in the type hierarchy: " + " -> ".join(cycle)
            )
