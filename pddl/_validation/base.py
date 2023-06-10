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

import functools
from abc import ABC
from typing import AbstractSet, Collection, Dict, Optional, Set, Tuple, cast

from pddl.action import Action
from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names, to_types  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import check, ensure, ensure_set, find_cycle, transitive_closure
from pddl.logic import Predicate
from pddl.logic.terms import Term, _print_tag_set
from pddl.parser.symbols import Symbols
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


class Types:
    """A class for representing and managing the types available in a PDDL Domain."""

    def __init__(
        self,
        types: Optional[Dict[namelike, Optional[namelike]]] = None,
        requirements: Optional[AbstractSet[Requirements]] = None,
        skip_checks: bool = False,
    ) -> None:
        """Initialize the Types object."""
        self._types = to_types(ensure(types, dict()))
        if not skip_checks:
            self._check_types_dictionary(self._types, ensure_set(requirements))

        self._all_types = self._get_all_types()
        self._types_closure = self._compute_types_closure()

    @property
    def raw(self) -> Dict[name_type, Optional[name_type]]:
        """Get the raw types dictionary."""
        return self._types

    @property
    def all_types(self) -> Set[name_type]:
        """Get all available types."""
        return self._all_types

    def is_subtype(self, type_a: name_type, type_b: name_type) -> bool:
        """Check if type_a is a subtype of type_b."""
        # check whether type_a and type_b are legal types
        error_msg = "type {0} is not in available types {1}"
        if type_a not in self._all_types:
            raise PDDLValidationError(error_msg.format(repr(type_a), self._all_types))
        if type_b not in self._all_types:
            raise PDDLValidationError(error_msg.format(repr(type_b), self._all_types))

        return type_a in self._types_closure.get(type_b, set())

    def _get_all_types(self) -> Set[name_type]:
        """Get all types supported by the domain."""
        if self._types is None:
            return set()
        result = set(self._types.keys()) | set(self._types.values())
        result.discard(None)
        return cast(Set[name_type], result)

    def _compute_types_closure(self) -> Dict[name_type, Set[name_type]]:
        """Compute the closure of the types dictionary."""
        return transitive_closure(self._types)

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
        # need to convert type_dict to a dict of sets, because find_cycle() expects a dict of sets
        cycle = find_cycle(
            {
                key: {value} if value is not None else set()
                for key, value in type_dict.items()
            }
        )  # type: ignore
        if cycle is not None:
            raise PDDLValidationError(
                "cycle detected in the type hierarchy: " + " -> ".join(cycle)
            )


class BaseTypeChecker(ABC):
    """Implementation of a base class for type checkers for PDDL definitions."""

    def __init__(self, requirements: AbstractSet[Requirements], types: Types) -> None:
        """Initialize the type checker."""
        self._types = types
        self._requirements = ensure_set(requirements)

    @property
    def has_typing(self) -> bool:
        """Check if the typing requirement is specified."""
        return Requirements.TYPING in self._requirements

    def _raise_not_implemented_error(self, obj: object) -> None:
        """Raise a NotImplementedError for the given object."""
        raise NotImplementedError(f"cannot check PDDL types of {type(obj)}")

    def _check_typing_requirement(self, type_tags: Collection[name_type]) -> None:
        """Check that the typing requirement is specified."""
        if not self.has_typing and len(type_tags) > 0:
            raise PDDLValidationError(
                f"typing requirement is not specified, but the following types were used: {type_tags}"
            )

    def _check_types_are_available(
        self, type_tags_or_none: Optional[Collection[name_type]], what: str
    ) -> None:
        """Check that the types are available in the domain."""
        type_tags = ensure_set(type_tags_or_none)
        if not self._types.all_types.issuperset(type_tags):
            raise PDDLValidationError(
                f"types {_print_tag_set(set(type_tags))} of {what} are not in available types {self._types.all_types}"
            )

    def _check_terms(
        self, terms: Collection[Term], check_repetitions: bool = True
    ) -> None:
        """Check the consistency of the terms."""
        seen: Dict[name_type, Set[name_type]] = {}
        for term in terms:
            self._check_term_repetition(term, seen, check_repetitions=check_repetitions)
            self._check_term(term)

    def _check_term_repetition(
        self,
        term: Term,
        seen: Dict[name_type, Set[name_type]],
        check_repetitions: bool = True,
    ) -> None:
        if term.name not in seen:
            seen[term.name] = set(term.type_tags)
        else:
            if check_repetitions:
                raise PDDLValidationError("term occurs more than once: " + repr(term))
            if seen[term.name] != set(term.type_tags):
                raise PDDLValidationError(
                    f"Term {term} has inconsistent type tags: previous type tags {_print_tag_set(seen[term.name])}, "
                    f"new type tags {_print_tag_set(term.type_tags)}",
                )

    def _check_term(self, term: Term) -> None:
        """Check types annotations of a PDDL term."""
        self._check_typing_requirement(term.type_tags)
        self._check_types_are_available(term.type_tags, f"term {repr(term)}")


class TermsChecker(BaseTypeChecker):
    """Utility class to perform checks on collections of terms."""

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: Types,
        check_repetitions: bool = True,
    ) -> None:
        """Initialize the checker."""
        super().__init__(requirements, types)
        self._check_repetitions = check_repetitions

    def check(self, terms: Collection[Term]):
        self._check_terms(terms, self._check_repetitions)


class DomainTypeChecker(BaseTypeChecker):
    """Implementation of a type checker for domains instances."""

    @functools.singledispatchmethod
    def check_type(self, domain: "Domain"):
        """Check types annotations of PDDL domain."""
        self._check_type_terms(domain.constants)
        self.check_type(domain.predicates)
        self.check_type(domain.actions)

    @check_type.register
    def _(self, action: Action) -> None:
        """Check types annotations of a PDDL term."""
        self.check_type(action.parameters)
        self.check_type(action.precondition)
        self.check_type(action.effect)

    @check_type.register
    def _(self, predicate: Predicate) -> None:
        """Check types annotations of a PDDL term."""
        self._check_type_terms(predicate.terms)


class ActionTypeChecker(BaseTypeChecker):
    """Implementation of a type checker for action instances."""

    @functools.singledispatchmethod
    def check_type(self, domain: "Domain"):
        """Check types annotations of PDDL domain."""
