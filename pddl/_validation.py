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
from collections.abc import Iterable
from typing import AbstractSet, Collection, Dict, Optional, Set, Tuple, cast

from pddl.action import Action
from pddl.custom_types import _check_not_a_keyword  # noqa: F401
from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names, to_types  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import check, ensure, ensure_set, find_cycle
from pddl.logic import Predicate
from pddl.logic.base import BinaryOp, QuantifiedCondition, UnaryOp
from pddl.logic.effects import AndEffect, Forall, When
from pddl.logic.functions import (
    BinaryFunction,
    FunctionExpression,
    NumericFunction,
    NumericValue,
)
from pddl.logic.predicates import DerivedPredicate, EqualTo
from pddl.logic.terms import Term
from pddl.parser.symbols import Symbols
from pddl.requirements import Requirements, _extend_domain_requirements


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

        self._all_types = self._get_all_types()

        if not skip_checks:
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


class TypeChecker:
    """Implementation of a type checker for domains and problems."""

    def __init__(
        self, types: Types, requirements: Optional[AbstractSet[Requirements]] = None
    ) -> None:
        """Initialize the type checker."""
        self._types = types
        self._requirements = ensure_set(requirements)

    @property
    def has_typing(self) -> bool:
        """Check if the typing requirement is specified."""
        self._extended_requirements = _extend_domain_requirements(self._requirements)
        return Requirements.TYPING in self._extended_requirements

    def _check_typing_requirement(self, type_tags: Collection[name_type]) -> None:
        """Check that the typing requirement is specified."""
        if not self.has_typing and len(type_tags) > 0:
            raise PDDLValidationError(
                f"typing requirement is not specified, but the following types were used: {type_tags}"
            )

    def _check_types_are_available(
        self, type_tags: Collection[name_type], what: str
    ) -> None:
        """Check that the types are available in the domain."""
        if not self._types.all_types.issuperset(type_tags):
            raise PDDLValidationError(
                f"types {sorted(type_tags)} of {what} are not in available types {self._types.all_types}"
            )

    @functools.singledispatchmethod  # type: ignore
    def check_type(self, obj: object):
        """Check types annotations of PDDL data structures."""
        raise NotImplementedError(f"cannot check PDDL types of {type(obj)}")

    @check_type.register
    def _(self, objects: Iterable) -> None:
        """Check the types of collections of objects."""
        for obj in objects:
            self.check_type(obj)

    @check_type.register
    def _(self, term: Term) -> None:
        """Check types annotations of a PDDL term."""
        self._check_typing_requirement(term.type_tags)
        self._check_types_are_available(term.type_tags, f"term {repr(term)}")

    @check_type.register
    def _(self, predicate: Predicate) -> None:
        """Check types annotations of a PDDL predicate."""
        self.check_type(predicate.terms)

    @check_type.register
    def _(self, function: NumericFunction) -> None:
        """Check types annotations of a PDDL numeric function."""
        self.check_type(function.terms)

    @check_type.register
    def _(self, _: NumericValue) -> None:
        """Check types annotations of a PDDL numeric value operator."""
        return None

    @check_type.register
    def _(self, binary_function: BinaryFunction) -> None:
        """Check types annotations of a PDDL numeric binary operator."""
        self.check_type(binary_function.operands)

    @check_type.register
    def _(self, equal_to: EqualTo) -> None:
        """Check types annotations of a PDDL equal-to atomic formula."""
        self.check_type(equal_to.left)
        self.check_type(equal_to.right)

    @check_type.register
    def _(self, derived_predicate: DerivedPredicate) -> None:
        """Check types annotations of a PDDL derived predicate."""
        self.check_type(derived_predicate.predicate)
        self.check_type(derived_predicate.condition)

    @check_type.register
    def _(self, formula: UnaryOp) -> None:
        """Check types annotations of a PDDL unary operator."""
        self.check_type(formula.argument)

    @check_type.register
    def _(self, formula: BinaryOp) -> None:
        """Check types annotations of a PDDL binary operator."""
        self.check_type(formula.operands)

    @check_type.register
    def _(self, formula: QuantifiedCondition) -> None:
        """Check types annotations of a PDDL quantified condition."""
        self.check_type(formula.variables)
        self.check_type(formula.condition)

    @check_type.register
    def _(self, effect: AndEffect) -> None:
        """Check types annotations of a PDDL and-effect."""
        self.check_type(effect.operands)

    @check_type.register
    def _(self, effect: When) -> None:
        """Check types annotations of a PDDL when-effect."""
        self.check_type(effect.condition)
        self.check_type(effect.effect)

    @check_type.register
    def _(self, effect: Forall) -> None:
        """Check types annotations of a PDDL forall-effect."""
        self.check_type(effect.variables)
        self.check_type(effect.effect)

    @check_type.register
    def _(self, action: Action) -> None:
        """Check types annotations of a PDDL term."""
        self.check_type(action.parameters)
        self.check_type(action.precondition)
        self.check_type(action.effect)


class Functions:
    """A class for representing and managing the numeric functions available in a PDDL Domain."""

    def __init__(
        self,
        functions: Optional[Collection[FunctionExpression]] = None,
        requirements: Optional[AbstractSet[Requirements]] = None,
        skip_checks: bool = False,
    ) -> None:
        """Initialize the Functions object."""
        self._functions = ensure(functions, dict())
        self._all_function = self._get_all_functions()

        if not skip_checks:
            self._check_total_cost(self._functions, ensure_set(requirements))

    @property
    def raw(self) -> Dict[NumericFunction, Optional[name_type]]:
        """Get the raw functions' dictionary."""
        return self._functions

    @property
    def all_functions(self) -> Set[NumericFunction]:
        """Get all available functions."""
        return self._all_function

    def _get_all_functions(self) -> Set[NumericFunction]:
        """Get all function supported by the domain."""
        if self._functions is None:
            return set()
        result = set(self._functions.keys()) | set(self._functions.values())
        result.discard(None)
        return cast(Set[NumericFunction], result)

    @classmethod
    def _check_total_cost(
        cls,
        function_dict: Dict[NumericFunction, Optional[name_type]],
        requirements: AbstractSet[Requirements],
    ) -> None:
        """Check consistency of total-cost function with the requirements."""
        if bool(function_dict):
            if any(f.name == Symbols.TOTAL_COST.value for f in {*function_dict}):
                validate(
                    Requirements.ACTION_COSTS in requirements,
                    f"action costs requirement is not specified, but the {Symbols.TOTAL_COST.value} "
                    f"function is specified.",
                )
                if any(
                    isinstance(f, FunctionExpression)
                    and not f.name == Symbols.TOTAL_COST.value
                    for f in function_dict.keys()
                ):
                    validate(
                        Requirements.NUMERIC_FLUENTS
                        in _extend_domain_requirements(requirements),
                        "numeric-fluents requirement is not specified, but numeric fluents are specified.",
                    )
            else:
                validate(
                    Requirements.NUMERIC_FLUENTS
                    in _extend_domain_requirements(requirements),
                    "numeric-fluents requirement is not specified, but numeric fluents are specified.",
                )
