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

"""Base module for the PDDL definitions."""
from typing import AbstractSet, Dict, FrozenSet, Mapping, Optional, Set, cast

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names, to_types  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import (
    assert_,
    ensure,
    ensure_set,
    find_cycle,
    transitive_closure,
)
from pddl.parser.symbols import Symbols
from pddl.requirements import Requirements


class TypesDef:
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

        # only for printing purposes
        self._sorted_all_types = sorted(self._all_types)

    @property
    def raw(self) -> Mapping[name_type, Optional[name_type]]:
        """Get the raw types dictionary."""
        return self._types

    @property
    def all_types(self) -> FrozenSet[name_type]:
        """Get all available types."""
        return self._all_types

    def is_subtype(self, type_a: name_type, type_b: name_type) -> bool:
        """Check if type_a is a subtype of type_b."""
        # check whether type_a and type_b are legal types
        error_msg = "type {0} is not in available types {1}"
        if type_a not in self._all_types:
            raise PDDLValidationError(
                error_msg.format(repr(type_a), self._sorted_all_types)
            )
        if type_b not in self._all_types:
            raise PDDLValidationError(
                error_msg.format(repr(type_b), self._sorted_all_types)
            )

        return type_a in self._types_closure.get(type_b, set())

    def _get_all_types(self) -> FrozenSet[name_type]:
        """Get all types supported by the domain."""
        if self._types is None:
            return frozenset()
        result = set(self._types.keys()) | set(self._types.values())
        result.discard(None)
        return cast(FrozenSet[name_type], frozenset(result))

    def _compute_types_closure(self) -> Mapping[name_type, Set[name_type]]:
        """Compute the closure of the types dictionary."""
        return transitive_closure(self._types)

    @classmethod
    def _check_types_dictionary(
        cls,
        type_dict: Mapping[name_type, Optional[name_type]],
        requirements: AbstractSet[Requirements],
    ) -> None:
        """
        Check the consistency of the types dictionary.

        1) Empty types dictionary is correct by definition:
        >>> TypesDef._check_types_dictionary({}, set())

        2) There are supertypes, but :typing requirement not specified
        >>> a, b, c = to_names(["a", "b", "c"])
        >>> TypesDef._check_types_dictionary({a: b, b: c}, set())
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: typing requirement is not specified, but types are used: 'b', 'c'

        3) The `object` type cannot be a subtype:
        >>> a = name_type("a")
        >>> TypesDef._check_types_dictionary({name_type("object"): a}, {Requirements.TYPING})
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: object must not have supertypes, but got 'object' is a subtype of 'a'

        4) If cycles in the type hierarchy graph are present, an error is raised:
        >>> a, b, c = to_names(["a", "b", "c"])
        >>> TypesDef._check_types_dictionary({a: b, b: c, c: a}, {Requirements.TYPING})
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


class _Definition:
    """Abstract class for a PDDL definition."""

    def __init__(
        self, requirements: AbstractSet[Requirements], types: TypesDef
    ) -> None:
        """Initialize the PDDL definition."""
        assert_(type(self) is not _Definition)
        self._requirements = requirements
        self._types = types
