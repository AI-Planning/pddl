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

"""Base module for validators."""
from typing import AbstractSet, Collection

from pddl.custom_types import name as name_type
from pddl.definitions.base import TypesDef
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import assert_
from pddl.requirements import Requirements


class BaseValidator:
    """Base class for validators."""

    def __init__(
        self, requirements: AbstractSet[Requirements], types: TypesDef
    ) -> None:
        """Initialize the validator."""
        assert_(type(self) is not BaseValidator)
        self._requirements = requirements
        self._types = types

    @property
    def has_typing(self) -> bool:
        """Check if the typing requirement is specified."""
        return Requirements.TYPING in self._requirements

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
