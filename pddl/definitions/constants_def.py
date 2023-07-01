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

"""This module implements the ConstantsDef class to handle the constants of a PDDL domain."""
from typing import AbstractSet, Collection, Optional

from pddl.definitions.base import TypesDef, _Definition
from pddl.helpers.base import ensure_set
from pddl.logic import Constant
from pddl.requirements import Requirements
from pddl.validation.terms import TermsValidator


class ConstantsDef(_Definition):
    """A set of constants of a PDDL domain."""

    def __init__(
        self,
        requirements: AbstractSet[Requirements],
        types: TypesDef,
        constants: Optional[Collection[Constant]],
    ) -> None:
        """Initialize the PDDL constants section validator."""
        TermsValidator(requirements, types, no_duplicates=True).check_terms(
            constants if constants else []
        )

        super().__init__(requirements, types)
        self._constants = ensure_set(constants)

    @property
    def constants(self) -> AbstractSet[Constant]:
        """Get the constants."""
        return self._constants
