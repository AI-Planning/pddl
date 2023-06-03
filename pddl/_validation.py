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

from typing import Collection, Dict, Optional, Set

from pddl.custom_types import name, to_names  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import find_cycle
from pddl.logic import Constant


def _check_types_dictionary(type_dict: Dict[name, Optional[name]]) -> None:
    """
    Check the consistency of the types dictionary.

    >>> # empty types dictionary is correct
    >>> _check_types_dictionary({})

    >>> # if cycles are present, an error is raised
    >>> a, b, c = to_names(["a", "b", "c"])
    >>> _check_types_dictionary({a: b, b: c, c: a})
    Traceback (most recent call last):
    ...
    pddl.exceptions.PDDLValidationError: cycle detected in the type hierarchy: a -> b -> c

    :param type_dict: the types dictionary
    """
    if len(type_dict) == 0:
        return

    # check cycles
    cycle = find_cycle(type_dict)  # type: ignore
    if cycle is not None:
        raise PDDLValidationError(
            "cycle detected in the type hierarchy: " + " -> ".join(cycle)
        )


def _check_constant_types(
    constants: Optional[Collection[Constant]], all_types: Set[name]
) -> None:
    """Check that the constants all have legal types."""
    if constants is None:
        return
    for c in constants:
        type_tag = c.type_tag
        if type_tag is not None and type_tag not in all_types:
            raise PDDLValidationError(
                f"type {repr(type_tag)} of constant {repr(c)} is not in available types {all_types}"
            )
