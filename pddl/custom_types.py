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

"""This module defines useful custom types."""

import re
from typing import Collection, Dict, List, Optional, Union

from pddl.helpers.base import RegexConstrainedString, find_cycle


class name(RegexConstrainedString):
    """
    This type represents a 'name' in a PDDL file.

    It must match the following regex: "[A-Za-z][-_A-Za-z0-9]*".
    """

    REGEX = re.compile("[A-Za-z][-_A-Za-z0-9]*")


"""
Either a true name, or a string (potentially not a name!).
The purpose is to make the APIs more usable. The developer
should take care of converting the bare strings to names
for better consistency, when developing a library component;
this can be achieved thanks to 'name' constructor idempotency,
without explicitly caring of whether the arguments
are actually a 'name' or a 'str'.
"""
namelike = Union[name, str]


def to_names(names: Collection[namelike]) -> List[name]:
    """From name-like sequence to list of names."""
    return list(map(name, names))


def to_names_types(
    names: Dict[namelike, Optional[namelike]]
) -> Dict[name, Optional[name]]:
    """From name-like dictionary to name dictionary."""
    return {
        name(type_): name(ancestor) if ancestor else None
        for type_, ancestor in names.items()
    }


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
    ValueError: cycle detected in the type hierarchy: a -> b -> c

    :param type_dict: the types dictionary
    """
    if len(type_dict) == 0:
        return

    # check cycles
    cycle = find_cycle(type_dict)  # type: ignore
    if cycle is not None:
        raise ValueError("cycle detected in the type hierarchy: " + " -> ".join(cycle))
