#
# Copyright 2021-2025 WhiteMech
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
from typing import AbstractSet, Collection, Dict, List, Optional, Union

from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import RegexConstrainedString, ensure_set
from pddl.parser.symbols import ALL_SYMBOLS, Symbols


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

Note, you should not use the raw constructor 'name' (unless you
know what you are doing), but the functino 'parse_name' below.
"""
namelike = Union[name, str]


def parse_name(s: str) -> name:
    """
    Parse a name from a string.

    It performs two validations:
    - the name is not a keyword;
    - the name is a valid name, i.e. it matches the name regular expression.

    Optionally, a set of keywords to ignore can be provided.

    :param s: the input string to be parsed
    :return: the parsed name
    """
    _check_not_a_keyword(s, "name", ignore=set())
    return name(s)


def parse_type(s: str) -> name:
    """
    Parse a type from a string.

    It performs two validations:
    - the type is not a keyword;
    - the type is a valid name, i.e. it matches the name regular expression.

    The type name 'object' is allowed.

    :param s: the input string to be parsed
    :return: the parsed type name
    """
    _check_not_a_keyword(s, "type", ignore={Symbols.OBJECT.value})
    return name(s)


def parse_function(s: str) -> name:
    """
    Parse a function name from a string.

    It performs two validations:
    - the function name is not a keyword;
    - the function name is a valid name, i.e. it matches the name regular expression.

    The function name 'total-cost' is allowed.

    :param s: the input string to be parsed
    :return: the parsed name
    """
    _check_not_a_keyword(s, "name", ignore={Symbols.TOTAL_COST.value})
    return name(s)


def to_names(names: Collection[namelike]) -> List[name]:
    """From name-like sequence to list of names."""
    return list(map(parse_name, names))


def to_type(names: Collection[namelike]) -> List[name]:
    """From name-like sequence to list of type names."""
    return list(map(parse_type, names))


def to_types(names: Dict[namelike, Optional[namelike]]) -> Dict[name, Optional[name]]:
    """From name-like dictionary to name dictionary."""
    return {
        parse_type(type_): parse_type(ancestor) if ancestor else None
        for type_, ancestor in names.items()
    }


def _is_a_keyword(word: str, ignore: Optional[AbstractSet[str]] = None) -> bool:
    """Check that the word is not a keyword."""
    ignore_set = ensure_set(ignore)
    # we remove the TOTAL_COST because it is not a keyword but a special function
    return word not in ignore_set and word in ALL_SYMBOLS


def _check_not_a_keyword(
    input_name: str,
    item_type: str,
    ignore: AbstractSet[str],
) -> None:
    """Check that the item name is not a keyword."""
    if _is_a_keyword(input_name, ignore=ignore):
        raise PDDLValidationError(
            f"invalid {item_type} '{input_name}': it is a keyword"
        )
