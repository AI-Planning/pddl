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

"""This module contains functions to simplify formulas creation."""

from typing import Collection, List, Optional

from pddl.custom_types import namelike
from pddl.helpers.base import ensure_set
from pddl.logic.terms import Constant, Variable


def variables(s: str, types: Optional[Collection[namelike]] = None) -> List[Variable]:
    """
    Return a list of terms.

    >>> variables("a b c", types=["type_1", "type_2"])
    [Variable(a), Variable(b), Variable(c)]

    :param s: a string with space-separated valid names.
    :param types: a list of types.
    :return: a list of variables.
    """
    types = ensure_set(types)
    return [Variable(x, types) for x in s.split()]


def constants(s: str, type_: Optional[namelike] = None) -> List[Constant]:
    """
    Return a list of constants.

    >>> constants("a b c")
    [Constant(a), Constant(b), Constant(c)]

    :param s: a string with space-separated valid names.
    :param type_: the type.
    :return: a list of constants.
    """
    return [Constant(x, type_) for x in s.split()]
