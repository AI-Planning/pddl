# -*- coding: utf-8 -*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl.  If not, see <https://www.gnu.org/licenses/>.
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


def constants(s: str, types: Optional[Collection[namelike]] = None) -> List[Constant]:
    """
    Return a list of constants.

    >>> constants("a b c")
    [Constant(a), Constant(b), Constant(c)]

    :param s: a string with space-separated valid names.
    :param types: a list of types.
    :return: a list of constants.
    """
    types = ensure_set(types)
    return [Constant(x, types) for x in s.split()]
