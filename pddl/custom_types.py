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

"""This module defines useful custom types."""

import re
from typing import Collection, List, Union

from pddl.helpers.base import RegexConstrainedString


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
