# -*- coding: utf-8 -*-
#
# Copyright 2021-2022 WhiteMech
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
