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

"""This module contains tests for the library custom types."""

import pytest

from pddl.custom_types import name


def test_name_string():
    """Test the 'name' string subclass defined in pddl.types."""
    a = name("a")
    assert a == "a"


def test_name_constructor_twice():
    """Test that the name constructor is idempotent."""
    a0 = "a"
    a1 = name(a0)
    a2 = name(a1)
    assert a0 == a1 == a2


def test_name_empty_string():
    """Test that providing an empty string to name constructor raises error."""
    with pytest.raises(ValueError):
        name("")


def test_name_starts_with_digits():
    """Test that providing a string to name constructor starting with digits raises error."""
    with pytest.raises(ValueError):
        name("123")
