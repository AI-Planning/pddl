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
