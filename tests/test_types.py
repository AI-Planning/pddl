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

"""This module contains tests for the library custom types."""

import pytest

from pddl.custom_types import name, parse_name, parse_type
from pddl.exceptions import PDDLValidationError
from pddl.parser.symbols import Symbols
from tests.conftest import TEXT_SYMBOLS


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


@pytest.mark.parametrize("keyword", TEXT_SYMBOLS)
def test_name_is_a_keyword(keyword):
    """Test that parse_name with keywords as input raises error."""
    with pytest.raises(
        PDDLValidationError, match=f"invalid name '{keyword}': it is a keyword"
    ):
        parse_name(keyword)


@pytest.mark.parametrize("keyword", TEXT_SYMBOLS - {Symbols.OBJECT.value})
def test_type_is_a_keyword(keyword):
    """Test that parse_type with keywords as input raises error."""
    with pytest.raises(
        PDDLValidationError, match=f"invalid type '{keyword}': it is a keyword"
    ):
        parse_type(keyword)


def test_object_is_a_valid_type_name():
    """Test that parse_type with input 'object' does not raise error."""
    parse_type(Symbols.OBJECT.value)
