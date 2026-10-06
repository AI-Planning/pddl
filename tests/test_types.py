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


def test_name_is_case_insensitive():
    """Test that names differing only by case are equal."""
    assert name("Counter") == name("counter")
    assert name("Counter") == "counter"
    assert "counter" == name("Counter")
    assert not (name("Counter") != name("counter"))


def test_name_case_insensitive_hash():
    """Test that case-insensitive names hash equally and deduplicate in sets."""
    assert hash(name("Counter")) == hash(name("counter"))
    assert len({name("Counter"), name("counter")}) == 1


def test_name_case_insensitive_ordering():
    """Test that ordering is consistent with case-insensitive equality."""
    a, b = name("Counter"), name("counter")
    assert not (a < b)
    assert not (b < a)
    assert a <= b
    assert b <= a


def test_name_preserves_original_case():
    """Test that the original spelling is preserved in output."""
    assert str(name("Counter")) == "Counter"


def test_name_comparisons_ignore_case():
    """Test all comparison operators on names are case-insensitive."""
    a, b = name("Counter"), name("counter")
    assert a == b
    assert not (a != b)
    assert not (a < b) and not (b < a)
    assert not (a > b) and not (b > a)
    assert a <= b and b <= a
    assert a >= b and b >= a


def test_name_comparisons_with_non_strings():
    """Test comparisons against non-strings (NotImplemented paths)."""
    a = name("Counter")
    assert not (a == 1)
    assert a != 1
    for op in (lambda: a < 1, lambda: a <= 1, lambda: a > 1, lambda: a >= 1):
        with pytest.raises(TypeError):
            op()
