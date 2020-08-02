# -*- coding: utf-8 -*-
"""This module contains tests for the library custom types."""

import pytest

from pddl.types import name


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
