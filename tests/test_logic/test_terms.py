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

"""Test pddl.logic.terms module."""

import pytest

from pddl.logic.terms import Constant, Variable


def test_no_duplicated_type_tags() -> None:
    """Test that no duplicated type tags are allowed."""
    with pytest.raises(
        ValueError, match=r"duplicate element in collection \['b', 'b'\]: 'b'"
    ):
        Variable("a", ["b", "b"])


def test_variable_ordering() -> None:
    """Test variable ordering."""
    v1 = Variable("x", ["type1"])
    v2 = Variable("y", ["type3"])
    v3 = Variable("z", ["type2"])
    assert v1 < v2
    assert v1 < v3
    assert v2 < v3
    assert not (v1 > v2)
    assert not (v1 > v3)
    assert not (v2 > v3)


def test_constant_ordering() -> None:
    """Test constant ordering."""
    c1 = Constant("a", "type1")
    c2 = Constant("b", "type2")
    c3 = Constant("c", "type1")
    assert c1 < c2
    assert c1 < c3
    assert c2 < c3
    assert not (c1 > c2)
    assert not (c1 > c3)
    assert not (c2 > c3)


def test_term_ordering() -> None:
    """Test term ordering between variables and constants."""
    v1 = Variable("x", ["type1"])
    v2 = Variable("y", ["type3"])
    v3 = Variable("z", ["type2"])
    c1 = Constant("a", "type1")
    c2 = Constant("b", "type2")
    c3 = Constant("c", "type1")
    assert c1 < v1
    assert c2 < v2
    assert c3 < v3
    assert not (v1 < c1)
    assert not (v2 < c2)
    assert not (v3 < c3)
    array = [c1, c2, c3, v1, v2, v3]
    assert array == sorted(array)
