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

"""Test the base helpers."""
from pddl.helpers.base import find_cycle


def test_find_cycle_empty_graph() -> None:
    """Test 'find_cycle' function, empty graph."""
    assert find_cycle({}) is None


def test_find_cycle_negative_case() -> None:
    """Test 'find_cycle' function, negative case."""
    a, b, c, d = "a b c d".split()
    assert find_cycle({a: {b}, b: {c}, d: {c}}) is None


def test_find_cycle_positive_case() -> None:
    """Test 'find_cycle' function, positive case."""
    a, b, c, d = "a b c d".split()
    assert find_cycle({a: {b}, b: {c}, d: {c}, c: {a}}) == ["a", "b", "c"]
