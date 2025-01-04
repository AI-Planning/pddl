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

"""Test pddl.logic.predicates module."""
import pytest

from pddl.logic import Predicate
from pddl.logic.predicates import EqualTo
from pddl.logic.terms import Variable


def test_inconsistent_predicate_terms() -> None:
    """Test that terms of a predicate must have consistent typing."""
    with pytest.raises(
        ValueError,
        match=r"Term \?a has inconsistent type tags: previous type tags \['t1', 't2'\], new type tags "
        r"\['t3', 't4'\]",
    ):
        a1, a2 = Variable("a", ["t1", "t2"]), Variable("a", ["t3", "t4"])
        Predicate("p", a1, a2)


def test_inconsistent_equal_to_terms() -> None:
    """Test that terms of a EqualTo atomic must have consistent typing."""
    with pytest.raises(
        ValueError,
        match=r"Term \?a has inconsistent type tags: previous type tags \['t1', 't2'\], new type tags "
        r"\['t3', 't4'\]",
    ):
        a1, a2 = Variable("a", ["t1", "t2"]), Variable("a", ["t3", "t4"])
        EqualTo(a1, a2)
