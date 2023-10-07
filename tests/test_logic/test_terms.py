#
# Copyright 2021-2023 WhiteMech
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

from pddl.logic.terms import Variable


def test_no_duplicated_type_tags() -> None:
    """Test that no duplicated type tags are allowed."""
    with pytest.raises(
        ValueError, match=r"duplicate element in collection \['b', 'b'\]: 'b'"
    ):
        Variable("a", ["b", "b"])
