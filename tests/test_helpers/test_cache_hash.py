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

"""Test the cache hash class decorator."""
import pickle  # nosec

from pddl.helpers.cache_hash import cache_hash


@cache_hash
class MyHashable:
    """A test class to test 'Hashable' metaclass."""

    def __init__(self):
        """Initialize."""
        super().__init__()
        self.a = "a"
        self.b = "b"

    def __hash__(self):
        """Compute the hash."""
        return hash((self.a, self.b))


def test_hashable():
    """Test the hashable class."""
    obj = MyHashable()

    assert not hasattr(obj, "__hash")

    h1 = hash(obj)
    h2 = hash(obj)
    assert h1 == h2

    assert hasattr(obj, "__hash")
    assert obj.__hash == h1 == h2

    dumped_obj = pickle.dumps(obj)  # nosec
    actual_obj = pickle.loads(dumped_obj)  # nosec
    assert not hasattr(actual_obj, "__hash")
