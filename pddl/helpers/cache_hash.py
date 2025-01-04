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

"""Base classes for pylogics logic formulas."""
from functools import wraps
from typing import Any, Callable, cast


def _cache_hash(fn) -> Callable[[Any], int]:
    """
    Compute the (possibly memoized) hash.

    If the hash for this object has already
    been computed, return it. Otherwise,
    compute it and store for later calls.

    :param fn: the hashing function.
    :return: the new hashing function.
    """

    @wraps(fn)
    def __hash__(self):
        if not hasattr(self, "__hash"):
            self.__hash = fn(self)
        return cast(int, self.__hash)

    return __hash__


def _getstate(fn):
    """
    Get the state.

    We need to ignore the hash value because in case the object
    is serialized with e.g. Pickle, if the state is restored
    with another instance of the interpreter, the stored hash might
    be inconsistent with the PYTHONHASHSEED initialization of the
    new interpreter.

    :param fn: the getstate function.
    :return: the new getstate function.
    """

    @wraps(fn)
    def __getstate__(self):
        d = fn(self)
        if d is None:
            return None
        # assuming d is a dictionary
        d.pop("__hash", None)
        return d

    return __getstate__


def default_getstate(self):
    """Implement the default getstate."""
    return self.__dict__


def default_setstate(self, state):
    """Implement the default getstate."""
    self.__dict__ = state


def _setstate(fn):
    """
    Set the state.

    The hash value needs to be set to None
    as the state might be restored in another
    interpreter in which the old hash value
    might not be consistent anymore.

    :param fn: the setstate function.
    :return: the new setstate function.
    """

    @wraps(fn)
    def __setstate__(self, state):
        fn(self, state)
        if hasattr(self, "__hash"):
            delattr(self, "__hash")

    return __setstate__


def cache_hash(cls):
    """
    Make instances of a class to cache their hash.

    This class decorator sets:
        __hash__
        __getstate__
        __setstate__

    :param cls: the class to wrap
    :return: the wrapped class
    """
    cls.__hash__ = _cache_hash(cls.__hash__)

    getstate_fn = cls.__getstate__ if hasattr(cls, "__getstate__") else default_getstate
    cls.__getstate__ = _getstate(getstate_fn)

    setstate_fn = cls.__setstate__ if hasattr(cls, "__setstate__") else default_setstate
    cls.__setstate__ = _setstate(setstate_fn)
    return cls
