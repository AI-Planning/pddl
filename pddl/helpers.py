# -*- coding: utf-8 -*-

"""Helper functions."""
from typing import Optional, Set


def ensure_set(s: Optional[Set]) -> Set:
    """
    Ensure the argument is a set.

    :param s: the set, or None.
    :return: the same set, or an empty set if the arg was None.
    """
    return s if s is not None else set()
