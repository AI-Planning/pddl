# -*- coding: utf-8 -*-

"""Helper functions."""


import re
from typing import Optional, Set, List, Any


def _assert(condition: bool, message: str = ""):
    """User-defined assert."""
    if condition:
        raise AssertionError(message)


def _ensure(arg: Optional[Any], default: Any):
    """Ensure the argument is not None, else return the default."""
    return arg if arg is not None else default


def ensure_set(s: Optional[Set]) -> Set:
    """
    Ensure the argument is a set.

    :param s: the set, or None.
    :return: the same set, or an empty set if the arg was None.
    """
    return _ensure(s, set())


def ensure_list(s: Optional[List]) -> List:
    """
    Ensure the argument is a list.

    :param s: the list, or None.
    :return: the same list, or an empty list if the arg was None.
    """
    return _ensure(s, list())


class RegexConstrainedString(str):
    """
    A string that is constrained by a regex.

    The default behaviour is to match anything.
    Subclass this class and change the 'REGEX' class
    attribute to implement a different behaviour.
    """

    REGEX = re.compile(".*", flags=re.DOTALL)

    def __new__(cls, value, *args, **kwargs):
        """Instantiate a new object."""
        if type(value) == cls:
            return value
        else:
            inst = super(RegexConstrainedString, cls).__new__(cls, value)
            return inst

    def __init__(self, *_, **__):
        """Initialize a regex constrained string."""
        super().__init__()
        if not self.REGEX.match(self):
            self._handle_no_match()

    def _handle_no_match(self):
        raise ValueError(
            "Value '{data}' does not match the regular expression {regex}".format(
                data=self, regex=self.REGEX
            )
        )
