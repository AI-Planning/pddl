# -*- coding: utf-8 -*-

"""Helper functions."""


import re
from typing import AbstractSet, Callable, Collection, Optional, Sequence


def _assert(condition: bool, message: str = ""):
    """User-defined assert."""
    if not condition:
        raise AssertionError(message)


def ensure_set(arg: Optional[Collection], immutable: bool = True) -> AbstractSet:
    """
    Ensure the argument is a set.

    :param arg: the set, or None.
    :param immutable: whether the collection should be immutable.
    :return: the same set, or an empty set if the arg was None.
    """
    op = frozenset if immutable else set
    return op(arg) if arg is not None else op()


def ensure_sequence(arg: Optional[Sequence], immutable: bool = True) -> Sequence:
    """
    Ensure the argument is a sequence.

    :param arg: the list, or None.
    :param immutable: whether the collection should be immutable.
    :return: the same list, or an empty list if the arg was None.
    """
    op: Callable = tuple if immutable else list
    return op(arg) if arg is not None else op()


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
