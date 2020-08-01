# -*- coding: utf-8 -*-
"""This module contains functions to simplify formulas creation."""

from typing import List

from pddl.logic.terms import Constant, Variable


def variables(s: str) -> List[Variable]:
    """
    Return a list of variables.

    >>> variables("a b c")
    [Variable(a), Variable(b), Variable(c)]

    :param s: a string with space-separated valid names.
    :return: a list of variables.
    """
    return list(map(Variable, s.split()))


def constants(s: str) -> List[Constant]:
    """
    Return a list of constants.

    >>> constants("a b c")
    [Constant(a), Constant(b), Constant(c)]

    :param s: a string with space-separated valid names.
    :return: a list of constants.
    """
    return list(map(Constant, s.split()))
