# -*- coding: utf-8 -*-
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Lydia is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Lydia.  If not, see <https://www.gnu.org/licenses/>.
#
"""This module contains the definition to deal with symbols."""
from enum import Enum
from typing import Set

OpSymbol = str
OpRequirement = str


class Symbols(Enum):
    """A set of symbols that can be used in PDDL."""

    DEFINE = "define"
    DOMAIN = "domain"
    PROBLEM = "problem"
    AND = "and"
    OR = "or"
    NOT = "not"
    IMPLY = "imply"
    ONEOF = "oneof"
    FORALL = "forall"
    EXISTS = "exists"
    WHEN = "when"
    DOMAIN_P = ":domain"
    OBJECTS = ":objects"
    INIT = ":init"
    GOAL = ":goal"
    REQUIREMENTS = ":requirements"
    CONSTANTS = ":constants"
    STRIPS = ":strips"
    ADL = ":adl"
    NON_DETERMINISTIC = ":non-deterministic"
    EQUALITY = ":equality"
    TYPING = ":typing"
    TYPES = ":types"
    PREDICATES = ":predicates"
    ACTION = ":action"
    PARAMETERS = ":parameters"
    PRECONDITION = ":precondition"
    EFFECT = ":effect"
    ROUND_BRACKET_LEFT = "("
    ROUND_BRACKET_RIGHT = ")"
    TYPE_SEP = "-"
    EQUAL = "="


ALL_SYMBOLS = {v.value for v in Symbols}  # type: Set[str]


class Requirements(Enum):
    """A set of requirements that can be used in PDDL."""

    STRIPS = ":strips"
    ADL = ":adl"
    NON_DETERMINISTIC = ":non-deterministic"
    NEG_PRECONDITION = ":negative-preconditions"
    EQUALITY = ":equality"
    TYPING = ":typing"


ALL_REQUIREMENTS = {v.value for v in Requirements}  # type: Set[str]
