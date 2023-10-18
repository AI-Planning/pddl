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

"""This module contains the definition to deal with symbols."""
from enum import Enum
from typing import Set

OpSymbol = str
OpRequirement = str


class Symbols(Enum):
    """A set of symbols that can be used in PDDL."""

    ROUND_BRACKET_LEFT = "("
    ROUND_BRACKET_RIGHT = ")"
    TYPE_SEP = "-"
    EQUAL = "="
    ACTION = ":action"
    AND = "and"
    CONSTANTS = ":constants"
    DEFINE = "define"
    DERIVED = ":derived"
    DOMAIN = "domain"
    DOMAIN_P = ":domain"
    EFFECT = ":effect"
    EITHER = "either"
    EXISTS = "exists"
    FORALL = "forall"
    GOAL = ":goal"
    IMPLY = "imply"
    INIT = ":init"
    NOT = "not"
    OBJECT = "object"
    OBJECTS = ":objects"
    ONEOF = "oneof"
    OR = "or"
    PARAMETERS = ":parameters"
    PRECONDITION = ":precondition"
    PREDICATES = ":predicates"
    PROBLEM = "problem"
    REQUIREMENTS = ":requirements"
    TYPES = ":types"
    METRIC = ":metric"
    WHEN = "when"
    GREATER_EQUAL = ">="
    GREATER = ">"
    LESSER_EQUAL = "<="
    LESSER = "<"
    MINUS = "-"
    PLUS = "+"
    TIMES = "*"
    DIVIDE = "/"
    ASSIGN = "assign"
    SCALE_UP = "scale-up"
    SCALE_DOWN = "scale-down"
    INCREASE = "increase"
    DECREASE = "decrease"
    MAXIMIZE = "maximize"
    MINIMIZE = "minimize"
    TOTAL_COST = "total-cost"


ALL_SYMBOLS: Set[str] = {v.value for v in Symbols}
BINARY_COMP_SYMBOLS: Set[str] = {
    v.value for v in Symbols if v.value in {">=", ">", "<=", "<", "="}
}


class RequirementSymbols(Enum):
    """A set of requirements that can be used in PDDL."""

    STRIPS = ":strips"
    TYPING = ":typing"
    NEG_PRECONDITION = ":negative-preconditions"
    DIS_PRECONDITION = ":disjunctive-preconditions"
    UNIVERSAL_PRECONDITION = ":universal-preconditions"
    EXISTENTIAL_PRECONDITION = ":existential-preconditions"
    QUANTIFIED_PRECONDITION = ":quantified-preconditions"
    EQUALITY = ":equality"
    CONDITIONAL_EFFECTS = ":conditional-effects"
    ADL = ":adl"
    DERIVED_PREDICATES = ":derived-predicates"
    NON_DETERMINISTIC = ":non-deterministic"
    FLUENTS = ":fluents"
    OBJECT_FLUENTS = ":object-fluents"
    NUMERIC_FLUENTS = ":numeric-fluents"
    ACTION_COSTS = ":action-costs"

    def strip(self) -> str:
        """Strip the leading colon."""
        return self.value[1:]


ALL_REQUIREMENTS: Set[str] = {v.value for v in RequirementSymbols}
