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

"""This module contains a utility function to propagate type tags of variables correctly along the formula."""
import copy
from functools import singledispatch
from typing import FrozenSet, Mapping, Optional

from pddl.custom_types import namelike
from pddl.logic.base import BinaryOp, Formula, QuantifiedCondition, UnaryOp
from pddl.logic.functions import BinaryFunction, NumericFunction, NumericValue
from pddl.logic.predicates import EqualTo as EqualToPredicate
from pddl.logic.predicates import Predicate
from pddl.logic.terms import Term, Variable


def update_type_tags(
    formula: Formula,
    var_to_types: Mapping[str, Optional[FrozenSet[namelike]]],
):
    """Update type tags of variables in a formula."""
    return _update_type_tags(formula, var_to_types)


@singledispatch
def _update_type_tags(
    formula: Formula,
    var_to_types: Mapping[str, Optional[FrozenSet[namelike]]],
):
    raise NotImplementedError(f"Formula of type {type(formula)} cannot be processed.")


@_update_type_tags.register
def _update_type_tags_binary_op(
    formula: BinaryOp, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
):
    new_operands = []
    for operand in formula.operands:
        new_operand = _update_type_tags(operand, var_to_types)
        new_operands.append(new_operand)
    return formula.__class__(*new_operands)


@_update_type_tags.register
def _update_type_tags_unary_op(
    formula: UnaryOp, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
):
    new_operand = _update_type_tags(formula.argument, var_to_types)
    return formula.__class__(new_operand)


@_update_type_tags.register
def _update_type_tags_quantified_condition(
    formula: QuantifiedCondition,
    var_to_types: Mapping[str, Optional[FrozenSet[namelike]]],
):
    new_var_to_types = dict(copy.copy(var_to_types))
    for var in formula.variables:
        new_var_to_types[var.name] = frozenset(var.type_tags)
    new_operand = _update_type_tags(formula.condition, new_var_to_types)
    return formula.__class__(new_operand, list(formula.variables))


@_update_type_tags.register
def _update_type_tags_equal_to_predicate(
    formula: EqualToPredicate,
    var_to_types: Mapping[str, Optional[FrozenSet[namelike]]],
):
    left_operand = _replace_term_with_type_tags_if_var(formula.left, var_to_types)
    right_operand = _replace_term_with_type_tags_if_var(formula.right, var_to_types)
    return EqualToPredicate(left_operand, right_operand)


@_update_type_tags.register
def _update_type_tags_predicate(
    formula: Predicate, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
):
    new_terms = []
    for term in formula.terms:
        new_terms.append(_replace_term_with_type_tags_if_var(term, var_to_types))
    return Predicate(formula.name, *new_terms)


@_update_type_tags.register
def _update_type_tags_numeric_function(
    formula: NumericFunction, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
):
    new_terms = []
    for term in formula.terms:
        new_terms.append(_replace_term_with_type_tags_if_var(term, var_to_types))
    return NumericFunction(formula.name, *new_terms)


@_update_type_tags.register
def _update_type_tags_numeric_value(
    formula: NumericValue, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
):
    return copy.copy(formula)


@_update_type_tags.register
def _update_type_tags_binary_function(
    formula: BinaryFunction, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
):
    new_operands = []
    for operand in formula.operands:
        new_operand = _update_type_tags(operand, var_to_types)
        new_operands.append(new_operand)
    return formula.__class__(*new_operands)


def _replace_term_with_type_tags_if_var(
    term: Term, var_to_types: Mapping[str, Optional[FrozenSet[namelike]]]
) -> Term:
    if isinstance(term, Variable) and term.name in var_to_types:
        return term.with_type_tags(var_to_types[term.name])
    return term
