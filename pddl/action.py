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

"""This module contains the definition of the PDDL action class."""
from typing import Dict, Optional, Sequence

from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, parse_name
from pddl.helpers.base import _typed_parameters, ensure_sequence
from pddl.logic import Variable
from pddl.logic.base import Formula
from pddl.logic.predicates import _check_terms_consistency
from pddl.logic.terms import Term


class Action:
    """A class for the PDDL Action."""

    def __init__(
        self,
        name: namelike,
        parameters: Sequence[Variable],
        precondition: Optional[Formula] = None,
        effect: Optional[Formula] = None,
    ):
        """
        Initialize the action.

        :param name: the action name.
        :param parameters: the action parameters.
        :param precondition: the action precondition.
        :param effect: the action effect.
        """
        self._name: name = parse_name(name)
        self._parameters: Sequence[Variable] = ensure_sequence(parameters)
        self._precondition = precondition
        self._effect = effect

        self._check_consistency()

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Sequence[Variable]:
        """Get the parameters."""
        return self._parameters

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return self.parameters

    @property
    def precondition(self) -> Optional[Formula]:
        """Get the precondition."""
        return self._precondition

    @property
    def effect(self) -> Optional[Formula]:
        """Get the effect."""
        return self._effect

    def __str__(self):
        """Get the string."""
        operator_str = "(:action {0}\n".format(self.name)
        operator_str += f"    :parameters ({_typed_parameters(self.parameters)})\n"
        if self.precondition is not None:
            operator_str += f"    :precondition {str(self.precondition)}\n"
        if self.effect is not None:
            operator_str += f"    :effect {str(self.effect)}\n"
        operator_str += ")"
        return operator_str

    def __eq__(self, other):
        """Check equality between two Actions."""
        return (
            isinstance(other, Action)
            and self.name == other.name
            and self.parameters == other.parameters
            and self.precondition == other.precondition
            and self.effect == other.effect
        )

    def __hash__(self):
        """Get the hash."""
        return hash((self.name, self.parameters, self.precondition, self.effect))

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return (
            f"{type(self).__name__}({self.name}, parameters={', '.join(map(str, self.parameters))}, "
            f"precondition={self.precondition}, effect={self.effect})"
        )

    def _check_consistency(self) -> None:
        """
        Check the consistency of the action definition.

        Note that a PDDL action does not have all the domain information to be validated. In particular, it does not
        have types information. Therefore, this method only checks the consistency of the action definition.
        """
        _check_terms_consistency(self._parameters)
        types_dict: Dict[name_type, None] = {
            t: None for p in self._parameters for t in p.type_tags
        }
