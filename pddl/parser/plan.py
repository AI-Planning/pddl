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

"""Implementation of the PDDL plan parser."""

from typing import Any

from lark import Transformer

from pddl.core import Plan
from pddl.logic.terms import Constant
from pddl.parser.base import BaseParser


class PlanTransformer(Transformer[Any, Plan]):
    """Plan Transformer."""

    def __init__(self) -> None:
        """Initialize the plan transformer."""
        super().__init__()

    def ground_action(self, args):
        """Process the 'ground_action' rule."""
        action_name = args[1]
        if len(args) > 3:
            parameters = args[2:-1]
        else:
            parameters = []
        parameters = [Constant(str(param)) for param in parameters]
        return (action_name, parameters)

    def plan(self, args):
        """Process the 'plan' rule."""
        actions = args
        return Plan(actions=actions)


class PlanParser(BaseParser[Plan]):
    """A class for parsing PDDL plans."""

    transformer_cls = PlanTransformer
    start_symbol = "plan"
