# -*- coding: utf-8 -*-
#
# Copyright 2021-2022 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""Custom Exception classes."""

from pddl.core import Requirements


class PDDLError(Exception):
    """Base class for PDDL error."""


class PDDLParsingError(PDDLError):
    """Raised for PDDL parsing error."""

    def __init__(self, message: str = "Parsing Error"):
        """Initialize the PDDL parsing error exception."""
        self.message = message
        super().__init__(self.message)


class PDDLMissingRequirementError(PDDLParsingError):
    """Raised for PDDL missing requirement error."""

    def __init__(self, requirement: Requirements):
        """Initialize the PDDL missing requirement error exception."""
        self.requirement = requirement
        self.message = "Missing PDDL requirement"
        super().__init__(self.message)

    def __str__(self):
        """Get the string representation."""
        return f"{self.message}, {self.requirement} not found."
