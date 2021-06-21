# -*- coding: utf-8 -*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl.  If not, see <https://www.gnu.org/licenses/>.
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
