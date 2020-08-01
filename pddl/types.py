# -*- coding: utf-8 -*-
"""This module defines useful custom types."""

import re

from pddl.helpers import RegexConstrainedString


class name(RegexConstrainedString):
    """
    This type represents a 'name' in a PDDL file.

    It must match the following regex: "[A-Za-z][-_A-Za-z0-9]*".
    """

    REGEX = re.compile("[A-Za-z][-_A-Za-z0-9]*")
