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

"""This module contains the implementation of the parsers for the supported PDDL formalisms."""

from pddl import _ROOT_PATH

PARSERS_DIRECTORY = _ROOT_PATH / "parser"
DOMAIN_GRAMMAR_FILE = PARSERS_DIRECTORY / "domain.lark"
PROBLEM_GRAMMAR_FILE = PARSERS_DIRECTORY / "problem.lark"
