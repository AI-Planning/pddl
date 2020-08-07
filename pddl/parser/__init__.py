# -*- coding: utf-8 -*-
"""This module contains the implementation of the parsers for the supported PDDL formalisms."""

from pddl import _ROOT_PATH

DOMAIN_GRAMMAR_FILE = _ROOT_PATH / "parser" / "domain.lark"
PROBLEM_GRAMMAR_FILE = _ROOT_PATH / "parser" / "problem.lark"
