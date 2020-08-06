# -*- coding: utf-8 -*-
"""Implementation of the PDDL domain parser."""

from lark import Lark, Transformer

from pddl.parser import DOMAIN_GRAMMAR_FILE


class PLTransformer(Transformer):
    """PL Transformer."""

    def start(self, args):
        """Entry point."""
        return args[0]


class DomainParser:
    """PL Parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = PLTransformer()
        self._parser = Lark(DOMAIN_GRAMMAR_FILE.open(), parser="lalr")

    def __call__(self, text):
        """Call."""
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula
