# -*- coding: utf-8 -*-
"""Implementation of the PDDL domain parser."""

from pathlib import Path

from lark import Lark, Transformer

from ltlf2dfa.helpers import ParsingError
from ltlf2dfa.parser import CUR_DIR
from ltlf2dfa.pl import (
    PLNot,
    PLAtomic,
    PLOr,
    PLAnd,
    PLImplies,
    PLEquivalence,
    PLTrue,
    PLFalse,
)


class PLTransformer(Transformer):
    """PL Transformer."""

    def start(self, args):
        """Entry point."""
        return args[0]

    def propositional_formula(self, args):
        """Parse Propositional formula."""
        assert len(args) == 1
        return args[0]

    def prop_equivalence(self, args):
        """Parse Propositional Equivalence."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLEquivalence(subformulas)
        else:
            raise ParsingError

    def prop_implication(self, args):
        """Parse Propositional Implication."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLImplies(subformulas)
        else:
            raise ParsingError

    def prop_or(self, args):
        """Parse Propositional Or."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLOr(subformulas)
        else:
            raise ParsingError

    def prop_and(self, args):
        """Parse Propositional And."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLAnd(subformulas)
        else:
            raise ParsingError

    def prop_not(self, args):
        """Parse Propositional Not."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = PLNot(f)
            return f

    def prop_wrapped(self, args):
        """Parse Propositional wrapped formula."""
        if len(args) == 1:
            return args[0]
        elif len(args) == 3:
            _, f, _ = args
            return f
        else:
            raise ParsingError

    def prop_atom(self, args):
        """Parse Propositional Atom."""
        assert len(args) == 1
        return args[0]

    def prop_true(self, args):
        """Parse Propositional True."""
        assert len(args) == 1
        return PLTrue()

    def prop_false(self, args):
        """Parse Propositional False."""
        assert len(args) == 1
        return PLFalse()

    def atom(self, args):
        """Parse Atom."""
        assert len(args) == 1
        return PLAtomic(str(args[0]))


class PLParser:
    """PL Parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = PLTransformer()
        self._parser = Lark(open(str(Path(CUR_DIR, "pl.lark"))), parser="lalr")

    def __call__(self, text):
        """Call."""
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula


if __name__ == "__main__":
    parser = PLParser()
    while True:
        try:
            s = input("pl > ")
        except EOFError:
            break
        if not s:
            continue
        result = parser(s)
        print(result)
