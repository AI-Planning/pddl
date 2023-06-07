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

"""Implementation of the PDDL problem parser."""
import sys
from typing import Dict

from lark import Lark, ParseError, Transformer

from pddl.core import Problem
from pddl.helpers.base import assert_
from pddl.logic.base import And, Not
from pddl.logic.predicates import EqualTo, Predicate
from pddl.logic.terms import Constant
from pddl.parser import PARSERS_DIRECTORY, PROBLEM_GRAMMAR_FILE
from pddl.parser.domain import DomainTransformer
from pddl.parser.symbols import Symbols
from pddl.requirements import Requirements


class ProblemTransformer(Transformer):
    """Problem Transformer."""

    def __init__(self):
        """Initialize the problem transformer."""
        super().__init__()

        self._domain_transformer = DomainTransformer()
        self._objects_by_name: Dict[str, Constant] = {}

    def start(self, args):
        """Process the rule 'start'."""
        return args[0]

    def problem(self, args):
        """Process the 'problem' rule."""
        args = [arg for arg in args if arg is not None]
        assert_(
            (args[0].value + args[1].value + args[-1].value == "(define)"),
            "Problem should start with '(define' and close with ')'",
        )
        return Problem(**dict(args[2:-1]))

    def problem_def(self, args):
        """Process the 'problem_def' rule."""
        return "name", args[2]

    def problem_domain(self, args):
        """Process the 'problem_domain' rule."""
        return "domain_name", args[2]

    def problem_requirements(self, args):
        """Process the 'requirements' rule."""
        return "requirements", {Requirements(r[1:]) for r in args[2:-1]}

    def objects(self, args):
        """Process the 'problem_domain' rule."""
        object_names = args[2]
        self._objects_by_name = {
            name: Constant(name, type_tag=type_) for name, type_ in object_names.items()
        }
        return "objects", list(self._objects_by_name.values())

    def typed_list_name(self, args):
        """
        Process the 'typed_list_name' rule.

        Return a dictionary with as keys the names and as value a set of types for each name.

        :param args: the argument of this grammar rule
        :return: a typed list (name)
        """
        return self._domain_transformer.typed_list_name(args)

    def domain__type_def(self, names):
        """Process a domain type def."""
        assert_(len(names) == 1)
        return str(names[0])

    def init(self, args):
        """Process the 'init' rule."""
        return "init", args[2:-1]

    def literal_name(self, args):
        """Process the 'literal_name' rule."""
        if len(args) == 1:
            return args[0]
        elif args[1] == Symbols.NOT.value:
            return Not(args[2])
        else:
            raise ParseError

    def goal(self, args):
        """Process the 'goal' rule."""
        return "goal", args[2]

    def gd_name(self, args):
        """Process the 'gd_name' rule."""
        if len(args) == 1:
            return args[0]
        elif args[1] == Symbols.NOT.value:
            return Not(args[2])
        elif args[1] == Symbols.AND.value:
            return And(*args[2:-1])
        else:
            raise ParseError

    def atomic_formula_name(self, args):
        """Process the 'atomic_formula_name' rule."""
        if args[1] == Symbols.EQUAL.value:
            obj1 = self._objects_by_name.get(args[1])
            obj2 = self._objects_by_name.get(args[2])
            return EqualTo(obj1, obj2)
        else:
            name = args[1]
            terms = [
                Constant(str(_term_name))
                if self._objects_by_name.get(str(_term_name)) is None
                else self._objects_by_name.get(str(_term_name))
                for _term_name in args[2:-1]
            ]
            return Predicate(name, *terms)


_problem_parser_lark = PROBLEM_GRAMMAR_FILE.read_text()


class ProblemParser:
    """PDDL problem parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = ProblemTransformer()
        self._parser = Lark(
            _problem_parser_lark, parser="lalr", import_paths=[PARSERS_DIRECTORY]
        )

    def __call__(self, text):
        """Call."""
        sys.tracebacklimit = 0  # noqa
        tree = self._parser.parse(text)
        sys.tracebacklimit = None  # noqa
        formula = self._transformer.transform(tree)
        return formula
