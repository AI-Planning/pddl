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

"""Implementation of the PDDL domain parser."""
import sys
from typing import AbstractSet, Dict, List, Mapping, Optional, Sequence, Set, Tuple

from lark import Lark, ParseError, Transformer

from pddl.core import Action, Domain, Requirements
from pddl.exceptions import PDDLMissingRequirementError, PDDLParsingError
from pddl.helpers.base import assert_, safe_index
from pddl.logic.base import (
    And,
    ExistsCondition,
    FalseFormula,
    ForallCondition,
    Imply,
    Not,
    OneOf,
    Or,
)
from pddl.logic.effects import AndEffect, Forall, When
from pddl.logic.predicates import DerivedPredicate, EqualTo, Predicate
from pddl.logic.terms import Constant, Variable
from pddl.parser import DOMAIN_GRAMMAR_FILE, PARSERS_DIRECTORY
from pddl.parser.symbols import Symbols
from pddl.parser.types_index import TypesIndex


class DomainTransformer(Transformer):
    """Domain Transformer."""

    def __init__(self, *args, **kwargs):
        """Initialize the domain transformer."""
        super().__init__(*args, **kwargs)

        self._constants_by_name: Dict[str, Constant] = {}
        self._predicates_by_name: Dict[str, Predicate] = {}
        self._current_parameters_by_name: Dict[str, Variable] = {}
        self._requirements: Set[str] = set()
        self._extended_requirements: Set[str] = set()

    def start(self, args):
        """Entry point."""
        return args[0]

    def domain(self, args):
        """Process the 'domain' rule."""
        args = [arg for arg in args if arg is not None]
        kwargs = {}
        actions = []
        derived_predicates = []
        for arg in args[2:-1]:
            if isinstance(arg, Action):
                actions.append(arg)
            elif isinstance(arg, DerivedPredicate):
                derived_predicates.append(arg)
            else:
                assert_(isinstance(arg, dict))
                kwargs.update(arg)
        kwargs.update(actions=actions, derived_predicates=derived_predicates)
        return Domain(**kwargs)

    def domain_def(self, args):
        """Process the 'domain_def' rule."""
        return dict(name=args[2])

    def requirements(self, args):
        """Process the 'requirements' rule."""
        self._requirements = {Requirements(r[1:]) for r in args[2:-1]}

        self._extended_requirements = set(self._requirements)
        if Requirements.STRIPS in self._requirements:
            self._extended_requirements.update(Requirements.strips_requirements())

        return dict(requirements=self._requirements)

    def types(self, args):
        """Parse the 'types' rule."""
        has_typing_requirement = self._has_requirement(Requirements.TYPING)
        types_definition = args[2]
        have_type_hierarchy = any(types_definition.values())
        if have_type_hierarchy and not has_typing_requirement:
            raise PDDLMissingRequirementError(Requirements.TYPING)
        return dict(types=types_definition)

    def constants(self, args):
        """Process the 'constant_def' rule."""
        self._constants_by_name = {
            name: Constant(name, type_tags) for name, type_tags in args[2].items()
        }
        return dict(constants=list(self._constants_by_name.values()))

    def predicates(self, args):
        """Process the 'predicates' rule."""
        predicates = args[2:-1]
        self._predicates_by_name = {p.name: p for p in predicates}
        return dict(predicates=predicates)

    def action_def(self, args):
        """Process the 'action_def' rule."""
        name = args[2]
        variables = args[4]

        # process action body
        _children = args[5].children
        action_body = {
            _children[i][1:]: _children[i + 1] for i in range(0, len(_children), 2)
        }
        return Action(name, variables, **action_body)

    def derived_predicates(self, args):
        """Process the 'derived_predicates' rule."""
        predicate = args[2]
        condition = args[3]
        return DerivedPredicate(predicate, condition)

    def action_parameters(self, args):
        """Process the 'action_parameters' rule."""
        self._current_parameters_by_name = {
            name: Variable(name, tags) for name, tags in args[1].items()
        }
        return list(self._current_parameters_by_name.values())

    def emptyor_pregd(self, args):
        """Process the 'emptyor_pregd' rule."""
        if len(args) == 2:
            return FalseFormula()
        else:
            assert_(len(args) == 1)
            return args[0]

    def gd_not(self, args):
        """Process the 'gd' not rule."""
        if not bool(
            {Requirements.NEG_PRECONDITION, Requirements.ADL}
            & self._extended_requirements
        ):
            # raise PDDLMissingRequirementError(Requirements.NEG_PRECONDITION)
            # TODO temporary change; remove
            pass
        return Not(args[2])

    def gd_and(self, args):
        """Process the 'gd_and' rule."""
        operands = args[2:-1]
        return And(*operands)

    def gd_or(self, args):
        """Process the 'gd' or rule."""
        if not bool(
            {Requirements.DIS_PRECONDITION, Requirements.ADL}
            & self._extended_requirements
        ):
            raise PDDLMissingRequirementError(Requirements.DIS_PRECONDITION)
        operands = args[2:-1]
        return Or(*operands)

    def gd_imply(self, args):
        """Process the 'gd' imply rule."""
        if not bool(
            {Requirements.DIS_PRECONDITION, Requirements.ADL}
            & self._extended_requirements
        ):
            raise PDDLMissingRequirementError(Requirements.DIS_PRECONDITION)
        return Imply(args[2], args[3])

    def gd_quantifiers(self, args):
        """Process the 'gd' quantifiers rule."""
        req, cond_class = {
            Symbols.FORALL.value: (
                Requirements.UNIVERSAL_PRECONDITION,
                ForallCondition,
            ),
            Symbols.EXISTS.value: (
                Requirements.EXISTENTIAL_PRECONDITION,
                ExistsCondition,
            ),
        }[args[1]]
        if not bool(
            {req, Requirements.QUANTIFIED_PRECONDITION, Requirements.ADL}
            & self._extended_requirements
        ):
            raise PDDLMissingRequirementError(req)
        variables = [Variable(name, tags) for name, tags in args[3].items()]
        condition = args[5]
        return cond_class(cond=condition, variables=variables)

    def gd(self, args):
        """Process the 'gd' rule."""
        if len(args) == 1:
            return args[0]
        elif args[1] == Symbols.NOT.value:
            return self.gd_not(args)
        elif args[1] == Symbols.AND.value:
            return self.gd_and(args)
        elif args[1] == Symbols.OR.value:
            return self.gd_or(args)
        elif args[1] == Symbols.IMPLY.value:
            return self.gd_imply(args)
        elif args[1] in [Symbols.FORALL.value, Symbols.EXISTS.value]:
            return self.gd_quantifiers(args)

    def emptyor_effect(self, args):
        """Process the 'emptyor_effect' rule."""
        if len(args) == 2:
            return FalseFormula()
        else:
            return args[0]

    def effect(self, args):
        """Process the 'effect' rule."""
        if len(args) == 1:
            return args[0]
        if args[1] == Symbols.AND.value:
            return AndEffect(*args[2:-1])
        raise ValueError("case not recognized")

    def c_effect(self, args):
        """Process the 'c_effect' rule."""
        if len(args) == 1:
            return args[0]
        if args[1] == Symbols.FORALL.value:
            variables = [Variable(name, tags) for name, tags in args[3].items()]
            return Forall(effect=args[-2], variables=variables)
        if args[1] == Symbols.WHEN.value:
            return When(args[2], args[3])
        if args[1] == Symbols.ONEOF.value:
            if not bool({Requirements.NON_DETERMINISTIC} & self._extended_requirements):
                raise PDDLMissingRequirementError(Requirements.NON_DETERMINISTIC)
            return OneOf(*args[2:-1])
        raise ValueError()

    def p_effect(self, args):
        """Process the 'p_effect' rule."""
        if len(args) == 1:
            return args[0]
        else:
            return Not(args[2])

    def cond_effect(self, args):
        """Process the 'cond_effect' rule."""
        if len(args) >= 3 and args[1] == Symbols.AND.value:
            p_effects = args[2:-1]
            return And(*p_effects)
        assert_(len(args) == 1)
        return args[0]

    def atomic_formula_term(self, args):
        """Process the 'atomic_formula_term' rule."""

        def constant_or_variable(t):
            # Case where the term is a free variable (bug) or comes from a parent quantifier
            if (
                not isinstance(t, Constant)
                and t not in self._current_parameters_by_name
            ):
                return Variable(str(t), {})
            return t if isinstance(t, Constant) else self._current_parameters_by_name[t]

        if args[1] == Symbols.EQUAL.value:
            if not bool({Requirements.EQUALITY} & self._extended_requirements):
                raise PDDLMissingRequirementError(Requirements.EQUALITY)
            left = constant_or_variable(args[2])
            right = constant_or_variable(args[3])
            return EqualTo(left, right)
        else:
            name = args[1]
            terms = list(map(constant_or_variable, args[2:-1]))
            return Predicate(name, *terms)

    def constant(self, args):
        """Process the 'constant' rule."""
        assert_(len(args) == 1, "Unexpected parsing error.")
        constant = self._constants_by_name.get(args[0], None)
        if constant is None:
            raise ParseError(f"Constant '{args[0]}' not defined.")
        return constant

    def atomic_formula_skeleton(self, args):
        """Process the 'atomic_formula_skeleton' rule."""
        name = args[1]
        variable_data: Dict[str, Set[str]] = args[2]
        variables = [Variable(name, tags) for name, tags in variable_data.items()]
        return Predicate(name, *variables)

    def typed_list_name(self, args) -> Dict[str, Optional[str]]:
        """Process the 'typed_list_name' rule."""
        types_index = TypesIndex.parse_typed_list(args)
        return types_index.get_typed_list_of_names()

    def typed_list_variable(self, args) -> Dict[str, Set[str]]:
        """
        Process the 'typed_list_variable' rule.

        Return a dictionary with as keys the terms and as value a set of types for each name.

        :param args: the argument of this grammar rule
        :return: a typed list (variable), i.e. a mapping from variables to the supported types
        """
        type_sep_index = safe_index(args, Symbols.TYPE_SEP.value)
        if type_sep_index is None:
            result = self._parse_simple_typed_list(args, check_for_duplicates=False)
            return {var: set() for var in result}

        # if we are here, the matched pattern is: [name_1 ... name_n], "-", type_def, other_typed_list_dict  # noqa
        # make sure there are only two tokens after "-"
        assert_(len(args[type_sep_index:]) == 3, "unexpected parser state")

        variables: Tuple[str, ...] = tuple(args[:type_sep_index])
        type_def: Set[str] = self._process_type_def(args[type_sep_index + 1])
        other_typed_list_dict: Mapping[str, Set[str]] = args[type_sep_index + 2]
        new_typed_list_dict: Mapping[str, Set[str]] = {v: type_def for v in variables}

        # check type conflicts
        self._check_duplicates(other_typed_list_dict.keys(), new_typed_list_dict.keys())

        return {**new_typed_list_dict, **other_typed_list_dict}

    def type_def(self, args):
        """Parse the 'type_def' rule."""
        return args if len(args) == 1 else args[1:-1]

    def _has_requirement(self, requirement: Requirements) -> bool:
        """Check whether a requirement is satisfied by the current state of the domain parsing."""
        return requirement in self._extended_requirements

    def _check_duplicates(
        self,
        other_names: AbstractSet[str],
        new_names: AbstractSet[str],
    ) -> None:
        names_intersection = new_names & other_names
        if len(names_intersection) != 0:
            names_list_as_strings = map(repr, map(str, names_intersection))
            names_list_str = ", ".join(sorted(names_list_as_strings))
            raise PDDLParsingError(
                f"detected conflicting items in a typed list: items occurred twice: [{names_list_str}]"
            )

    def _parse_simple_typed_list(
        self, args: Sequence[str], check_for_duplicates: bool = True
    ) -> Dict[str, Optional[str]]:
        """
        Parse a 'simple' typed list.

        In this simple case, there are no type specifications, i.e. just a list of items.

        If check_for_duplicates is True, a check for duplicates is performed.
        """
        # check for duplicates
        if check_for_duplicates and len(set(args)) != len(args):
            # find duplicates
            seen = set()
            dupes = [str(x) for x in args if x in seen or seen.add(x)]  # type: ignore
            raise PDDLParsingError(
                f"duplicate items {dupes} found in the typed list: {list(map(str, args))}'"
            )

        return {arg: None for arg in args}

    def _process_type_def(self, type_def: List[str]) -> Set[str]:
        """Process a raw type_def and return a set of types."""
        assert_(len(type_def) != 0, "unexpected parser state: empty type_def")

        if len(type_def) == 1:
            # single-typed type-def, return
            return set(type_def)

        # if we are here, type_def is of the form (either t1 ... tn)
        either_keyword, types = type_def[0], type_def[1:]
        assert_(str(either_keyword) == Symbols.EITHER.value)
        return set(types)


_domain_parser_lark = DOMAIN_GRAMMAR_FILE.read_text()


class DomainParser:
    """PDDL domain parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = DomainTransformer()
        self._parser = Lark(
            _domain_parser_lark, parser="lalr", import_paths=[PARSERS_DIRECTORY]
        )

    def __call__(self, text):
        """Call."""
        sys.tracebacklimit = 0  # noqa
        tree = self._parser.parse(text)
        sys.tracebacklimit = None  # noqa
        formula = self._transformer.transform(tree)
        return formula
