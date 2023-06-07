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

"""
Core module of the package.

It contains the class definitions to build and modify PDDL domains or problems.
"""
from operator import xor
from typing import AbstractSet, Collection, Dict, Optional, Sequence, cast

from pddl._validation import (
    TypeChecker,
    Types,
    _check_types_in_has_terms_objects,
    validate,
)
from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names, to_names_types  # noqa: F401
from pddl.helpers.base import (
    _typed_parameters,
    assert_,
    ensure,
    ensure_sequence,
    ensure_set,
)
from pddl.logic.base import Formula, TrueFormula, is_literal
from pddl.logic.predicates import DerivedPredicate, Predicate
from pddl.logic.terms import Constant, Term, Variable
from pddl.requirements import Requirements


class Domain:
    """A class for a PDDL domain file."""

    def __init__(
        self,
        name: namelike,
        requirements: Optional[Collection["Requirements"]] = None,
        types: Optional[Dict[namelike, Optional[namelike]]] = None,
        constants: Optional[Collection[Constant]] = None,
        predicates: Optional[Collection[Predicate]] = None,  # TODO cannot be empty
        derived_predicates: Optional[
            Collection[DerivedPredicate]
        ] = None,  # TODO cannot be empty
        actions: Optional[Collection["Action"]] = None,
    ):
        """
        Initialize a PDDL domain.

        :param name: the name of the domain.
        :param requirements: the requirements supported.
        :param types: the hierarchy of supported types.
            types is a dictionary mapping a type name to its ancestor.
        :param constants: the constants.
        :param predicates: the predicates.
        :param derived_predicates: the derived predicates.
        :param actions: the actions.
        """
        self._name = name_type(name)
        self._requirements = ensure_set(requirements)
        self._types = Types(types, self._requirements)
        self._constants = ensure_set(constants)
        self._predicates = ensure_set(predicates)
        self._derived_predicates = ensure_set(derived_predicates)
        self._actions = ensure_set(actions)

        self._check_consistency()

    def _check_consistency(self) -> None:
        """Check consistency of a domain instance object."""
        checker = TypeChecker(self._types, self.requirements)
        checker.check_type(self.constants)
        _check_types_in_has_terms_objects(self._predicates, self._types.all_types)
        _check_types_in_has_terms_objects(self._actions, self._types.all_types)  # type: ignore
        self._check_types_in_derived_predicates()

    def _check_types_in_derived_predicates(self) -> None:
        """Check types in derived predicates."""
        dp_list = (
            [dp.predicate for dp in self._derived_predicates]
            if self._derived_predicates
            else set()
        )
        _check_types_in_has_terms_objects(dp_list, self._types.all_types)

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def requirements(self) -> AbstractSet["Requirements"]:
        """Get the PDDL requirements for this domain."""
        return self._requirements

    @property
    def constants(self) -> AbstractSet[Constant]:
        """Get the constants."""
        return self._constants

    @property
    def predicates(self) -> AbstractSet[Predicate]:
        """Get the predicates."""
        return self._predicates

    @property
    def derived_predicates(self) -> AbstractSet[DerivedPredicate]:
        """Get the derived predicates."""
        return self._derived_predicates

    @property
    def actions(self) -> AbstractSet["Action"]:
        """Get the actions."""
        return self._actions

    @property
    def types(self) -> Dict[name_type, Optional[name_type]]:
        """Get the type definitions, if defined. Else, raise error."""
        return self._types.raw

    def __eq__(self, other):
        """Compare with another object."""
        return (
            isinstance(other, Domain)
            and self.name == other.name
            and self.requirements == other.requirements
            and self.types == other.types
            and self.constants == other.constants
            and self.predicates == other.predicates
            and self.derived_predicates == other.derived_predicates
            and self.actions == other.actions
        )


class Problem:
    """A class for a PDDL problem file."""

    def __init__(
        self,
        name: namelike,
        domain: Optional[Domain] = None,
        domain_name: Optional[str] = None,
        requirements: Optional[Collection["Requirements"]] = None,
        objects: Optional[Collection["Constant"]] = None,
        init: Optional[Collection[Formula]] = None,
        goal: Optional[Formula] = None,
    ):
        """
        Initialize the PDDL problem.

        :param name: the name of the PDDL problem.
        :param domain: the PDDL domain.
        :param domain_name: the domain name. Must match with the domain object.
        :param requirements: the set of PDDL requirements.
        :param objects: the set of objects.
        :param init: the initial condition.
        :param goal: the goal condition.
        """
        self._name = name_type(name)
        self._domain: Optional[Domain] = domain
        self._domain_name = name_type(domain_name) if domain_name else None
        self._requirements: Optional[
            AbstractSet[Requirements]
        ] = self._parse_requirements(domain, requirements)
        self._objects: AbstractSet[Constant] = ensure_set(objects)
        self._init: AbstractSet[Formula] = ensure_set(init)
        self._goal: Formula = ensure(goal, TrueFormula())
        validate(
            all(map(is_literal, self.init)),
            "Not all formulas of initial condition are literals!",
        )

        self._check_consistency()

    def _parse_requirements(
        self,
        domain: Optional[Domain],
        requirements: Optional[Collection["Requirements"]],
    ) -> Optional[AbstractSet[Requirements]]:
        """
        Parse the requirements.

        If the requirements set is given, use it. Otherwise, take the requirements from the domain.
        """
        if requirements is not None or domain is None:
            return set(requirements) if requirements is not None else None
        return domain.requirements

    def _check_consistency(self) -> None:
        """Check consistency of the PDDL Problem instance object."""
        validate(
            xor(self._domain is not None, self._domain_name is not None),
            "Only one between 'domain' and 'domain_name' must be set.",
        )
        if self._domain is not None:
            self.check(self._domain)

    def check(self, domain: Domain) -> None:
        """Check the problem definition against a domain definition."""
        validate(
            self.domain_name == domain.name,
            "Domain names don't match.",
        )
        validate(
            self.requirements is None or self.requirements == domain.requirements,
            "Requirements don't match.",
        )
        types = Types(domain.types, domain.requirements, skip_checks=True)  # type: ignore
        type_checker = TypeChecker(types, domain.requirements)
        type_checker.check_type(self.objects)
        # TODO check init
        # TODO check goal

    @property
    def name(self) -> name_type:
        """Get the name."""
        return self._name

    @property
    def domain(self) -> Domain:
        """Get the domain."""
        assert_(self._domain is not None, "Domain is not set.")
        return cast(Domain, self._domain)

    @domain.setter
    def domain(self, domain: Domain) -> None:
        """Set the domain."""
        self._domain_name = None
        self._domain = domain
        self._check_consistency()

    @property
    def domain_name(self) -> name_type:
        """Get the domain name."""
        if self._domain is not None:
            return self._domain.name

        assert_(self._domain_name is not None, "Domain name is not set.")
        return cast(name_type, self._domain_name)

    @property
    def requirements(self) -> AbstractSet["Requirements"]:
        """Get the requirements."""
        if self._domain is not None:
            return self._domain.requirements

        if self._requirements is None:
            raise AttributeError("neither requirements nor domain are set.")
        return cast(AbstractSet[Requirements], self._requirements)

    @property
    def objects(self) -> AbstractSet["Constant"]:
        """Get the set of objects."""
        return self._objects

    @property
    def init(self) -> AbstractSet[Formula]:
        """Get the initial state."""
        return self._init

    @property
    def goal(self) -> Formula:
        """Get the goal."""
        return self._goal

    def __eq__(self, other):
        """Compare with another object."""
        return (
            isinstance(other, Problem)
            and self.name == other.name
            and self._domain == other._domain
            and self.domain_name == other.domain_name
            and self.requirements == other.requirements
            and self.objects == other.objects
            and self.init == other.init
            and self.goal == other.goal
        )


class Action:
    """A class for the PDDL Action."""

    def __init__(
        self,
        name: namelike,
        parameters: Sequence[Variable],
        precondition: Optional[Formula] = None,
        effect: Optional[Formula] = None,
    ):
        """
        Initialize the action.

        :param name: the action name.
        :param parameters: the action parameters.
        :param precondition: the action precondition.
        :param effect: the action effect.
        """
        self._name: str = name_type(name)
        self._parameters: Sequence[Variable] = ensure_sequence(parameters)
        self._precondition = precondition
        self._effect = effect

    @property
    def name(self) -> str:
        """Get the name."""
        return self._name

    @property
    def parameters(self) -> Sequence[Variable]:
        """Get the parameters."""
        return self._parameters

    @property
    def terms(self) -> Sequence[Term]:
        """Get the terms."""
        return self.parameters

    @property
    def precondition(self) -> Optional[Formula]:
        """Get the precondition."""
        return self._precondition

    @property
    def effect(self) -> Optional[Formula]:
        """Get the effect."""
        return self._effect

    def __str__(self):
        """Get the string."""
        operator_str = "(:action {0}\n".format(self.name)
        operator_str += f"    :parameters ({_typed_parameters(self.parameters)})\n"
        if self.precondition is not None:
            operator_str += f"    :precondition {str(self.precondition)}\n"
        if self.effect is not None:
            operator_str += f"    :effect {str(self.effect)}\n"
        operator_str += ")"
        return operator_str

    def __eq__(self, other):
        """Check equality between two Actions."""
        return (
            isinstance(other, Action)
            and self.name == other.name
            and self.parameters == other.parameters
            and self.precondition == other.precondition
            and self.effect == other.effect
        )

    def __hash__(self):
        """Get the hash."""
        return hash((self.name, self.parameters, self.precondition, self.effect))

    def __repr__(self) -> str:
        """Get an unambiguous string representation."""
        return (
            f"{type(self).__name__}({self.name}, parameters={', '.join(map(str, self.parameters))}, "
            f"precondition={self.precondition}, effect={self.effect})"
        )
