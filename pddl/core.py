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
import functools
from enum import Enum
from typing import AbstractSet, Collection, Dict, Optional, Sequence, Set, cast

from pddl._validation import _check_constant_types, _check_types_in_has_terms_objects
from pddl.custom_types import name as name_type
from pddl.custom_types import namelike, to_names, to_names_types  # noqa: F401
from pddl.exceptions import PDDLValidationError
from pddl.helpers.base import (
    _typed_parameters,
    assert_,
    ensure,
    ensure_sequence,
    ensure_set,
    find_cycle,
)
from pddl.logic.base import Formula, TrueFormula, is_literal
from pddl.logic.predicates import DerivedPredicate, Predicate
from pddl.logic.terms import Constant, Term, Variable
from pddl.parser.symbols import RequirementSymbols as RS
from pddl.parser.symbols import Symbols


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
        self._types = _Types(types, self._requirements)
        self._constants = ensure_set(constants)
        self._predicates = ensure_set(predicates)
        self._derived_predicates = ensure_set(derived_predicates)
        self._actions = ensure_set(actions)

        self._check_consistency()

    def _check_consistency(self) -> None:
        """Check consistency of a domain instance object."""
        _check_constant_types(self._constants, self._types.all_types)
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
    def name(self) -> str:
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
        self._requirements: AbstractSet[Requirements] = ensure_set(requirements)
        self._objects: AbstractSet[Constant] = ensure_set(objects)
        self._init: AbstractSet[Formula] = ensure_set(init)
        self._goal: Formula = ensure(goal, TrueFormula())
        assert_(
            all(map(is_literal, self.init)),
            "Not all formulas of initial condition are literals!",
        )

        self._check_consistency()

    def _check_consistency(self):
        assert_(
            self._domain is not None or self._domain_name is not None,
            "At least one between 'domain' and 'domain_name' must be set.",
        )
        assert_(
            self._domain is None
            or self._domain_name is None
            or self._domain.name == self._domain_name
        )

    @property
    def name(self) -> str:
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
        if self._domain_name is not None:
            assert_(
                self._domain_name == domain.name,
                f"Domain names don't match. Expected {self._domain_name}, got {domain.name}.",
            )
        self._domain = domain

    @property
    def domain_name(self) -> str:
        """Get the domain name."""
        if self._domain is not None:
            return self._domain.name

        assert_(self._domain_name is not None, "Domain name is not set.")
        return cast(str, self._domain_name)

    @property
    def requirements(self) -> AbstractSet["Requirements"]:
        """Get the requirements."""
        return self._requirements

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


@functools.total_ordering
class Requirements(Enum):
    """Enum class for the requirements."""

    STRIPS = RS.STRIPS.strip()
    TYPING = RS.TYPING.strip()
    NEG_PRECONDITION = RS.NEG_PRECONDITION.strip()
    DIS_PRECONDITION = RS.DIS_PRECONDITION.strip()
    UNIVERSAL_PRECONDITION = RS.UNIVERSAL_PRECONDITION.strip()
    EXISTENTIAL_PRECONDITION = RS.EXISTENTIAL_PRECONDITION.strip()
    QUANTIFIED_PRECONDITION = RS.QUANTIFIED_PRECONDITION.strip()
    EQUALITY = RS.EQUALITY.strip()
    CONDITIONAL_EFFECTS = RS.CONDITIONAL_EFFECTS.strip()
    ADL = RS.ADL.strip()
    DERIVED_PREDICATES = RS.DERIVED_PREDICATES.strip()
    NON_DETERMINISTIC = RS.NON_DETERMINISTIC.strip()

    @classmethod
    def strips_requirements(cls) -> Set["Requirements"]:
        """Get the STRIPS requirements."""
        return {
            Requirements.TYPING,
            Requirements.NEG_PRECONDITION,
            Requirements.DIS_PRECONDITION,
            Requirements.EQUALITY,
            Requirements.CONDITIONAL_EFFECTS,
        }

    def __str__(self) -> str:
        """Get the string representation."""
        return f":{self.value}"

    def __repr__(self) -> str:
        """Get an unambiguous representation."""
        return f"Requirements{self.name}"

    def __lt__(self, other):
        """Compare with another object."""
        if isinstance(other, Requirements):
            return self.value <= other.value
        else:
            return super().__lt__(other)


class _Types:
    """A class for representing and managing the types available in a PDDL Domain."""

    def __init__(
        self,
        types: Optional[Dict[namelike, Optional[namelike]]] = None,
        requirements: Optional[AbstractSet[Requirements]] = None,
    ) -> None:
        """Initialize the Types object."""
        self._types = to_names_types(ensure(types, dict()))

        self._all_types = self._get_all_types()
        self._check_types_dictionary(self._types, ensure_set(requirements))

    @property
    def raw(self) -> Dict[name_type, Optional[name_type]]:
        """Get the raw types dictionary."""
        return self._types

    @property
    def all_types(self) -> Set[name_type]:
        """Get all available types."""
        return self._all_types

    def _get_all_types(self) -> Set[name_type]:
        """Get all types supported by the domain."""
        if self._types is None:
            return set()
        result = set(self._types.keys()) | set(self._types.values())
        result.discard(None)
        return cast(Set[name_type], result)

    @classmethod
    def _check_types_dictionary(
        cls,
        type_dict: Dict[name_type, Optional[name_type]],
        requirements: AbstractSet[Requirements],
    ) -> None:
        """
        Check the consistency of the types dictionary.

        1) Empty types dictionary is correct by definition:
        >>> _Types._check_types_dictionary({}, set())

        2) There are supertypes, but :typing requirement not specified
        >>> a, b, c = to_names(["a", "b", "c"])
        >>> _Types._check_types_dictionary({a: b, b: c}, set())
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: typing requirement is not specified, but types are used: 'b', 'c'

        3) The `object` type cannot be a subtype:
        >>> a = name_type("a")
        >>> _Types._check_types_dictionary({name_type("object"): a}, {Requirements.TYPING})
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: object must not have supertypes, but got 'object' is a subtype of 'a'

        4) If cycles in the type hierarchy graph are present, an error is raised:
        >>> a, b, c = to_names(["a", "b", "c"])
        >>> _Types._check_types_dictionary({a: b, b: c, c: a}, {Requirements.TYPING})
        Traceback (most recent call last):
        ...
        pddl.exceptions.PDDLValidationError: cycle detected in the type hierarchy: a -> b -> c

        :param type_dict: the types dictionary
        """
        if len(type_dict) == 0:
            return

        # check typing requirement
        supertypes = {t for t in type_dict.values() if t is not None}
        if len(supertypes) > 0 and Requirements.TYPING not in requirements:
            raise PDDLValidationError(
                "typing requirement is not specified, but types are used: '"
                + "', '".join(map(str, sorted(supertypes)))
                + "'"
            )

        # check `object` type
        object_name = name_type(Symbols.OBJECT.value)
        if object_name in type_dict and type_dict[object_name] is not None:
            object_supertype = type_dict[object_name]
            raise PDDLValidationError(
                f"object must not have supertypes, but got 'object' is a subtype of '{object_supertype}'"
            )

        # check cycles
        cycle = find_cycle(type_dict)  # type: ignore
        if cycle is not None:
            raise PDDLValidationError(
                "cycle detected in the type hierarchy: " + " -> ".join(cycle)
            )
