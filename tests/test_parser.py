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

"""This module contains the tests for the domain parser."""
from pathlib import Path
from textwrap import dedent

import lark
import pytest
from pytest import lazy_fixture  # type:ignore  # noqa

from pddl.core import Domain, Problem
from pddl.parser.domain import DomainParser
from pddl.parser.symbols import Symbols
from tests.conftest import (
    BLOCKSWORLD_FILES,
    BLOCKSWORLD_FOND_FILES,
    DOMAIN_FILES,
    PROBLEM_FILES,
    TEXT_SYMBOLS,
    TRIANGLE_FILES,
)


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_domain_parser(domain_parser, pddl_file: Path):
    """Test only that the domain parsing works for all the fixtures."""
    domain_parser(pddl_file.read_text())


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_problem_parser(problem_parser, pddl_file: Path):
    """Test only that the problem parsing works for all the fixtures."""
    problem_parser(pddl_file.read_text())


@pytest.mark.parametrize(
    "pddl_file,expected_domain",
    [
        (
            BLOCKSWORLD_FILES / "domain.pddl",
            lazy_fixture("blocksworld_domain"),  # type:ignore
        ),
        (
            TRIANGLE_FILES / "domain.pddl",
            lazy_fixture("triangle_tireworld_domain"),  # type:ignore
        ),
        (
            BLOCKSWORLD_FOND_FILES / "domain.pddl",
            lazy_fixture("blocksworld_fond_domain"),  # type:ignore
        ),
    ],
)
def test_check_domain_parser_output(domain_parser, pddl_file: Path, expected_domain):
    """Test domain parsing."""
    actual_domain = domain_parser(pddl_file.read_text())

    assert isinstance(actual_domain, Domain)
    assert actual_domain == expected_domain


@pytest.mark.parametrize(
    "pddl_file,expected_problem",
    [
        (
            BLOCKSWORLD_FILES / "p01.pddl",
            lazy_fixture("blocksworld_problem_01"),  # type:ignore
        ),
        (
            BLOCKSWORLD_FOND_FILES / "p01.pddl",
            lazy_fixture("blocksworld_fond_01"),  # type:ignore
        ),
    ],
)
def test_check_problem_parser_output(problem_parser, pddl_file: Path, expected_problem):
    """Test problem parsing."""
    actual_problem = problem_parser(pddl_file.read_text())

    assert isinstance(actual_problem, Problem)
    assert actual_problem == expected_problem


def test_hierarchical_types() -> None:
    """Test correct parsing of hierarchical types (see https://github.com/AI-Planning/pddl/issues/70)."""
    domain_str = dedent(
        """
    (define (domain logistics)
        (:requirements :strips :typing)
        (:types truck airplane - vehicle
            package vehicle - physobj
            airport location - place
            city place physobj - object)
        (:predicates (in-city ?loc - place ?city - city)
            (at ?obj - physobj ?loc - place)
            (in ?pkg - package ?veh - vehicle))
        (:action LOAD-TRUCK
            :parameters   (?pkg - package ?truck - truck ?loc - place)
            :precondition (and (at ?truck ?loc) (at ?pkg ?loc))
            :effect       (and (not (at ?pkg ?loc)) (in ?pkg ?truck)))
    )
    """
    )
    domain = DomainParser()(domain_str)

    assert domain.types == {
        "truck": "vehicle",
        "airplane": "vehicle",
        "package": "physobj",
        "vehicle": "physobj",
        "airport": "place",
        "location": "place",
        "city": "object",
        "place": "object",
        "physobj": "object",
    }


def test_types_repetition_in_simple_typed_lists_not_allowed() -> None:
    """Check types repetition in simple typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types a b c a)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['a', 'b', 'c', 'a'\\]: "
        "duplicate name 'a' in typed list already present",
    ):
        DomainParser()(domain_str)


def test_types_repetition_in_typed_lists_not_allowed() -> None:
    """Check types repetition in typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types a - t1 b c - t2 a - t3)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['a', '-', 't1', 'b', 'c', '-', 't2', 'a', '-', 't3'\\]: "
        "duplicate name 'a' in typed list already present with types \\['t1'\\]",
    ):
        DomainParser()(domain_str)


def test_constants_repetition_in_simple_typed_lists_not_allowed() -> None:
    """Check constants repetition in simple typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1)
        (:constants c1 c2 c3 c1)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['c1', 'c2', 'c3', 'c1'\\]: "
        "duplicate name 'c1' in typed list already present",
    ):
        DomainParser()(domain_str)


def test_constants_repetition_in_typed_lists_not_allowed() -> None:
    """Check constants repetition in typed lists is detected and a parsing error is raised."""
    domain_str = dedent(
        """
    (define (domain test)
        (:requirements :typing)
        (:types t1 t2)
        (:constants c1 - t1 c1 - t2)
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=".*error while parsing tokens \\['c1', '-', 't1', 'c1', '-', 't2'\\]: "
        "duplicate name 'c1' in typed list already present with types \\['t1'\\]",
    ):
        DomainParser()(domain_str)


@pytest.mark.parametrize("keyword", TEXT_SYMBOLS - {Symbols.OBJECT.value})
def test_keyword_usage_not_allowed_as_name(keyword) -> None:
    """Check keywords usage as names is detected and a parsing error is raised."""
    domain_str = dedent(
        f"""
    (define (domain test)
        (:requirements :typing)
        (:types {keyword})
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=f".*error while parsing tokens \\['{keyword}'\\]: "
        f"invalid name '{keyword}': it is a keyword",
    ):
        DomainParser()(domain_str)


@pytest.mark.parametrize("keyword", TEXT_SYMBOLS - {Symbols.OBJECT.value})
def test_keyword_usage_not_allowed_as_type(keyword) -> None:
    """Check keywords usage as types is detected and a parsing error is raised."""
    domain_str = dedent(
        f"""
    (define (domain test)
        (:requirements :typing)
        (:types t1 - {keyword})
    )
    """
    )

    with pytest.raises(
        lark.exceptions.VisitError,
        match=f".*error while parsing tokens \\['t1', '-', '{keyword}'\\]: "
        f"invalid type '{keyword}': it is a keyword",
    ):
        DomainParser()(domain_str)
