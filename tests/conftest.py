#
# Copyright 2021-2025 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""This module contains the configurations for the tests."""
import inspect
import itertools
from pathlib import Path

import mistune
import pytest
from pytest_lazy_fixtures import lf

import pddl
from pddl.parser.domain import DomainParser
from pddl.parser.problem import ProblemParser
from pddl.parser.symbols import Symbols

_current_filepath = inspect.getframeinfo(inspect.currentframe()).filename  # type: ignore
TEST_DIRECTORY = Path(_current_filepath).absolute().parent
ROOT_DIRECTORY = TEST_DIRECTORY.parent
LIBRARY_DIRECTORY = ROOT_DIRECTORY / pddl.__name__
DOCS_DIRECTORY = ROOT_DIRECTORY / "docs"


FIXTURES_DIR = TEST_DIRECTORY / "fixtures"
FIXTURES_PDDL_FILES = FIXTURES_DIR / "pddl_files"
BLOCKSWORLD_FILES = FIXTURES_PDDL_FILES / "blocksworld-ipc08"
TRIANGLE_FILES = FIXTURES_PDDL_FILES / "triangle-tireworld"
BLOCKSWORLD_FOND_FILES = FIXTURES_PDDL_FILES / "blocksworld_fond"

DOMAIN_FILES = [*FIXTURES_PDDL_FILES.glob("./**/domain.pddl")]

DOMAIN_NAMES = [
    "acrobatics",
    "barman-sequential-optimal",
    "beam-walk",
    "blocksworld-ipc08",
    "blocksworld_fond",
    "cave-diving-sequential-optimal",
    # "depots-numeric-automatic",
    "doors",
    "earth_observation",
    "elevators",
    # "faults-ipc08",
    "first-responders-ipc08",
    "islands",
    "maintenance-sequential-satisficing-ipc2014",
    "miner",
    "rovers-numeric-automatic",
    "rovers_fond",
    "sokoban-sequential-optimal",
    "spiky-tireworld",
    "storage",
    "tireworld",
    "tireworld-truck",
    "triangle-tireworld",
    "zenotravel",
    "hello-world-functions",
]

PROBLEM_FILES = list(
    itertools.chain(
        *[
            (FIXTURES_PDDL_FILES / domain_name).rglob("p*.pddl")
            for domain_name in DOMAIN_NAMES
        ]
    )
)


@pytest.fixture(scope="session")
def domain_parser():
    """Get the PDDL domain parser."""
    return DomainParser()


@pytest.fixture(scope="session")
def problem_parser():
    """Get the PDDL problem parser."""
    return ProblemParser()


@pytest.fixture(scope="session")
def markdown_parser():
    """Get the Mistune Markdown parser."""
    return mistune.create_markdown(renderer=mistune.AstRenderer())


#################################################
# Import PDDL fixtures
from tests.fixtures.code_objects.blocksworld_fond import (  # noqa: E402, F401
    blocksworld_fond_01,
    blocksworld_fond_domain,
)
from tests.fixtures.code_objects.blocksworld_ipc08 import (  # noqa: E402, F401
    blocksworld_domain,
    blocksworld_problem_01,
)
from tests.fixtures.code_objects.triangle_tireworld import (  # noqa: E402, F401
    triangle_tireworld_domain,
    triangle_tireworld_problem_01,
)

pddl_objects_domains = [
    lf("blocksworld_domain"),  # type:ignore
    lf("triangle_tireworld_domain"),  # type:ignore
    lf("blocksworld_fond_domain"),  # type:ignore
]
pddl_objects_problems = [
    lf("blocksworld_fond_01"),  # type:ignore
    lf("blocksworld_problem_01"),  # type:ignore
    lf("triangle_tireworld_problem_01"),  # type:ignore
]

#################################################


# A set of symbols that can be matched as names but they are keywords
# this is a subset of all symbols
TEXT_SYMBOLS = {
    Symbols.AND.value,
    Symbols.DEFINE.value,
    Symbols.DOMAIN.value,
    Symbols.EITHER.value,
    Symbols.EXISTS.value,
    Symbols.FORALL.value,
    Symbols.NOT.value,
    Symbols.OBJECT.value,
    Symbols.ONEOF.value,
    Symbols.OR.value,
    Symbols.PROBLEM.value,
    Symbols.WHEN.value,
}
