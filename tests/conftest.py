# -*- coding: utf-8 -*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl.  If not, see <https://www.gnu.org/licenses/>.
#

"""This module contains the configurations for the tests."""
import inspect
from pathlib import Path

import mistune
import pytest

import pddl
from pddl.parser.domain import DomainParser
from pddl.parser.problem import ProblemParser

_current_filepath = inspect.getframeinfo(inspect.currentframe()).filename  # type: ignore
TEST_DIRECTORY = Path(_current_filepath).absolute().parent
ROOT_DIRECTORY = TEST_DIRECTORY.parent
LIBRARY_DIRECTORY = ROOT_DIRECTORY / pddl.__name__
DOCS_DIRECTORY = ROOT_DIRECTORY / "docs"


FIXTURES_DIR = TEST_DIRECTORY / "fixtures"
FIXTURES_PDDL_FILES = FIXTURES_DIR / "pddl_files"
BLOCKSWORLD_FILES = FIXTURES_PDDL_FILES / "blocksworld-ipc08"
TRIANGLE_FILES = FIXTURES_PDDL_FILES / "triangle-tireworld"

# TODO once missing features are supported, uncomment this
# DOMAIN_FILES = [
#     *FIXTURES_PDDL_FILES.glob("./**/domain.pddl")
# ]

DOMAIN_NAMES = [
    "acrobatics",
    "beam-walk",
    "blocksworld-ipc08",
    "doors",
    # "earth_observation",
    "elevators",
    # "faults-ipc08",
    # "first-responders-ipc08",
    "islands",
    "miner",
    "spiky-tireworld",
    "tireworld",
    "tireworld-truck",
    "triangle-tireworld",
    # "zenotravel",
]

DOMAIN_FILES = [
    FIXTURES_PDDL_FILES / domain_name / "domain.pddl" for domain_name in DOMAIN_NAMES
]

PROBLEM_FILES = [*FIXTURES_PDDL_FILES.glob("./**/p*.pddl")]


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
from tests.fixtures.code_objects.blocksworld_ipc08 import (  # noqa: E402, F401
    blocksworld_domain,
    blocksworld_problem_01,
)
from tests.fixtures.code_objects.triangle_tireworld import (  # noqa: E402, F401
    triangle_tireworld_domain,
    triangle_tireworld_problem_01,
)

#################################################
