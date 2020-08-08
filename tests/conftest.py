# -*- coding: utf-8 -*-
"""This module contains the configurations for the tests."""
import inspect
import os
from pathlib import Path

import pytest

from pddl.parser.domain import DomainParser
from pddl.parser.problem import ProblemParser

CUR_PATH = Path(os.path.dirname(inspect.getfile(inspect.currentframe())))  # type: ignore
ROOT_DIR = Path(CUR_PATH, "..").resolve()  # type: ignore

FIXTURES_DIR = CUR_PATH / "fixtures"
FIXTURES_PDDL_FILES = FIXTURES_DIR / "pddl_files"
BLOCKSWORLD_FILES = FIXTURES_PDDL_FILES / "blocksworld-ipc08"
TRIANGLE_FILES = FIXTURES_PDDL_FILES / "triangle-tireworld"

# TODO use globbing when we will support all the domain files.
DOMAIN_FILES = [
    BLOCKSWORLD_FILES
    / "domain.pddl"
    # TRIANGLE_FILES / "domain.pddl",  TODO: effect of action with nested and-oneof-and
]

PROBLEM_FILES = [*BLOCKSWORLD_FILES.glob("./p*.pddl")]


@pytest.fixture(scope="session")
def domain_parser():
    """Get the PDDL domain parser."""
    return DomainParser()


@pytest.fixture(scope="session")
def problem_parser():
    """Get the PDDL problem parser."""
    return ProblemParser()


#################################################
# Import PDDL fixtures
from tests.fixtures.code_objects.blocksworld_ipc08 import (  # noqa: E402, F401
    blocksworld_domain,
    blocksworld_problem_01,
)

#################################################
