# -*- coding: utf-8 -*-
"""This module contains the configurations for the tests."""
import inspect
import os
from pathlib import Path

import pytest

from pddl.parser.domain import DomainParser

CUR_PATH = Path(os.path.dirname(inspect.getfile(inspect.currentframe())))  # type: ignore
ROOT_DIR = Path(CUR_PATH, "..").resolve()  # type: ignore

FIXTURES_DIR = CUR_PATH / "fixtures"


@pytest.fixture(scope="session")
def domain_parser():
    """Get the PDDL domain parser."""
    return DomainParser()


# Import PDDL fixtures
from tests.fixtures.code_objects.blocksworld_ipc08 import (  # noqa: E402, F401
    blocksworld_ipc08_domain,
)
