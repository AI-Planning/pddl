# -*- coding: utf-8 -*-

"""This module contains tests for the CLI tool."""
import pytest
from click.testing import CliRunner

from pddl.__main__ import cli
from tests.conftest import DOMAIN_FILES, PROBLEM_FILES


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_pddl_domain_cli(pddl_file):
    """Test PDDL domain cli."""
    result = CliRunner().invoke(cli, args=["domain", str(pddl_file)])
    assert result.exit_code == 0


@pytest.mark.parametrize("pddl_file", PROBLEM_FILES)
def test_pddl_problem_cli(pddl_file):
    """Test PDDL domain cli."""
    result = CliRunner().invoke(cli, args=["problem", str(pddl_file)])
    assert result.exit_code == 0
