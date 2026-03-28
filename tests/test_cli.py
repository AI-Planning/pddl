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

"""This module contains tests for the CLI tool."""

import logging
from pathlib import Path

import pytest
from click.testing import CliRunner

from pddl.__main__ import cli
from tests.conftest import DOMAIN_FILES, PLAN_FILES, PROBLEM_FILES


@pytest.mark.parametrize("pddl_file", DOMAIN_FILES)
def test_pddl_domain_cli(pddl_file):
    """Test PDDL domain-only CLI invocation."""
    logging.debug(f"Parsing domain file: {pddl_file}")
    result = CliRunner().invoke(cli, args=[str(pddl_file)], catch_exceptions=False)
    assert result.exit_code == 0


def _find_domain_for_problem(problem_file: Path) -> Path:
    """Find the nearest domain.pddl for a given problem file."""
    for parent in [problem_file.parent, *problem_file.parents]:
        domain_file = parent / "domain.pddl"
        if domain_file.exists():
            return domain_file
    raise RuntimeError(f"Could not find domain.pddl for problem file {problem_file}")


def _find_problem_for_plan(plan_file: Path) -> Path:
    """Find the matching problem file for a given plan file."""
    problem_file = plan_file.with_suffix(".pddl")
    if problem_file.exists():
        return problem_file
    raise RuntimeError(f"Could not find matching problem file for plan {plan_file}")


@pytest.mark.parametrize("problem_file", PROBLEM_FILES)
def test_pddl_problem_cli(problem_file):
    """Test PDDL domain+problem CLI invocation."""
    domain_file = _find_domain_for_problem(problem_file)
    logging.debug(f"Parsing problem file: {problem_file} with domain {domain_file}")
    result = CliRunner().invoke(
        cli, args=[str(domain_file), str(problem_file)], catch_exceptions=False
    )
    assert result.exit_code == 0


@pytest.mark.parametrize("plan_file", PLAN_FILES)
def test_pddl_plan_cli(plan_file):
    """Test PDDL domain+problem+plan CLI invocation."""
    problem_file = _find_problem_for_plan(plan_file)
    domain_file = _find_domain_for_problem(problem_file)
    logging.debug(
        f"Parsing plan file: {plan_file} with problem {problem_file} and domain {domain_file}"
    )
    result = CliRunner().invoke(
        cli,
        args=[str(domain_file), str(problem_file), str(plan_file)],
        catch_exceptions=False,
    )
    assert result.exit_code == 0
