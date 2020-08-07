# -*- coding: utf-8 -*-

"""This module contains the tests for the domain parser."""
from pddl.core import Domain, Problem
from tests.conftest import FIXTURES_DIR


def test_blocksworld_ipc08_domain_parser(domain_parser, blocksworld_ipc08_domain):
    """Test simple domain parsing."""
    filepath = FIXTURES_DIR / "pddl_files" / "blocksworld-ipc08" / "domain.pddl"
    domain = domain_parser(filepath.read_text())

    assert isinstance(domain, Domain)
    assert domain == blocksworld_ipc08_domain


def test_blocksworld_ipc08_problem_parser(problem_parser, blocksworld_ipc08_problem_01):
    """Test simple problem parsing."""
    filepath = FIXTURES_DIR / "pddl_files" / "blocksworld-ipc08" / "p01.pddl"
    problem = problem_parser(filepath.read_text())

    assert isinstance(problem, Problem)
    assert problem == blocksworld_ipc08_problem_01
