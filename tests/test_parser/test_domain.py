# -*- coding: utf-8 -*-

"""This module contains the tests for the domain parser."""
from tests.conftest import FIXTURES_DIR


def test_blocksworld_ipc08_domain_parser(domain_parser):
    """Test simple domain parsing."""
    filepath = FIXTURES_DIR / "blocksworld-ipc08" / "domain.pddl"
    domain_parser(filepath.read_text())
