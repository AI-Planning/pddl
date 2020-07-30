# -*- coding: utf-8 -*-
"""This module contains tests for a PDDL problem."""
from pddl.core import Domain, Problem


class TestProblemEmpty:
    """Test the empty problem."""

    def setup(self):
        """Set up the tests."""
        self.domain = Domain("empty_domain")
        self.problem = Problem("empty_problem", self.domain)

    def test_name(self):
        """Test the name getter."""
        assert self.problem.name == "empty_problem"

    def test_requirements(self):
        """Test the requirements getter."""
        assert self.problem.requirements == set()

    def test_objects(self):
        """Test the objects getter."""
        assert self.problem.objects == set()

    def test_init(self):
        """Test the init getter."""
        assert self.problem.init == set()

    def test_goal(self):
        """Test the goal getter."""
        assert self.problem.goal == set()
