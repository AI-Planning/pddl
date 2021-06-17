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

"""Test introduction documentation page."""
from pddl.formatter import domain_to_string, problem_to_string
from tests.conftest import ROOT_DIRECTORY
from tests.test_docs.base import BaseTestMarkdownDocs, compile_and_exec


class TestIntroduction(BaseTestMarkdownDocs):
    """Test that the code snippet in the introduction are correct."""

    MD_FILE = ROOT_DIRECTORY / "README.md"

    @classmethod
    def setup_class(cls):
        """Set up the test."""
        super().setup_class()
        cls.python_code_blocks = cls.extract_code_blocks("python")
        cls.output_code_blocks = cls.extract_code_blocks("output")
        cls.locals = {}

    def test_outputs(self):
        """Test Python snipped and its output."""
        domain_python_code = self.python_code_blocks[0]
        expected_domain_output = self.output_code_blocks[0]
        locals_dict = compile_and_exec(domain_python_code, locals_dict=self.locals)
        domain = locals_dict["domain"]
        actual_domain_output = domain_to_string(domain)
        assert expected_domain_output == actual_domain_output

        problem_python_code = self.python_code_blocks[1]
        expected_problem_output = self.output_code_blocks[1]
        locals_dict = compile_and_exec(problem_python_code, locals_dict=self.locals)
        problem = locals_dict["problem"]
        actual_problem_output = problem_to_string(problem)
        assert expected_problem_output == actual_problem_output
