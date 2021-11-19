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

"""Formatting utilities for PDDL domains and problems."""
from textwrap import indent
from typing import Callable, Collection

from pddl.core import Domain, Problem
from pddl.logic.base import TRUE


def _remove_empty_lines(s: str) -> str:
    """Remove empty lines from string."""
    return "\n".join(filter(str.strip, s.splitlines()))


def _sort_and_print_collection(
    prefix, collection: Collection, postfix, to_string: Callable = str
):
    if len(collection) > 0:
        return prefix + " ".join(sorted(map(to_string, collection))) + postfix
    else:
        return ""


def domain_to_string(domain: Domain) -> str:
    """Print a PDDL domain object."""
    result = f"(define (domain {domain.name})"
    body = ""
    indentation = " " * 4
    body += _sort_and_print_collection("(:requirements ", domain.requirements, ")\n")
    body += _sort_and_print_collection("(:types ", domain.types, ")\n")
    body += _sort_and_print_collection("(:constants ", domain.constants, ")\n")
    body += _sort_and_print_collection("(:predicates ", domain.predicates, ")\n")
    body += _sort_and_print_collection(
        "",
        domain.derived_predicates,
        "",
        to_string=lambda obj: str(obj) + "\n",
    )
    body += _sort_and_print_collection(
        "",
        domain.actions,
        "",
        to_string=lambda obj: str(obj) + "\n",
    )
    result = result + "\n" + indent(body, indentation) + "\n)"
    result = _remove_empty_lines(result)
    return result


def problem_to_string(problem: Problem) -> str:
    """Print a PDDL problem object."""
    result = f"(define (problem {problem.name})"
    body = f"(:domain {problem.domain_name})\n"
    indentation = " " * 4
    body += _sort_and_print_collection("(:requirements ", problem.requirements, ")\n")
    body += _sort_and_print_collection("(:objects ", problem.objects, ")\n")
    body += _sort_and_print_collection("(:init ", problem.init, ")\n")
    body += f"{'(:goal ' + str(problem.goal) + ')'}\n" if problem.goal != TRUE else ""
    result = result + "\n" + indent(body, indentation) + "\n)"
    result = _remove_empty_lines(result)
    return result
