# -*- coding: utf-8 -*-

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
        "(:actions \n",
        domain.actions,
        ")\n",
        to_string=lambda obj: indent(str(obj), indentation) + "\n",
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
