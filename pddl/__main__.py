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

"""Main entrypoint for the PDDL parser CLI tool."""

import os
import sys

import click

from pddl import parse_domain, parse_plan, parse_problem
from pddl.formatter import domain_to_string, plan_to_string, problem_to_string


@click.command()
@click.argument("domain_file", type=click.Path(exists=True, dir_okay=False))
@click.argument(
    "problem_file",
    type=click.Path(exists=True, dir_okay=False),
    required=False,
)
@click.argument(
    "plan_file",
    type=click.Path(exists=True, dir_okay=False),
    required=False,
)
@click.option("-q", "--quiet", is_flag=True, help="Don't print anything.")
def cli(domain_file, problem_file, plan_file, quiet):
    """The unquestionable parser for PDDL 3.1."""  # noqa
    if quiet:
        sys.stdout = open(os.devnull, "a")
    domain = parse_domain(domain_file)
    print(domain_to_string(domain))
    if problem_file is not None:
        problem = parse_problem(problem_file)
        problem.check(domain)
        print(problem_to_string(problem))
    if plan_file is not None:
        plan = parse_plan(plan_file)
        plan.instantiate(domain)
        print(plan_to_string(plan))


if __name__ == "__main__":
    cli()
