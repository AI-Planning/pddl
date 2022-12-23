# -*- coding: utf-8 -*-
#
# Copyright 2021-2022 WhiteMech
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

from pddl import parse_domain, parse_problem
from pddl.formatter import domain_to_string, problem_to_string


@click.group()
def cli():
    """The unquestionable parser for PDDL 3.1."""  # noqa


quiet_option = click.option("-q", "--quiet", is_flag=True, help="Don't print anything.")


@cli.command()
@click.argument("domain_file", type=click.Path(exists=True, dir_okay=False))
@quiet_option
def domain(domain_file, quiet):
    """Check a PDDL domain file is correct."""
    if quiet:
        sys.stdout = open(os.devnull, "a")
    print(domain_to_string(parse_domain(domain_file)))


@cli.command()
@click.argument("problem_file", type=click.Path(exists=True, dir_okay=False))
@quiet_option
def problem(problem_file, quiet):
    """Check a PDDL problem file is correct."""
    if quiet:
        sys.stdout = open(os.devnull, "a")
    print(problem_to_string(parse_problem(problem_file)))


if __name__ == "__main__":
    cli()
