# -*- coding: utf-8 -*-
"""Main entrypoint for the PDDL parser CLI tool."""
import os
import sys
from pathlib import Path

import click

from pddl.formatter import domain_to_string, problem_to_string
from pddl.parser.domain import DomainParser
from pddl.parser.problem import ProblemParser


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
    print(domain_to_string(DomainParser()(Path(domain_file).read_text())))


@cli.command()
@click.argument("problem_file", type=click.Path(exists=True, dir_okay=False))
@quiet_option
def problem(problem_file, quiet):
    """Check a PDDL problem file is correct."""
    if quiet:
        sys.stdout = open(os.devnull, "a")
    print(problem_to_string(ProblemParser()(Path(problem_file).read_text())))


if __name__ == "__main__":
    cli()
