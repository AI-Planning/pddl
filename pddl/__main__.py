# -*- coding: utf-8 -*-
"""Main entrypoint for the PDDL parser CLI tool."""
from pathlib import Path

import click

from pddl.parser.domain import DomainParser


@click.command()
@click.argument("domain", type=click.Path(exists=True, dir_okay=False))
def main(domain):
    """The unquestionable parser for PDDL 3.1."""  # noqa: D401
    print(DomainParser()(Path(domain).read_text()))


if __name__ == "__main__":
    main()
