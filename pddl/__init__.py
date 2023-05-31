#
# Copyright 2021-2023 WhiteMech
#
# ------------------------------
#
# This file is part of pddl.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.
#

"""Top-level package for pddl."""

from .__version__ import (
    __author__,
    __author_email__,
    __copyright__,
    __description__,
    __license__,
    __title__,
    __url__,
    __version__,
)
from .helpers.base import _get_current_path

_ROOT_PATH = _get_current_path()


# Simple helpers
def parse_domain(fn):
    from pddl.parser.domain import DomainParser

    with open(fn, "r") as f:
        dtext = f.read()
    return DomainParser()(dtext)


def parse_problem(fn):
    from pddl.parser.problem import ProblemParser

    with open(fn, "r") as f:
        ptext = f.read()
    return ProblemParser()(ptext)
