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

"""
This script checks that all the Python files of the repository have:
- (optional) the Python shebang
- the encoding header;
- the copyright notice;

It is assumed the script is run from the repository root.
"""

import itertools
import re
import sys
from pathlib import Path

HEADER_REGEX = r"""(# -\*- coding: utf-8 -\*-
#
# Copyright 2021 WhiteMech
#
# ------------------------------
#
# This file is part of pddl\.
#
# pddl is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# \(at your option\) any later version\.
#
# pddl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE\.  See the
# GNU Lesser General Public License for more details\.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pddl\.  If not, see <https://www\.gnu\.org/licenses/>\.
#)
"""


IGNORE_FILES = {
    Path("scripts", "whitelist.py")
}


def check_copyright(file: Path) -> bool:
    """
    Given a file, check if the header stuff is in place.

    Return True if the files has the encoding header and the copyright notice,
    optionally prefixed by the shebang. Return False otherwise.

    :param file: the file to check.
    :return True if the file is compliant with the checks, False otherwise.
    """
    content = file.read_text()
    header_regex = re.compile(HEADER_REGEX, re.MULTILINE)
    return re.match(header_regex, content) is not None


def parse_args():
    """Parse arguments."""
    import argparse  # pylint: disable=import-outside-toplevel

    parser = argparse.ArgumentParser("check_copyright_notice")
    parser.add_argument(
        "--directory", type=str, default=".", help="The path to the repository root."
    )


if __name__ == "__main__":
    python_files = itertools.chain(
        Path("pddl").glob("**/*.py"),
        Path("tests").glob("**/*.py"),
        Path("scripts").glob("**/*.py"),
        [Path("setup.py")],
    )

    python_files = filter(lambda p: p not in IGNORE_FILES, python_files)

    bad_files = [
        filepath for filepath in python_files if not check_copyright(filepath)
    ]

    if len(bad_files) > 0:
        print("The following files are not well formatted:")
        print("\n".join(map(str, bad_files)))
        sys.exit(1)
    else:
        print("OK")
        sys.exit(0)
