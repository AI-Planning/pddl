#!/usr/bin/env python3
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

HEADER_REGEX = r"""(#!/usr/bin/env python3
)?#
# Copyright 2021-2023 WhiteMech
#
# ------------------------------
#
# This file is part of pddl\.
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource\.org/licenses/MIT\.
#
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
