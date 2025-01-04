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

"""The setup script."""

import os
import glob

from setuptools import setup, find_packages

with open('README.md') as readme_file:
    readme = readme_file.read()

here = os.path.abspath(os.path.dirname(__file__))
about = {}
with open(os.path.join(here, 'pddl', '__version__.py'), 'r') as f:
    exec(f.read(), about)


install_requires = [
    "lark>=1.1.5,<1.2.0",
    "click>=8.1.3,<9.0.0"
]

setup(
    name=about['__title__'],
    description=about['__description__'],
    version=about['__version__'],
    author=about['__author__'],
    url=about['__url__'],
    author_email=about["__author_email__"],
    long_description=readme,
    long_description_content_type="text/markdown",
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Education',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Programming Language :: Python :: 3.12',
    ],
    python_requires='>=3.9',
    install_requires=install_requires,
    license=about["__license__"],
    include_package_data=True,
    data_files=[
        ("pddl/parser", glob.glob("pddl/parser/*.lark")),
    ],
    keywords='pddl',
    packages=find_packages(include=['pddl*']),
    entry_points={
        'console_scripts': ["pddl=pddl.__main__:cli"],
    },
    test_suite='tests',
    tests_require=["pytest"],
    zip_safe=False,
)
