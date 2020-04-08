#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The setup script."""
import os

from setuptools import setup, find_packages

with open('README.md') as readme_file:
    readme = readme_file.read()

with open('HISTORY.md') as history_file:
    history = history_file.read()

here = os.path.abspath(os.path.dirname(__file__))
about = {}
with open(os.path.join(here, 'fondpddl', '__version__.py'), 'r') as f:
    exec(f.read(), about)


install_requires = [
    "lark-parser"
]

setup(
    name=about['__title__'],
    description=about['__description__'],
    version=about['__version__'],
    author=about['__author__'],
    url=about['__url__'],
    author_email=about["__author_email__"],
    long_description=readme + '\n\n' + history,
    long_description_content_type="text/markdown",
    classifiers=[
        'Development Status :: 1 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.7',
    ],
    install_requires=install_requires,
    license=about["__license__"],
    keywords='fondpddl',
    packages=find_packages(include=['fondpddl*']),
    test_suite='tests',
    tests_require=["pytest"],
    zip_safe=False,
)
