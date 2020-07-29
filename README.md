# pddl

![pddl CI](https://github.com/whitemech/pddl/workflows/pddl%20CI/badge.svg)
[![](https://img.shields.io/badge/docs-mkdocs-9cf)](https://www.mkdocs.org/)
[![](https://img.shields.io/badge/status-development-orange.svg)](https://img.shields.io/badge/status-development-orange.svg)
[![codecov](https://codecov.io/gh/whitemech/pddl/branch/master/graph/badge.svg?token=FG3ATGP5P5)](https://codecov.io/gh/whitemech/pddl)
[![](https://img.shields.io/badge/flake8-checked-blueviolet)](https://img.shields.io/badge/flake8-checked-blueviolet)
[![](https://img.shields.io/badge/mypy-checked-blue)](https://img.shields.io/badge/mypy-checked-blue)
[![](https://img.shields.io/badge/license-LGPLv3%2B-blue)](./LICENSE)

pddl aims at being the state-of-the-art parser for Fully Observable Non Deterministic (FOND) PDDL planning problems.

## Prerequisites

This tool is also based on the following libraries:

- [lark-parser 0.9.0](https://pypi.org/project/lark-parser/)

It is automatically added while installing pddl.

## Install

- from [PyPI](https://pypi.org/project/pddl/):
```
pip install pddl
```
- or, from source (`master` branch):
```
pip install git+https://github.com/whitemech/pddl.git
```

- or, clone the repository and install:
```
git clone https://github.com/whitemech/pddl.git
cd pddl
pip install .
```
## How To Use


## Features


## Tests

To run tests: `tox`

To run only the code tests: `tox -e py37`

To run only the code style checks: `tox -e flake8`

## Docs

To build the docs: `mkdocs build`

To view documentation in a browser: `mkdocs serve`
and then go to [http://localhost:8000](http://localhost:8000)

## License

fond-pddl is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2018-2020 WhiteMech

## Author

[Francesco Fuggitti](https://francescofuggitti.github.io/)
