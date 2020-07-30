# pddl

![test](https://github.com/whitemech/pddl/workflows/test/badge.svg)
![lint](https://github.com/whitemech/pddl/workflows/lint/badge.svg)
![docs](https://github.com/whitemech/pddl/workflows/docs/badge.svg)
[![](https://img.shields.io/badge/docs-mkdocs-9cf)](https://www.mkdocs.org/)
[![](https://img.shields.io/badge/status-development-orange.svg)](https://img.shields.io/badge/status-development-orange.svg)
[![codecov](https://codecov.io/gh/whitemech/pddl/branch/master/graph/badge.svg?token=FG3ATGP5P5)](https://codecov.io/gh/whitemech/pddl)
[![](https://img.shields.io/badge/flake8-checked-blueviolet)](https://img.shields.io/badge/flake8-checked-blueviolet)
[![](https://img.shields.io/badge/mypy-checked-blue)](https://img.shields.io/badge/mypy-checked-blue)
[![](https://img.shields.io/badge/license-LGPLv3%2B-blue)](./LICENSE)

pddl aims at being the state-of-the-art parser for Fully Observable Non Deterministic (FOND) PDDL planning problems.

## Prerequisites

pddl is based on the following libraries:

- [lark-parser 0.9.0](https://pypi.org/project/lark-parser/)

## Install

- from source (`master` branch):
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
TBA

## Features

- Support for parsing [PDDL 3.1](https://helios.hud.ac.uk/scommv/IPC-14/repository/kovacs-pddl-3.1-2011.pdf)
- Support for the `non-deterministic` key requirement ([6th IPC: Uncertainty Part](https://pdfs.semanticscholar.org/b3d5/3dcc8183048849788405be5bfbb08a364dc5.pdf))

## Tests

To run tests: `tox`

To run only the code tests: `tox -e py37`

To run only the code style checks: `tox -e flake8`

## Docs

To build the docs: `mkdocs build`

To view documentation in a browser: `mkdocs serve`
and then go to [http://localhost:8000](http://localhost:8000)

## License

pddl is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2020 WhiteMech

## Authors

- [Marco Favorito](https://marcofavorito.github.io/)
- [Francesco Fuggitti](https://francescofuggitti.github.io/)
