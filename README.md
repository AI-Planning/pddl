<h1 align="center">
  <b>pddl</b>
</h1>

<p align="center">
  <a href="https://pypi.org/project/yarllib">
    <img alt="PyPI" src="https://img.shields.io/pypi/v/yarllib">
  </a>
  <a href="https://pypi.org/project/yarllib">
    <img alt="PyPI - Python Version" src="https://img.shields.io/pypi/pyversions/yarllib" />
  </a>
  <a href="">
    <img alt="PyPI - Status" src="https://img.shields.io/pypi/status/yarllib" />
  </a>
  <a href="">
    <img alt="PyPI - Implementation" src="https://img.shields.io/pypi/implementation/yarllib">
  </a>
  <a href="">
    <img alt="PyPI - Wheel" src="https://img.shields.io/pypi/wheel/yarllib">
  </a>
  <a href="./LICENSE">
    <img alt="" src="https://img.shields.io/badge/license-LGPLv3%2B-blue">
  </a>  
</p>
<p align="center">
  <a href="">
    <img alt="test" src="https://github.com/whitemech/pddl/workflows/test/badge.svg">
  </a>
  <a href="">
    <img alt="lint" src="https://github.com/whitemech/pddl/workflows/lint/badge.svg">
  </a>
  <a href="">
    <img alt="docs" src="https://github.com/whitemech/pddl/workflows/docs/badge.svg">
  </a>
  <a href="https://codecov.io/gh/whitemech/pddl">
    <img alt="codecov" src="https://codecov.io/gh/whitemech/pddl/branch/master/graph/badge.svg?token=FG3ATGP5P5">
  </a>
</p>
<p align="center">
  <a href="https://img.shields.io/badge/flake8-checked-blueviolet">
    <img alt="" src="https://img.shields.io/badge/flake8-checked-blueviolet">
  </a>
  <a href="https://img.shields.io/badge/mypy-checked-blue">
    <img alt="" src="https://img.shields.io/badge/mypy-checked-blue">
  </a>
  <a href="https://img.shields.io/badge/code%20style-black-black">
    <img alt="black" src="https://img.shields.io/badge/code%20style-black-black" />
  </a>
  <a href="https://www.mkdocs.org/">
    <img alt="" src="https://img.shields.io/badge/docs-mkdocs-9cf">
  </a>
</p>

`pddl` aims to be an unquestionable and complete parser for PDDL 3.1.

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

Supported [PDDL 3.1](https://helios.hud.ac.uk/scommv/IPC-14/repository/kovacs-pddl-3.1-2011.pdf) 
requirements:
- [ ] `:strips`
- [X] `:typing`
- [ ] `:negative-preconditions`
- [ ] `:disjunctive-preconditions`
- [X] `:equality`
- [ ] `:existential-preconditions`
- [ ] `:universal-preconditions`
- [ ] `:quantified-preconditions`
- [ ] `:conditional-effects`
- [ ] `:fluents`
- [ ] `:numeric-fluents`
- [X] `:non-deterministic` (see [6th IPC: Uncertainty Part](https://pdfs.semanticscholar.org/b3d5/3dcc8183048849788405be5bfbb08a364dc5.pdf))
- [ ] `:adl`
- [ ] `:durative-actions`
- [ ] `:duration-inequalities`
- [ ] `:derived-predicates`
- [ ] `:timed-initial-literals`
- [ ] `:preferences`
- [ ] `:constraints`
- [ ] `:action-costs`


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
