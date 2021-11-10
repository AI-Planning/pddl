<h1 align="center">
  <b>pddl</b>
</h1>

<p align="center">
  <a href="https://pypi.org/project/pddl">
    <img alt="PyPI" src="https://img.shields.io/pypi/v/pddl">
  </a>
  <a href="https://pypi.org/project/pddl">
    <img alt="PyPI - Python Version" src="https://img.shields.io/pypi/pyversions/pddl" />
  </a>
  <a href="">
    <img alt="PyPI - Status" src="https://img.shields.io/pypi/status/pddl" />
  </a>
  <a href="">
    <img alt="PyPI - Implementation" src="https://img.shields.io/pypi/implementation/pddl">
  </a>
  <a href="">
    <img alt="PyPI - Wheel" src="https://img.shields.io/pypi/wheel/pddl">
  </a>
  <a href="https://github.com/whitemech/pddl/blob/master/LICENSE">
    <img alt="GitHub" src="https://img.shields.io/github/license/whitemech/pddl">
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
## Quickstart

You can use the `pddl` package in two ways: as a library, and as a CLI tool.

### As a library

This is an example of how you can build a PDDL domain or problem 
programmatically:
```python
from pddl.logic import Predicate, constants, variables
from pddl.core import Domain, Problem, Action, Requirements
from pddl.formatter import domain_to_string, problem_to_string

# set up variables and constants
x, y, z = variables("x y z", types=["type_1"])
a, b, c = constants("a b c", types=["type_1"])

# define predicates
p1 = Predicate("p1", x, y, z)
p2 = Predicate("p2", x, y)

# define actions
a1 = Action(
    "action-1",
    parameters=[x, y, z],
    precondition=p1(x, y, z) & ~p2(y, z),
    effect=p2(y, z)
)

# define the domain object.
requirements = [Requirements.STRIPS, Requirements.TYPING]
domain = Domain("my_domain",
       requirements=requirements,
       types=["type_1"],
       constants=[a, b, c],
       predicates=[p1, p2],
       actions=[a1])

print(domain_to_string(domain))
```

that gives:
```output
(define (domain my_domain)
    (:requirements :strips :typing)
    (:types type_1)
    (:constants a b c)
    (:predicates (p1 ?x ?y ?z) (p2 ?x ?y))
    (:action action-1
        :parameters (?x ?y ?z)
        :precondition (and (p1 ?x ?y ?z) (not (p2 ?y ?z)))
        :effect (p2 ?y ?z)
    )
)
```

As well as a PDDL problem:
```python
problem = Problem(
    "problem-1",
    domain=domain,
    requirements=requirements,
    objects=[a, b, c],
    init=[p1(a, b, c), ~p2(b, c)],
    goal=p2(b, c)
)
print(problem_to_string(problem))
```

Output:
```output
(define (problem problem-1)
    (:domain my_domain)
    (:requirements :strips :typing)
    (:objects a b c)
    (:init (not (p2 b c)) (p1 a b c))
    (:goal (p2 b c))
)
```

### As CLI tool

The package can also be used as a CLI tool.
Supported commands are:
- `pddl domain FILE`: validate a PDDL domain file, and print it formatted.
- `pddl problem FILE`: validate a PDDL problem file, and print it formatted.

## Features

Supported [PDDL 3.1](https://helios.hud.ac.uk/scommv/IPC-14/repository/kovacs-pddl-3.1-2011.pdf) 
requirements:  

- [x] `:strips`  
- [x] `:typing`  
- [x] `:negative-preconditions`  
- [x] `:disjunctive-preconditions`  
- [x] `:equality`  
- [ ] `:existential-preconditions`  
- [ ] `:universal-preconditions`  
- [ ] `:quantified-preconditions`  
- [x] `:conditional-effects`  
- [ ] `:fluents`  
- [ ] `:numeric-fluents`  
- [x] `:non-deterministic` (see [6th IPC: Uncertainty Part](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.163.7140&rep=rep1&type=pdf))  
- [ ] `:adl`  
- [ ] `:durative-actions`  
- [ ] `:duration-inequalities`  
- [x] `:derived-predicates`  
- [ ] `:timed-initial-literals`  
- [ ] `:preferences`  
- [ ] `:constraints`  
- [ ] `:action-costs`  

## Development

If you want to contribute, here's how to set up your development environment.

- Install [Pipenv](https://pipenv-fork.readthedocs.io/en/latest/)
- Clone the repository: `git clone https://github.com/whitemech/pddl.git && cd pddl`
- Install development dependencies: `pipenv shell --python 3.7 && pipenv install --dev`

## Tests

To run tests: `tox`

To run only the code tests: `tox -e py37`

To run only the code style checks: `tox -e flake8`

## Docs

To build the docs: `mkdocs build`

To view documentation in a browser: `mkdocs serve`
and then go to [http://localhost:8000](http://localhost:8000)

## Authors

- [Marco Favorito](https://marcofavorito.github.io/)
- [Francesco Fuggitti](https://francescofuggitti.github.io/)

## License

`pddl` is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2021 WhiteMech
