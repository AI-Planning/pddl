from pddl.formatter import domain_to_string, problem_to_string

from pddl.core import (
    Domain,
    Problem,
    Function,
    Requirements,
    Action,
    Variable,
    Constant,
)
from pddl.logic.functions import LesserEqualThan, Increase, EqualTo, GreaterEqualThan
from pddl.logic.effects import AndEffect
from pddl.logic.base import ForallCondition, Number


def test_numerical_hello_world_domain_formatter():
    neighbor = Variable("neighbor")
    hello_counter = Function("hello_counter", neighbor)
    action = Action(
        "say-hello-world",
        parameters=[neighbor],
        precondition=LesserEqualThan(hello_counter, Number(3)),
        effect=AndEffect(Increase(hello_counter, Number(1))),
    )

    domain = Domain(
        name="hello-world-functions",
        requirements=[Requirements.STRIPS, Requirements.FLUENTS],
        functions=[hello_counter],
        actions=[action],
    )

    assert domain_to_string(domain) == "\n".join(
        (
            "(define (domain hello-world-functions)",
            "    (:requirements :fluents :strips)",
            "    (:functions (hello_counter ?neighbor))",
            "    (:action say-hello-world",
            "        :parameters (?neighbor)",
            "        :precondition (<= (hello_counter ?neighbor) 3)",
            "        :effect (and (increase (hello_counter ?neighbor) 1))",
            "    )",
            ")",
        )
    )


def test_numerical_hello_world_problem_formatter():
    neighbors = [Constant(name, ["neighbor"]) for name in ("Alice", "Bob", "Charlie")]
    problem = Problem(
        name="hello-3-times",
        domain_name="hello-world-functions",
        objects=neighbors,
        init=[
            EqualTo(Function("hello_counter", neighbor), Number(0))
            for neighbor in neighbors
        ],
        goal=ForallCondition(
            GreaterEqualThan(
                Function("hello_counter", Variable("neighbor")), Number(1)
            ),
            [Variable("neighbor", ["neighbor"])],
        ),
    )

    assert problem_to_string(problem) == "\n".join(
        (
            "(define (problem hello-3-times)",
            "    (:domain hello-world-functions)",
            "    (:objects Alice - neighbor Bob - neighbor Charlie - neighbor)",
            "    (:init (= (hello_counter Alice) 0) (= (hello_counter Bob) 0) (= (hello_counter Charlie) 0))",
            "    (:goal (forall (?neighbor - neighbor) (>= (hello_counter ?neighbor) 1)))",
            ")",
        )
    )
