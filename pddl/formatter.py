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

"""Formatting utilities for PDDL domains and problems."""
from typing import Callable, Collection, Dict, List, Optional, TypeVar

from pddl.custom_types import name
from pddl.logic.functions import NumericFunction
from pddl.logic.terms import Constant

T = TypeVar("T", name, NumericFunction)


def remove_empty_lines(s: str) -> str:
    """Remove empty lines from string."""
    return "\n".join(filter(str.strip, s.splitlines()))


def sort_and_print_collection(
    prefix,
    collection: Collection,
    postfix,
    to_string: Callable = str,
    is_mandatory: bool = False,
):
    r"""Produce the string of a PDDL section for a collection (e.g., requirements, actions, objects, etc.).

    Prefix starts the PDDL section, like "(:requirements"  or "(:actions"
    Postfix ends the section, usually with ")\n"
    The collection is sorted and printed as a string, using to_string to convert each element to a string.

    Args:
        prefix (str): start of the string
        collection (Collection): the collection of entities to report as a string
        postfix (str): the end of the string
        to_string (Callable, optional): the function to use to convert to string. Defaults to str.
        is_mandatory (bool, optional): if the string is mandatory even if the collection is empty. Defaults to False.

    Returns:
        str: a string with <prefix> <string of collection> <postfix>
    """
    if len(collection) > 0:
        return prefix + " ".join(sorted(map(to_string, collection))) + postfix
    elif is_mandatory:
        return prefix + postfix
    return ""


def print_types_or_functions_with_parents(
    prefix: str,
    types_dict: Dict[T, Optional[name]],
    postfix: str,
    to_string: Callable = str,
):
    """Print the type dictionary of a PDDL domain."""
    name_by_obj: Dict[Optional[T], List[name]] = {}
    for obj_name, parent_type in types_dict.items():
        name_by_obj.setdefault(parent_type, []).append(obj_name)  # type: ignore
    if not bool(name_by_obj):
        return ""
    return print_typed_lists(prefix, name_by_obj, postfix, to_string)


def print_constants(
    prefix, constants: Collection[Constant], postfix, to_string: Callable = str
):
    """Print constants in a PDDL domain."""
    term_by_type_tags: Dict[Optional[name], List[name]] = {}
    for c in constants:
        term_by_type_tags.setdefault(c.type_tag, []).append(c.name)
    if not bool(term_by_type_tags):
        return ""
    return print_typed_lists(prefix, term_by_type_tags, postfix, to_string)


def print_predicates_with_types(predicates: Collection):
    """Generate a string with predicates with type tags for the :predicates section."""
    result = ""
    for p in sorted(predicates):
        if p.arity == 0:
            result += f"({p.name})"
        else:
            result += f"({p.name}"
            for t in p.terms:
                if len(t.type_tags) > 1:
                    result += f" ?{t.name} - (either {' '.join(sorted(t.type_tags))})"
                else:
                    result += (
                        f" ?{t.name} - {sorted(t.type_tags)[0]}"
                        if t.type_tags
                        else f" ?{t.name}"
                    )
            result += ") "
        result += " "
    return result.strip()


def print_function_skeleton(function: NumericFunction) -> str:
    """Callable to print a function skeleton with type tags."""
    result = ""
    if function.arity == 0:
        result += f"({function.name})"
    else:
        result += f"({function.name}"
        for t in function.terms:
            result += (
                f" ?{t.name} - {sorted(t.type_tags)[0]}"
                if t.type_tags
                else f" ?{t.name}"
            )
        result += ")"
    return result


def print_typed_lists(
    prefix,
    names_by_obj: Dict[Optional[T], List[name]],
    postfix,
    to_string: Callable = str,
):
    """Print typed lists."""
    result = prefix

    if ":types" in prefix:
        # for mypy, convert names_by_obj to a dict with keys being strings (or None) and values list of strings
        super_to_subs = {
            str(k) if k is not None else None: v for k, v in names_by_obj.items()
        }

        # first print those types that have no parents
        subtypes = set()
        for subs in super_to_subs.values():
            subtypes |= set(subs)

        no_parents = set(super_to_subs.keys()) - subtypes
        no_parents |= set(super_to_subs.get(None, []))
        no_parents |= set(super_to_subs.get("object", []))
        no_parents.discard(None)
        no_parents.discard("object")

        result += (
            "\n    "
            + " ".join(sorted(to_string(n) for n in no_parents))
            + " - object\n"
        )

        # sort the rest of the parent types based on (1) if their subtypes are already defined and (2) their name
        remaining = sorted(str(k) for k in super_to_subs if k not in ("object", None))
        ind = 0
        while remaining:
            # if the index is out of range, we have a cycle in the types
            if ind >= len(remaining):
                raise ValueError("Cycle in types")

            # get the first type and its subtypes
            parent = remaining[ind]

            # check if parent is a subtype of something remaining
            allsubtypes = set()
            for k in remaining:
                allsubtypes |= set(super_to_subs[k])
            if parent in allsubtypes:
                ind += 1
            else:
                # print the parent and its subtypes
                result += "    " + (
                    " ".join(sorted(to_string(n) for n in super_to_subs[parent]))
                    + " - "
                    + to_string(parent)
                    + "\n"
                )
                # remove the parent from the remaining list
                remaining.remove(parent)
                # reset the index to 0
                ind = 0

    else:

        result += " "

        # names with no type will be printed at the end
        names_with_none_types = names_by_obj.pop(None, [])

        # print typed constants, first sorted by type, then by constant name
        for type_tag, typed_names in sorted(
            names_by_obj.items(), key=lambda type_and_name: type_and_name[0]  # type: ignore
        ):
            result += (
                " ".join(sorted(to_string(n) for n in typed_names)) + " - " + type_tag + " "  # type: ignore
            )

        if len(names_with_none_types) == 0:
            return result.strip() + postfix

        # print constants with no type
        result += " ".join(sorted(to_string(n) for n in names_with_none_types))

        if result == prefix + " ":
            result = result[:-1]

    result += postfix

    return result


def domain_to_string(domain) -> str:
    """Print a PDDL domain object."""
    return str(domain)


def problem_to_string(problem) -> str:
    """Print a PDDL problem object."""
    return str(problem)
