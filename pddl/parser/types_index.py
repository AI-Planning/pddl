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

"""Utility to handle typed lists."""
import itertools
from collections import OrderedDict
from typing import Dict, List, Optional
from typing import OrderedDict as OrderedDictType
from typing import Set, Union

from pddl.custom_types import name
from pddl.helpers.base import check, safe_index
from pddl.parser.symbols import Symbols


class TypesIndex:
    """
    An index for PDDL types and PDDL names/variables.

    This class is used to index PDDL names and variables by their types.
    OrderedDict is used to preserve the order of the types and the names, e.g. for predicate variables.

    Other types of validations are performed to ensure that the types index is consistent.
    """

    def __init__(self) -> None:
        """Initialize the types index."""
        self._types_to_items: OrderedDictType[name, Set[name]] = OrderedDict()
        self._item_to_types: OrderedDictType[name, Set[name]] = OrderedDict()

    def add_item(self, item_name: name, type_tags: Set[name]) -> None:
        """
        Add an item to the types index with the given type tags.

        Both the item name and the type tags are validated according to the name type regular expression.

        :param item_name: the item name
        :param type_tags: the types for the item
        """
        self._check_item_name_already_present(item_name)
        self._check_tags_already_present(item_name, type_tags)
        self._add_item(item_name, type_tags)

    def get_typed_list_of_names(self) -> Dict[name, Optional[name]]:
        """Get the typed list of names in form of dictionary."""
        result: Dict[name, Optional[name]] = {}
        for item, types_tags in self._item_to_types.items():
            if len(types_tags) > 1:
                self._raise_multiple_types_error(item, types_tags)
            type_tag = next(iter(types_tags)) if len(types_tags) == 1 else None
            result[item] = type_tag
        return result

    @classmethod
    def parse_typed_list(cls, tokens: List[Union[str, List[str]]]) -> "TypesIndex":
        """
        Parse typed list.

        This method takes in input a list of tokens as returned by the domain or problem parsers,
        and returns a TypesIndex object.

        The input list of tokens must have the following format:
        - if the list is not typed, it is simply a list of names
        - if the list is typed, the format is: [name_1, ..., name_n], "-", [type_1, ..., type_m], ...

        >>> index = TypesIndex.parse_typed_list(["a", "b", "c"])
        >>> index.get_typed_list_of_names()
        {'a': None, 'b': None, 'c': None}

        >>> index = TypesIndex.parse_typed_list(["a", "b", "c", "-", ["t1"]])
        >>> index.get_typed_list_of_names()
        {'a': 't1', 'b': 't1', 'c': 't1'}

        >>> index = TypesIndex.parse_typed_list(["a", "b", "c", "-", ["t1", "t2"]])
        >>> index.get_typed_list_of_names()
        Traceback (most recent call last):
        ...
        ValueError: typed list names should not have more than one type, got 'a' with types ['t1', 't2']

        :param tokens: the list of tokens
        :return: the TypesIndex object
        """
        result = TypesIndex()

        type_sep_index = safe_index(tokens, Symbols.TYPE_SEP.value)

        if type_sep_index is None:
            # simple list of names
            cls._add_typed_lists(result, 0, len(tokens), tokens, set())
            return result

        # if we are here, the matched pattern is: [name_1, ..., name_n], "-", parent_name, ...
        # Consume typed sublists iteratively. Caveat: the last typed list *might* have no parent type.

        # the index of the separator symbol for the current typed sublist being processed
        type_sep_index = safe_index(tokens, Symbols.TYPE_SEP.value)
        # the index of the first element of the current typed sublist
        start_index = 0
        # the index of the last element of the current typed sublist
        end_index = len(tokens)
        while type_sep_index is not None:
            # the name of the parent type is the element after the separator
            parent_names = tokens[type_sep_index + 1]
            # handle the case of a single parent type
            if not isinstance(parent_names, list):
                parent_names = [parent_names]

            # parse the typed list
            cls._add_typed_lists(
                result, start_index, type_sep_index, tokens, set(parent_names)
            )

            # go to next typed list (if any)
            start_index = type_sep_index + 2
            type_sep_index = safe_index(
                tokens, Symbols.TYPE_SEP.value, start_index, end_index
            )

        # this is to handle the case the last sublist is not typed
        if start_index != end_index:
            # parse the last typed list, with no type.
            cls._add_typed_lists(result, start_index, end_index, tokens, set())

        return result

    @classmethod
    def _add_typed_lists(
        cls,
        result: "TypesIndex",
        start_index: int,
        end_index: int,
        tokens: List[Union[str, List[str]]],
        type_tags: Set[str],
    ) -> None:
        """
        Merge typed lists.

        Side-effect on the 'result' dictionary. The start_index and end_index are needed to avoid useless
        sublist copies.
        """
        for item_name in itertools.islice(tokens, start_index, end_index):
            check(
                isinstance(item_name, str), f"invalid item '{item_name}' in typed list"
            )
            # these lines implicitly perform name validation
            item_name = name(item_name)
            type_tags_names: Set[name] = set(map(name, type_tags))
            result.add_item(item_name, type_tags_names)

    def _check_item_name_already_present(self, item_name: name) -> None:
        """
        Check if the item name is already present in the index.

        :param item_name: the item name
        """
        if item_name in self._item_to_types:
            types_list = sorted(map(str, self._item_to_types[item_name]))
            types_list_str = f" with types {types_list}" if len(types_list) > 0 else ""
            raise ValueError(
                f"duplicate name '{item_name}' in typed list already present"
                + types_list_str
            )

    def _check_tags_already_present(
        self, item_name: name, type_tags: Set[name]
    ) -> None:
        """
        Check if the type tags are already present for the given item name.

        :param item_name: the item name
        :param type_tags: the type tags
        """
        exisiting_tags = self._item_to_types.get(item_name, set())
        for type_tag in type_tags:
            if type_tag in exisiting_tags:
                raise ValueError(
                    f"duplicate type tag '{type_tag}' in typed list: type already specified for item {item_name}"
                )

    def _add_item(self, item_name: name, type_tags: Set[name]) -> None:
        """Add an item (no validation)."""
        for type_tag in type_tags:
            self._types_to_items.setdefault(type_tag, set()).add(item_name)
        self._item_to_types.setdefault(item_name, set()).update(type_tags)

    def _raise_multiple_types_error(
        self, item_name: name, types_tags: Set[name]
    ) -> None:
        """Raise an error if the item has multiple types."""
        raise ValueError(
            f"typed list names should not have more than one type, got '{item_name}' with "
            f"types {sorted(map(str, types_tags))}"
        )
