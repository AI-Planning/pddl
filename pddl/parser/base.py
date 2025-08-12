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

"""Base classes for PDDL parsers."""
import sys
from abc import ABC
from typing import Any, Type, TypeVar

from lark import Lark, Transformer

from pddl.parser import GRAMMAR_FILE, PARSERS_DIRECTORY

T = TypeVar("T")


class BaseParser(Transformer[Any, T], ABC):
    """Base class for PDDL parsers."""

    start_symbol: str
    transformer_cls: Type[Transformer[Any, T]]

    def __init__(self, *args, **kwargs) -> None:
        """Initialize the parser with the given grammar file."""
        super().__init__(*args, **kwargs)
        self._transformer = self.transformer_cls()
        self._parser = Lark(
            GRAMMAR_FILE.read_text(),
            parser="lalr",
            import_paths=[PARSERS_DIRECTORY],
            start=self.start_symbol,
        )

    def __call__(self, text: str) -> T:
        """Call."""
        return self._call_parser(text, self._parser, self._transformer)

    @classmethod
    def _call_parser(
        cls, text: str, parser: Lark, transformer: Transformer[Any, T]
    ) -> T:
        """
        Parse a text with a Lark parser and transformer.

        To produce a better traceback in case of an error, the function will temporarily overwrite the
        sys.tracebacklimit value of the current interpreter.

        :param text: the text to parse
        :param parser: the Lark parser object
        :param transformer: the Lark transformer object
        :return: the object returned by the parser
        """
        old_tracebacklimit = getattr(sys, "tracebacklimit", None)
        try:
            sys.tracebacklimit = 0  # noqa
            tree = parser.parse(text)
            sys.tracebacklimit = None  # type: ignore
            result = transformer.transform(tree)
        finally:
            if old_tracebacklimit is not None:
                sys.tracebacklimit = old_tracebacklimit
        return result
