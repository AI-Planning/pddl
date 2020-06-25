# -*- coding: utf-8 -*-
"""This module contains the implementation of the parsers for the supported PDDL formalisms."""
import inspect
import os

CUR_DIR = os.path.dirname(inspect.getfile(inspect.currentframe()))  # type: ignore