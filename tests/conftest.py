# -*- coding: utf-8 -*-
"""This module contains the configurations for the tests."""
import inspect
import os
from pathlib import Path

CUR_PATH = os.path.dirname(inspect.getfile(inspect.currentframe()))  # type: ignore
ROOT_DIR = str(Path(CUR_PATH, "..").resolve())  # type: ignore
