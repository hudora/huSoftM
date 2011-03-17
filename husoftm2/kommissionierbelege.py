#!/usr/bin/env python
# encoding: utf-8

import warnings
from husoftm2.kommiauftraege import get_kommiauftrag, get_kommibeleg


__all__ = ['get_kommiauftrag', 'get_kommibeleg']


warnings.warn("husoftm2.kommissionierbelege is obsolete, use husoftm2.kommiauftraege instead",
              DeprecationWarning)
