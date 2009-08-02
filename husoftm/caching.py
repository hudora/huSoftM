#!/usr/bin/env python
# encoding: utf-8
"""
caching.py

Created by Maximillian Dornseif on 2009-04-12.
Copyright (c) 2009 HUDORA. All rights reserved.
"""


import memcache

# TODO: service basierte benamung memcache.local.hudora.biz + monitoring
_cache = memcache.Client(['balancer.local.hudora.biz:11211'])


def get_cache():
    return _cache
