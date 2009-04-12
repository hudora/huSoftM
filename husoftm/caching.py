#!/usr/bin/env python
# encoding: utf-8
"""
caching.py

Created by Maximillian Dornseif on 2009-04-12.
Copyright (c) 2009 HUDORA. All rights reserved.
"""


import memcache


# TODO: service basierte benamung memcache.hudora.biz  + monitoring
def get_cache():
    return memcache.Client(['balancer.local.hudora.biz:11211'])

