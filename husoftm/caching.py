#!/usr/bin/env python
# encoding: utf-8
"""
caching.py

Created by Maximillian Dornseif on 2009-04-12.
Copyright (c) 2009 HUDORA. All rights reserved.
"""


import memcache



class Client(memcache.Client):
    def __del__(self):
        # see http://code.djangoproject.com/ticket/5133 for the problem we try to solve here
        self.disconnect_all()
        return super(Client, self).__del__()

# TODO: service basierte benamung memcache.local.hudora.biz + monitoring
def get_cache():
    return Client(['balancer.local.hudora.biz:11211'])

