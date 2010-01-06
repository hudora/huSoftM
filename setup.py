#!/usr/bin/env python
# encoding: utf-8

"""Handling of communication with the SoftM Suite on an AS/400."""

from setuptools import setup, find_packages
import codecs


hubarcode = setup(name='huSoftM',
      maintainer='Maximillian Dornseif',
      maintainer_email='md@hudora.de',
      url='https://cybernetics.hudora.biz/projects/wiki/HuSoftm',
      version='0.57p2',
      description='communication with the SoftM Suite on an AS/400',
      long_description=codecs.open('README.rst', "r", "utf-8").read(),
      classifiers=['License :: OSI Approved :: BSD License',
                   'Intended Audience :: Developers',
                   'Programming Language :: Python'],
      zip_safe=False,
      packages = find_packages(),
      install_requires=['cs', 'simplejson', 'couchdb', 'python-memcached', 'huTools>=0.38p2', 'Pyro'],
      dependency_links = ['http://cybernetics.hudora.biz/nonpublic/eggs/',
                          'http://cybernetics.hudora.biz/dist/huTools/',
                          'http://ovh.dl.sourceforge.net/project/pyro/Pyro/3.9.1/']
)
