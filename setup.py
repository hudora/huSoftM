"""
Handling of communication with the SoftM Suite on an AS/400.
See https://cybernetics.hudora.biz/projects/wiki/HuSoftm for details.
"""

from ez_setup import use_setuptools
use_setuptools()
from setuptools import setup, find_packages


hubarcode = setup(name='huSoftM',
      maintainer='Maximillian Dornseif',
      maintainer_email='md@hudora.de',
      url='https://cybernetics.hudora.biz/projects/wiki/HuSoftm',
      version='0.51p2',
      description='communication with the SoftM Suite on an AS/400',
      long_description=__doc__,
      classifiers=['License :: OSI Approved :: BSD License',
                   'Intended Audience :: Developers',
                   'Programming Language :: Python'],
      zip_safe=False,
      packages = find_packages(),
      install_requires=['simplejson', 'couchdb', 'python-memcached'],
)
