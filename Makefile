check: clean
	find husoftm -name '*.py'  -exec pep8 --ignore=E501,W291 --repeat {} \;
	pylint husoftm

build:
	python setup.py build sdist bdist_egg

upload: build doc
	rsync dist/* root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/huSoftM/
	rsync husoftm/fields.py root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/huSoftM/fields.py
	rsync -r --delete html root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/huSoftM/

publish:
	# remove development tag
	perl -npe 's/^tag_build = .dev/# tag_build = .dev/' -i setup.cfg
	svn commit
	python setup.py build sdist bdist_egg
	# add development tag
	perl -npe 's/^\# tag_build = .dev/tag_build = .dev/' -i setup.cfg
	rsync dist/* root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/huSoftM/
	echo "now bump version number in setup.py and commit"

doc: build
	rm -Rf html
	mkdir -p html
	sh -c '(cd html; pydoc -w ../husoftm/*.py)'

test:
	PYTHONPATH=. python husoftm/tools.py
	PYTHONPATH=. python husoftm/stapelschnittstelle.py

install: build
	sh -c 'sudo python setup.py install'

clean:
	rm -Rf build dist html test.db huSoftM.egg-info
	find . -name '*.pyc' -or -name '*.pyo' -delete

.PHONY: build test
