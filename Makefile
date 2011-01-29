default: dependencies check test

check:
	pep8 -r --ignore=E501 husoftm2/
	sh -c 'PYTHONPATH=. pyflakes husoftm2/'
	-sh -c 'PYTHONPATH=. pylint -iy --max-line-length=110 husoftm2/' # -rn

build:
	python setup.py build sdist

test: dependencies
	PYTHONPATH=. python husoftm2/tools.py
	#PYTHONPATH=. python husoftm/connection2.py
	#PYTHONPATH=. python husoftm/lieferanten.py
	#PYTHONPATH=. python husoftm/misc.py
	#PYTHONPATH=. python husoftm/preise_ek.py
	#PYTHONPATH=. ./pythonenv/bin/python husoftm/stapelschnittstelle.py
	#PYTHONPATH=. python husoftm/lagerschnittstelle.py
	# dependencies on CentralServices
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/artikel.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/auftraege.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/bestaende.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/kunden.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/lieferscheine.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/preise.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/texte.py
	PYTHONPATH=. ./pythonenv/bin/python husoftm2/tools.py

coverage: dependencies
	printf '.*/tests/.*\n.*test.py\n' > .figleaf-exclude.txt
	printf '/usr/local/lib/.*\n/opt/.*\npythonenv/.*\n' >> .figleaf-exclude.txt
	printf '.*manage.py\n.*settings.py\n.*setup.py\n.*urls.py\n' >> .figleaf-exclude.txt
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/artikel.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/connection2.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/kunden.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/lagerschnittstelle.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/lieferanten.py  
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/lieferscheine.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/misc.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/preise_ek.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/softmtables.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/stapelschnittstelle.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/tools.py
	PYTHONPATH=. python /usr/local/hudorakit/bin/hd_figleaf --ignore-pylibs husoftm/bestaende.py
	python /usr/local/hudorakit/bin/hd_figleaf2html -d ./coverage -x .figleaf-exclude.txt
	echo "Coverage: " `grep -A3 ">totals:<" coverage/index.html|tail -n1|cut -c 9-13|cut -d'<' -f1`
	test `grep -A3 ">totals:<" coverage/index.html|tail -n1|cut -c 9-13|cut -d'.' -f1` -gt 70
	printf 'YVALUE=' > .coverage.score
	grep -A3 ">totals:<" coverage/index.html|tail -n1|cut -c 9-12 >> .coverage.score

dependencies: pythonenv/bin/python

pythonenv/bin/python:
	virtualenv pythonenv
	pip -q install -E pythonenv -r requirements.txt


statistics:
	sloccount --wide --details . | grep -v -E '(testenv|build|.svn)/' > sloccount.sc

upload: build
	python setup.py sdist upload

publish:
	# remove development tag
	perl -npe 's/^tag_build = .dev/# tag_build = .dev/' -i setup.cfg
	svn commit
	python setup.py build sdist bdist_egg
	# add development tag
	perl -npe 's/^\# tag_build = .dev/tag_build = .dev/' -i setup.cfg
	rsync dist/* root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/huSoftM/
	echo "now bump version number in setup.py and commit"

testenv_pydoc: dependencies
	echo '#!/usr/bin/env python' > testenv/bin/pydoc.x
	cat `which pydoc` >> testenv/bin/pydoc.x
	chmod +x testenv/bin/pydoc.x

doc: build testenv_pydoc
	rm -Rf html
	mkdir -p html
	sh -c '(export PYTHONPATH=$(PWD); cd html; pydoc.x -w ../husoftm/*.py)'

install: build
	python setup.py install

clean:
	rm -Rf pythonenv build dist html test.db huSoftM.egg-info svn-commit.tmp pylint.out .coverage.score sloccount.sc pip-log.txt as400-sqlite-test.db
	find . -name '*.pyc' -or -name '*.pyo' -or -name 'biketextmate.log' -delete

.PHONY: build test
