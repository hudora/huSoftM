# setting the PATH seems only to work in GNUmake not in BSDmake
PATH:=testenv/bin:$(PATH)

default: dependencies check test statistics

check:
	find husoftm -name '*.py' | xargs /usr/local/hudorakit/bin/hd_pep8
	/usr/local/hudorakit/bin/hd_pylint -f parseable husoftm | tee pylint.out

build:
	python setup.py build sdist bdist_egg

test:
	PYTHONPATH=. python husoftm/artikel.py
	PYTHONPATH=. python husoftm/connection2.py
	PYTHONPATH=. python husoftm/kunden.py
	PYTHONPATH=. python husoftm/lagerschnittstelle.py
	PYTHONPATH=. python husoftm/lieferscheine.py
	PYTHONPATH=. python husoftm/misc.py
	PYTHONPATH=. python husoftm/preise_ek.py
	PYTHONPATH=. python husoftm/softmtables.py
	PYTHONPATH=. python husoftm/stapelschnittstelle.py
	PYTHONPATH=. python husoftm/tools.py
	PYTHONPATH=. python husoftm/bestaende.py # slow tests

dependencies:
	virtualenv testenv
	pip -q install -E testenv -r requirements.txt

statistics:
	sloccount --wide --details . | grep -v -E '(testenv|build|.svn)/' > sloccount.sc

upload: build doc
	rsync dist/* root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/huSoftM/
	rsync dist/* root@cybernetics.hudora.biz:/usr/local/www/apache22/data/nonpublic/eggs/
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

install: build
	sh -c 'sudo python setup.py install'

clean:
	rm -Rf testenv build dist html test.db huSoftM.egg-info svn-commit.tmp pylint.out sloccount.sc pip-log.txt
	find . -name '*.pyc' -or -name '*.pyo' -or -name 'biketextmate.log' -delete

.PHONY: build test
