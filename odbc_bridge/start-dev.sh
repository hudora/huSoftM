#!/bin/sh
cd `dirname $0`
# start odbc_bridge in development mode
mkdir -p tmp/pipe/ ./log/
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s odbc_bridge
