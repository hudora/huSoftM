#!/bin/sh
# start odbv_bridge
cd `dirname $0`
mkdir -p tmp/pipe/ ./log/
run_erl -daemon ./tmp/pipe/ ./log/ \
"exec erl -heart -sname odbc_bridge@localhost -pa ./ebin ./deps/*/ebin -boot start_sasl -s odbc_bridge"
