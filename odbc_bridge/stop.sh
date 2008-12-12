#!/bin/sh
# stop a running odbc_bridge instance
cd `dirname $0`
erl_call -sname odbc_bridge -a "init stop"
