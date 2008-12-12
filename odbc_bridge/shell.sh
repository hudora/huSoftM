#!/bin/sh
# connect to a running odbc_bridge instance
erl -sname oddc_admin -remsh odbc_bridge@localhost
