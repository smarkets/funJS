#!/bin/sh

erl -pa ebin/ deps/*/ebin -s client start_shell -boot start_sasl -name client@localhost -config sys.config
