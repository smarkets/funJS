#!/bin/sh

erl -pa ebin/ deps/*/ebin -s price start_shell -boot start_sasl -name price@localhost -config sys.config
