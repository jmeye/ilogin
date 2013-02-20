#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl +K true +A 16 +a 2048 -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname ilogin_dev \
    -s ilogin \
    -s reloader
