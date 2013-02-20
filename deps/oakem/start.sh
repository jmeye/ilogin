#!/bin/bash
cd `dirname $0`

exec erl +K true -pa $PWD/ebin  -sname oakpool@localhost -boot start_sasl -s oakem_app
