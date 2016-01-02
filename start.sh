#!/bin/bash

export ERL_LIBS=./_build/default/lib/zole:./_build/default/lib/goldrush:./_build/default/lib/lager

erl -config app.config -run zole_app
