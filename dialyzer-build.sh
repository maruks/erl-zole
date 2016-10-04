#!/bin/bash

dialyzer --build_plt _build/default/lib/lager/ebin --apps erts kernel stdlib crypto mnesia sasl
