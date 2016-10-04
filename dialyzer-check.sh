#!/bin/bash

dialyzer _build/default/lib/zole/ebin -Wrace_conditions -Wunderspecs
