#!/bin/bash
output=$(fpm run --example false-assertion --compiler flang-new --flag '-mmlir -allow-assumed-rank -O3 -DASSERTIONS' > /dev/null 2>&1)
echo $?
