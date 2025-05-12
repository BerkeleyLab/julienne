#!/bin/bash
output=$(fpm run --example false-assertion --compiler ifx --flag '-O3 -DASSERTIONS' > /dev/null 2>&1)
echo $?
