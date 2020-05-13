#!/bin/sh

base=$(dirname "$0")

function compile {
    glslc -g -O $1 -o $1.spv
}

compile $base/src/bin/reflect_test.vert
