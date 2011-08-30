#lang honu

macro testx () {x:expression} {syntax(x_result + 1)}

testx 5 * 2;
