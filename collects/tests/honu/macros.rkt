#lang honu

macro testx () {x:expression} {syntax(x_result + 1)}

testx 5 * 2

for z = 1 to testx 5 * 2 do
  printf("z is ~a\n", z)
