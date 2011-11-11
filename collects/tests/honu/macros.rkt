#lang honu

macro testx () {x:expression} {syntax(x + 1)}

testx 5 * 2

for z = 1 to testx 5 * 2 do
  printf("z is ~a\n", z)

macro testfor () {x:expression} {
  syntax(for z = 1 to x do
    printf("z is ~a\n" z))
}

macro testfor2 () {x:expression}{
  syntax(testfor x * 2)
}

testfor2 1 + 2
