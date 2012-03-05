#lang honu

macro testx () {x:expression} {
  var out1 = syntax(x)
  var out2 = syntax(+ 1)
  mergeSyntax(out1, out2)
}

testx 5 * 2

for z in 1 to testx 6 * 2 do
  printf("z is ~a\n", z)

macro testfor () {x:expression} {
  syntax(for z in 1 to x do
    printf("z is ~a\n" z))
}

macro testfor2 () {x:expression}{
  syntax(testfor x * 2)
}

testfor2 1 + 2
