#lang info

(define raco-commands
  '(("make" compiler/commands/make "compile source to bytecode" 100)
    ("exe" compiler/commands/exe "create executable" 20)
    ("pack" compiler/commands/pack "pack files/collections into a .plt archive" #f)
    ("unpack" compiler/commands/unpack "unpack files/collections from a .plt archive" #f)
    ("decompile" compiler/commands/decompile "decompile bytecode" #f)
    ("test" compiler/commands/test "run tests associated with files/directories" 15)
    ("expand" compiler/commands/expand "macro-expand source" #f)
    ("read" compiler/commands/read "read and pretty-print source" #f)
    ("distribute" compiler/commands/exe-dir "prepare executable(s) in a directory for distribution" #f)
    ("demodularize" compiler/demodularizer/batch "produce a whole program from a single module" #f)))

(define test-responsibles '(("test.rkt" jay)))
