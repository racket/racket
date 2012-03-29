#lang setup/infotab

(define raco-commands
  '(("make" compiler/commands/make "compile source to bytecode" 100)
    ("exe" compiler/commands/exe "create executable" 20)
    ("pack" compiler/commands/pack "pack files/collections into a .plt archive" 10)
    ("unpack" compiler/commands/unpack "unpack files/collections from a .plt archive" #f)
    ("decompile" compiler/commands/decompile "decompile bytecode" #f)
    ("test" compiler/commands/test "run tests associated with files/directories" 15)
    ("expand" compiler/commands/expand "macro-expand source" #f)
    ("distribute" compiler/commands/exe-dir "prepare executable(s) in a directory for distribution" #f)
    ("ctool" compiler/commands/ctool "compile and link C-based extensions" #f)
    ("demodularize" compiler/demodularizer/batch "produce a whole program from a single module" #f)))
