(module web-server mzscheme
  (require (lib "manual.ss" "scribble")
           (lib "eval.ss" "scribble"))

  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "eval.ss" "scribble"))))