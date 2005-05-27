(module lookup-def mzscheme
  (provide lookup-def)

  (require (lib "23.ss" "srfi")) ; ERROR
  (require "coutputs.ss")

  (require (lib "include.ss"))
  (include "lookup-def.scm"))

    