#!/bin/sh -x
gracket module-lang-test.rkt &&
gracket repl-test.rkt &&
gracket io.rkt &&
gracket language-test.rkt &&
gracket syncheck-test.rkt &&
gracket teachpack.rkt &&
gracket save-teaching-lang-file.rkt
