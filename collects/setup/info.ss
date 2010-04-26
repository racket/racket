#lang setup/infotab

(define compile-omit-paths '("main.ss"))

(define mzscheme-launcher-libraries '("main.ss"))
(define mzscheme-launcher-names '("Setup PLT"))

(define raco-commands '(("setup" setup/main "install and build libraries and documentation" 90)))
