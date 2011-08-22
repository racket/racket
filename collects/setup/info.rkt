#lang setup/infotab

(define compile-omit-paths '("main.rkt"))

(define mzscheme-launcher-libraries '("main.rkt"))
(define mzscheme-launcher-names '("Setup PLT"))

(define raco-commands '(("setup" setup/main "install and build libraries and documentation" 90)
                        ("link" setup/commands/link "manage library-collection directories" 80)))
