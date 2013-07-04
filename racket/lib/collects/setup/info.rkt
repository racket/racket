#lang info

(define compile-omit-paths '("main.rkt"))

(define raco-commands '(("setup" setup/main "install and build libraries and documentation" 90)
                        ("link" setup/commands/link "manage library-collection directories" #f)))
