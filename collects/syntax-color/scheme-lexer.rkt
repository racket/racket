#lang racket/base
#|

This file is provided for backwards compatibility.
New code should use racket-lexer.rkt.

|#
(require "racket-lexer.rkt")
(provide 
 (rename-out [racket-lexer scheme-lexer]
             [racket-lexer/status scheme-lexer/status]
             [racket-nobar-lexer/status scheme-nobar-lexer/status]))
