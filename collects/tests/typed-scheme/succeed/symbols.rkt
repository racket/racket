#lang typed/racket

(: foo Symbol)
(: bar Symbol)

(define foo 'foo)
(define bar 'bar)

(symbol? foo)
(symbol? 2)

(symbol-interned? foo)
(symbol-interned? (string->unreadable-symbol "bar"))
(symbol-interned? (string->uninterned-symbol "bar"))
(symbol-interned? (gensym foo))

(symbol-unreadable? (gensym))
(symbol-unreadable? foo)
(symbol-unreadable? (string->unreadable-symbol "bar"))
(symbol-unreadable? (string->uninterned-symbol "bar"))

(symbol->string foo)
(string->symbol (symbol->string foo))

