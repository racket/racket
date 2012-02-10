#lang racket

(require drracket/private/get-defs
         racket/gui
         string-constants)

(define (get-definitions/string
         string
         #:define-prefix [define-prefix "(define"])
  (define text (new text%))
  (send text insert (make-object string-snip% string))
  (get-definitions define-prefix #f text))

(define-syntax (test-definitions stx)
  (syntax-case stx ()
    [(_ string ((name start end) ...))
     #`(let ([actual (map (match-lambda [(defn _ n s e)
                                         (list n s e)])
                          (get-definitions/string string))]
             [expected (list (list name start end) ...)])
         (unless (equal? actual expected)
           (eprintf "Test failure at ~a\nActual: ~s\nExpected: ~s\n"
                    (format "~a:~a:~a"
                            '(syntax-source #'stx)
                            '(syntax-line #'stx)
                            '(syntax-column #'stx))
                    actual
                    expected)))]))

(test-definitions 
 #<<END
(define x 1)
(define (f x)
  (define y x)
  y)
(define y 2)
(define
END
 (("x" 0 12)
  ; The end positions for f and the inner y look wrong to me.
  ; If they look wrong to you too (but you know what you're doing),
  ; please change the tests.
  ("f" 13 28) 
  ("y" 29 46)
  ("y" 47 59)
  ((string-constant end-of-buffer-define) 60 67)))

(test-definitions 
 #<<END
(define-metafunction L
  f : p -> q
  [(f p) t])
(define-metafunction L
  [(g p) t])
(define-metafunction/extension f L
  h : p -> q
  [(h p) t])
(define-metafunction/extension f L
  [(i p) t])
(define-metafunction
END
 (("f" 0 48)
  ("g" 49 84)
  ("h" 85 145)
  ("i" 146 193)
  ((string-constant end-of-buffer-define) 194 214)))
