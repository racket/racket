#lang racket

(require drracket/private/get-defs
         racket/gui
         string-constants)

(define (get-definitions/string
         string
         #:define-prefix [define-prefix "(define"]
         #:indent? [indent? #f])
  (define text (new text%))
  (send text insert (make-object string-snip% string))
  (get-definitions define-prefix indent? text))

(define-syntax (test-definitions stx)
  (syntax-case stx ()
    [(_ string (name ...))
     #`(let ([actual (map defn-name (get-definitions/string string))]
             [expected (list name ...)])
         (unless (equal? actual expected)
           (eprintf "Test failure at ~a\nActual: ~s\nExpected: ~s\n"
                    #,(format "~a:~a:~a"
                              (syntax-source #'stx)
                              (syntax-line #'stx)
                              (syntax-column #'stx))
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
 ("x" "f" "y" "y" (string-constant end-of-buffer-define)))

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
 ("f" "g" "h" "i" (string-constant end-of-buffer-define)))