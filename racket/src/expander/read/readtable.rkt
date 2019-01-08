#lang racket/base
(require "../common/inline.rkt"
         "../common/parameter-like.rkt"
         "config.rkt"
         "coerce.rkt"
         "parameter.rkt"
         "readtable-parameter.rkt"
         "special-comment.rkt")

(provide readtable-delimiter-ht
         make-readtable
         readtable?
         readtable-mapping
         current-readtable
         readtable-effective-char
         effective-char
         readtable-handler
         readtable-dispatch-handler
         readtable-apply
         readtable-symbol-parser
         readtable-equivalent-chars)

(struct readtable (symbol-parser ; parser for default token handling: symbol-or-number
                   ;; The character table maps characters to either a
                   ;; parsing function or another character whose
                   ;; default to use
                   char-ht
                   ;; The dispatch table maps character for `#` dispatch
                   dispatch-ht
                   ;; The delimter table maps a character to 'delimit,
                   ;; 'no-delimit, or a character whose default to use;
                   ;; absence of a mapping is the default for that character
                   delimiter-ht)
  #:property prop:readtable #t
  #:authentic)

(define (make-readtable rt . args)
  (unless (or (not rt) (readtable? rt))
    (raise-argument-error 'make-readtable "(or/c readtable? #f)" rt))
  (let loop ([args args]
             [symbol-parser (and rt (readtable-symbol-parser rt))]
             [char-ht (if rt (readtable-char-ht rt) #hasheqv())]
             [dispatch-ht (if rt (readtable-dispatch-ht rt) #hasheqv())]
             [delimiter-ht (if rt (readtable-delimiter-ht rt) #hasheqv())])
    (cond
     [(null? args) (readtable symbol-parser char-ht dispatch-ht delimiter-ht)]
     [else
      ;; Key is a character or #f
      (define key (car args))
      (unless (or (not key) (char? key))
        (raise-argument-error 'make-readtable "(or/c char? #f)" key))
      
      ;; Mode determines how the key is mapped
      (when (null? (cdr args))
        (cond
         [key (raise-arguments-error 'make-readtable
                                     (string-append "expected 'terminating-macro, 'non-terminating-macro, 'dispatch-macro,"
                                                    " or character argument after character argument")
                                     "character" key)]
         [else (raise-arguments-error 'make-readtable
                                      "expected 'non-terminating-macro after #f")]))
      (define mode (cadr args))
      (cond
       [key
        (unless (or (eq? mode 'terminating-macro)
                    (eq? mode 'non-terminating-macro)
                    (eq? mode 'dispatch-macro)
                    (char? mode))
          (raise-argument-error 'make-readtable
                                "(or/c 'terminating-macro 'non-terminating-macro 'dispatch-macro char?)"
                                mode))]
       [else
        (unless (eq? mode 'non-terminating-macro)
          (raise-arguments-error 'make-readtable
                                 "expected 'non-terminating-macro after #f"))])
      
      ;; Target is what the key is mapped to
      (when (null? (cddr args))
        (raise-arguments-error 'make-readtable
                               (if key
                                   "expected readtable or #f argument after character argument"
                                   "expected procedure argument after symbol argument")
                               "given" mode))
      (define target (caddr args))
      
      ;; Update the readtable
      (define rest-args (cdddr args))
      (cond
       [(not key)
        ;; Update symbol parser
        (unless (and (procedure? target) (procedure-arity-includes? target 6))
          (raise-argument-error 'make-readtable "(procedure-arity-includes/c 6)" target))
        (loop rest-args target char-ht dispatch-ht delimiter-ht)]
       [(eq? mode 'dispatch-macro)
        ;; Update `#`-triggered dispatch table
        (unless (and (procedure? target) (procedure-arity-includes? target 6))
          (raise-argument-error 'make-readtable "(procedure-arity-includes/c 6)" target))
        (loop rest-args symbol-parser char-ht (hash-set dispatch-ht key target) delimiter-ht)]
       [(char? mode)
        ;; Update main character table with a character alias
        (unless (or (not target) (readtable? target))
          (raise-argument-error 'make-readtable "(or/c readtable? #f)" target))
        (define actual-target (or (and target (hash-ref (readtable-char-ht target) mode #f))
                                  mode))
        (define new-char-ht (if actual-target
                                (hash-set char-ht key actual-target)
                                (hash-remove char-ht key)))
        (define new-delimiter-ht (hash-set delimiter-ht
                                           key
                                           (if target
                                               (hash-ref (readtable-delimiter-ht target) mode mode)
                                               mode)))
        (loop rest-args symbol-parser new-char-ht dispatch-ht new-delimiter-ht)]
       [else
        ;; Update main character table with a new handler
        (unless (and (procedure? target) (procedure-arity-includes? target 6))
          (raise-argument-error 'make-readtable "(procedure-arity-includes/c 6)" target))
        (define new-char-ht (hash-set char-ht key target))
        (define new-delimiter-ht (hash-set delimiter-ht key (if (eq? mode 'terminating-macro)
                                                                'delimit
                                                                'no-delimit)))
        (loop rest-args symbol-parser new-char-ht dispatch-ht new-delimiter-ht)])])))

;; Map a character to another character (if any) whose default
;; treatment should be used; be sure to map non-characters like
;; EOF to themselves.
(define-inline (readtable-effective-char rt c)
  (cond
   [(or (not rt) (not (char? c))) c]
   [else (*readtable-effective-char rt c)]))

(define (*readtable-effective-char rt c)
  (define target (hash-ref (readtable-char-ht rt) c #f))
  (cond
   [(not target) c]
   [(char? target) target]
   [else #\x])) ; return some non-special character

(define (effective-char c config)
  (readtable-effective-char (read-config-readtable config) c))

;; Map a character to a handler, if any:
(define (readtable-handler config c)
  (define rt (read-config-readtable config))
  (and rt
       (let ([target (hash-ref (readtable-char-ht rt) c #f)])
         (and target
              (not (char? target))
              target))))

;; Map a character after `#` to a handler, if any:
(define (readtable-dispatch-handler config c)
  (force-parameters! config)
  (define rt (read-config-readtable config))
  (and rt
       (hash-ref (readtable-dispatch-ht rt) c #f)))

(define (readtable-apply handler c in config line col pos)
  (define for-syntax? (read-config-for-syntax? config))
  (define v
    (cond
     [(not for-syntax?)
      (parameterize-like
       #:with ([current-read-config config])
       (if (procedure-arity-includes? handler 2)
           (handler c in)
           (handler c in #f line col pos)))]
     [else
      (parameterize-like
       #:with ([current-read-config config])
       (handler c in (read-config-source config) line col pos))]))
  (if (special-comment? v)
      v
      (coerce v in config)))

;; Part of the public API:
(define (readtable-mapping rt c)
  (unless (readtable? rt)
    (raise-argument-error 'readtable-mapping "readtable?" rt))
  (unless (char? c)
    (raise-argument-error 'readtable-mapping "char?" c))
  (define handler (hash-ref (readtable-char-ht rt) c #f))
  (values (or (and handler
                   (cond
                    [(char? handler) handler]
                    [(eq? 'delimit (hash-ref (readtable-delimiter-ht rt) c #f))
                     'terminating-macro]
                    [else
                     'non-terminating-macro]))
              c)
          (if (char? handler) #f handler)
          (hash-ref (readtable-dispatch-ht rt) c #f)))

;; Return a list of characters mapped to `c`:
(define (readtable-equivalent-chars rt c)
  (define ht (readtable-char-ht rt))
  (append
   (if (hash-ref ht c #f)
       null
       (list c))
   (for/list ([(k v) (in-hash ht)]
              #:when (eqv? v c))
     k)))
