#lang racket/base

(provide mangle-id mangle-id-for-maker
         build-struct-names
         lookup-struct-info
         nums-up-to
         add-name-prop
         all-but-last
         known-good-contract?
         update-loc)

(require setup/main-collects
         racket/struct-info
         (for-template racket/base))

(define (update-loc stx loc)
  (datum->syntax stx (syntax-e stx) loc))

;; lookup-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
(define (lookup-struct-info stx provide-stx)
  (let ([id (syntax-case stx ()
              [(a b) (syntax a)]
              [_ stx])])
    (let ([v (syntax-local-value id (λ () #f))])
      (if (struct-info? v)
          (extract-struct-info v)
          (raise-syntax-error 'provide/contract
                              "expected a struct name" 
                              provide-stx
                              id)))))


(define (add-name-prop name stx)
  (cond
    [(identifier? name)
     (syntax-property stx 'inferred-name (syntax-e name))]
    [(symbol? name)
     (syntax-property stx 'inferred-name name)]
    [else stx]))

;; mangle-id : syntax string syntax ... -> syntax
;; constructs a mangled name of an identifier from an identifier
;; the name isn't fresh, so `id' combined with `ids' must already be unique.
(define (mangle-id main-stx prefix id . ids)
  (datum->syntax
   #f
   (string->symbol
    (string-append
     prefix
     (format 
      "-~a~a"
      (syntax->datum id)
      (apply 
       string-append 
       (map 
        (lambda (id)
          (format "-~a" (syntax->datum id)))
        ids)))))))

(define (mangle-id-for-maker main-stx prefix id . ids)
  (let ([id-w/out-make (regexp-replace #rx"^make-" (format "~a" (syntax->datum id)) "")])
    (datum->syntax
     #f
     (string->symbol
      (string-append
       "make-"
       prefix
       (format 
        "-~a~a"
        id-w/out-make
        (apply 
         string-append 
         (map 
          (lambda (id)
            (format "-~a" (syntax->datum id)))
          ids))))))))

;; (cons X (listof X)) -> (listof X)
;; returns the elements of `l', minus the last element
;; special case: if l is an improper list, it leaves off
;; the contents of the last cdr (ie, making a proper list
;; out of the input), so (all-but-last '(1 2 . 3)) = '(1 2)
(define (all-but-last l)
  (cond
    [(null? l) (error 'all-but-last "bad input")]
    [(not (pair? l)) '()]
    [(null? (cdr l)) null]
    [(pair? (cdr l)) (cons (car l) (all-but-last (cdr l)))]
    [else (list (car l))]))

;; helper for build-src-loc-string
(define (source->name src)
  (let* ([bs (cond [(bytes? src)  src]
                   [(path? src)   (path->bytes src)]
                   [(string? src) (string->bytes/locale src)]
                   [else #f])]
         [r (and bs (path->main-collects-relative bs))])
    (and bs
         (bytes->string/locale (if (and (pair? r) (eq? 'collects (car r)))
                                   (apply bytes-append 
                                          #"<collects>" 
                                          (map (lambda (s)
                                                 (bytes-append #"/" s))
                                               (cdr r)))
                                   bs)))))

;; build-src-loc-string : (or/c srcloc syntax) -> (union #f string)
(define (build-src-loc-string stx)
  (let-values ([(source line col pos)
                (if (syntax? stx)
                    (values (source->name (syntax-source stx))
                            (syntax-line stx)
                            (syntax-column stx)
                            (syntax-position stx))
                    (if (syntax? (srcloc-source stx))
                      (values (source->name 
                               (resolved-module-path-name 
                                (module-path-index-resolve 
                                 (syntax-source-module
                                  (srcloc-source stx)))))
                              (srcloc-line stx)
                              (srcloc-column stx)
                              (srcloc-position stx))
                      (error 'contract
                             "malformed srcloc has non-syntax source: ~e"
                             stx)))])
    (let ([location (cond [(and line col) (format "~a:~a" line col)]
                          [pos (format "~a" pos)]
                          [else #f])])
      (if (and source location)
          (string-append source ":" location)
          (or location source)))))

(define build-struct-names
  (lambda (name-stx fields omit-sel? omit-set? srcloc-stx)
    (let ([name (symbol->string (syntax-e name-stx))]
          [fields (map symbol->string (map syntax-e fields))]
          [+ string-append])
      (map (lambda (s)
             (datum->syntax name-stx (string->symbol s) srcloc-stx))
           (append
            (list 
             (+ "struct:" name)
             (+ "make-" name)
             (+ name "?"))
            (let loop ([l fields])
              (if (null? l)
                  null
                  (append
                   (if omit-sel?
                       null
                       (list (+ name "-" (car l))))
                   (if omit-set?
                       null
                       (list (+ "set-" name "-" (car l) "!")))
                   (loop (cdr l))))))))))

(define (nums-up-to n)
  (let loop ([i 0])
    (cond
      [(= i n) '()]
      [else (cons i (loop (+ i 1)))])))

#|

;; the code below builds the known-good-syms-ht

(define cm
  (parameterize ([read-accept-compiled #t])
    (call-with-input-file
        "C:\\Users\\robby\\git\\plt\\collects\\racket\\compiled\\base_rkt.zo" read)))

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'racket/base))

(define-values (vars stx) (module-compiled-exports cm))
(define known-good-syms 
  (filter (λ (x) (and (regexp-match #rx"[?]$" (symbol->string x))
                      (procedure-arity-includes? (eval x ns) 1)))
          (map car (cdr (assoc 0 vars)))))
(define table-to-be-turned-into-a-literal-hash
  (map (λ (x) (cons x #t))
       (sort known-good-syms
             string<=?
             #:key symbol->string)))

|#

(define known-good-syms-ht 
  '#hash((absolute-path? . #t)
         (arity-at-least? . #t)
         (boolean? . #t)
         (box? . #t)
         (byte-pregexp? . #t)
         (byte-ready? . #t)
         (byte-regexp? . #t)
         (byte? . #t)
         (bytes-converter? . #t)
         (bytes? . #t)
         (channel? . #t)
         (chaperone? . #t)
         (char-alphabetic? . #t)
         (char-blank? . #t)
         (char-graphic? . #t)
         (char-iso-control? . #t)
         (char-lower-case? . #t)
         (char-numeric? . #t)
         (char-punctuation? . #t)
         (char-ready? . #t)
         (char-symbolic? . #t)
         (char-title-case? . #t)
         (char-upper-case? . #t)
         (char-whitespace? . #t)
         (char? . #t)
         (compiled-expression? . #t)
         (compiled-module-expression? . #t)
         (complete-path? . #t)
         (complex? . #t)
         (continuation-mark-set? . #t)
         (continuation-prompt-available? . #t)
         (continuation-prompt-tag? . #t)
         (continuation? . #t)
         (custodian-box? . #t)
         (custodian? . #t)
         (custom-print-quotable? . #t)
         (custom-write? . #t)
         (date*? . #t)
         (date-dst? . #t)
         (date? . #t)
         (directory-exists? . #t)
         (double-flonum? . #t)
         (eof-object? . #t)
         (ephemeron? . #t)
         (even? . #t)
         (evt? . #t)
         (exact-integer? . #t)
         (exact-nonnegative-integer? . #t)
         (exact-positive-integer? . #t)
         (exact? . #t)
         (exn:break? . #t)
         (exn:fail:contract:arity? . #t)
         (exn:fail:contract:continuation? . #t)
         (exn:fail:contract:divide-by-zero? . #t)
         (exn:fail:contract:non-fixnum-result? . #t)
         (exn:fail:contract:variable? . #t)
         (exn:fail:contract? . #t)
         (exn:fail:filesystem:exists? . #t)
         (exn:fail:filesystem:version? . #t)
         (exn:fail:filesystem? . #t)
         (exn:fail:network? . #t)
         (exn:fail:out-of-memory? . #t)
         (exn:fail:read:eof? . #t)
         (exn:fail:read:non-char? . #t)
         (exn:fail:read? . #t)
         (exn:fail:syntax:unbound? . #t)
         (exn:fail:syntax? . #t)
         (exn:fail:unsupported? . #t)
         (exn:fail:user? . #t)
         (exn:fail? . #t)
         (exn:srclocs? . #t)
         (exn? . #t)
         (file-exists? . #t)
         (file-stream-port? . #t)
         (fixnum? . #t)
         (flonum? . #t)
         (handle-evt? . #t)
         (hash-eq? . #t)
         (hash-equal? . #t)
         (hash-eqv? . #t)
         (hash-placeholder? . #t)
         (hash-weak? . #t)
         (hash? . #t)
         (identifier? . #t)
         (immutable? . #t)
         (impersonator-property-accessor-procedure? . #t)
         (impersonator-property? . #t)
         (impersonator? . #t)
         (inexact-real? . #t)
         (inexact? . #t)
         (input-port? . #t)
         (inspector? . #t)
         (integer? . #t)
         (internal-definition-context? . #t)
         (keyword? . #t)
         (liberal-define-context? . #t)
         (link-exists? . #t)
         (list? . #t)
         (log-receiver? . #t)
         (logger? . #t)
         (module-path-index? . #t)
         (module-path? . #t)
         (mpair? . #t)
         (namespace-anchor? . #t)
         (namespace? . #t)
         (negative? . #t)
         (null? . #t)
         (number? . #t)
         (odd? . #t)
         (output-port? . #t)
         (pair? . #t)
         (parameter? . #t)
         (parameterization? . #t)
         (path-for-some-system? . #t)
         (path-string? . #t)
         (path? . #t)
         (placeholder? . #t)
         (port-closed? . #t)
         (port-provides-progress-evts? . #t)
         (port-writes-atomic? . #t)
         (port-writes-special? . #t)
         (port? . #t)
         (positive? . #t)
         (pregexp? . #t)
         (primitive-closure? . #t)
         (primitive? . #t)
         (procedure-arity? . #t)
         (procedure-struct-type? . #t)
         (procedure? . #t)
         (pseudo-random-generator? . #t)
         (rational? . #t)
         (readtable? . #t)
         (real? . #t)
         (regexp? . #t)
         (relative-path? . #t)
         (rename-transformer? . #t)
         (resolved-module-path? . #t)
         (security-guard? . #t)
         (semaphore-try-wait? . #t)
         (semaphore? . #t)
         (sequence? . #t)
         (set!-transformer? . #t)
         (single-flonum? . #t)
         (special-comment? . #t)
         (srcloc? . #t)
         (string? . #t)
         (struct-accessor-procedure? . #t)
         (struct-constructor-procedure? . #t)
         (struct-mutator-procedure? . #t)
         (struct-predicate-procedure? . #t)
         (struct-type-property-accessor-procedure? . #t)
         (struct-type-property? . #t)
         (struct-type? . #t)
         (struct? . #t)
         (subprocess? . #t)
         (symbol-interned? . #t)
         (symbol-unreadable? . #t)
         (symbol? . #t)
         (syntax-original? . #t)
         (syntax-tainted? . #t)
         (syntax? . #t)
         (terminal-port? . #t)
         (thread-cell? . #t)
         (thread-dead? . #t)
         (thread-group? . #t)
         (thread-running? . #t)
         (thread? . #t)
         (variable-reference-constant? . #t)
         (variable-reference? . #t)
         (vector? . #t)
         (void? . #t)
         (weak-box? . #t)
         (will-executor? . #t)
         (zero? . #t)))

(define (known-good-contract? id)
  (define r-id (syntax-e id))
  (and (symbol? r-id)
       (hash-ref known-good-syms-ht (syntax-e id) #t)
       (free-identifier=? id (datum->syntax #'here r-id))))
