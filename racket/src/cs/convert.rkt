#lang racket/base
(require racket/cmdline
         racket/pretty
         racket/match
         racket/file
         racket/extflonum
         racket/include
         "../schemify/schemify.rkt"
         "../schemify/serialize.rkt"
         "../schemify/known.rkt"
         "../schemify/lift.rkt")

(define skip-export? #f)
(define for-cify? #f)
(define unsafe-mode? #f)

(define-values (in-file out-file)
  (command-line
   #:once-each
   [("--skip-export") "Don't generate an `export` form"
    (set! skip-export? #t)]
   [("--for-cify") "Keep `make-struct-type` as-is, etc."
    (set! for-cify? #t)]
   [("--unsafe") "Compile for unsafe mode"
    (set! unsafe-mode? #t)]
   #:args
   (in-file out-file)
   (values in-file out-file)))

(define content (call-with-input-file* in-file read))
(define l (cdddr content))

(let loop ([l l])
  (cond
    [(eq? l 'make-optional-keyword-procedure)
     (error "keyword residual `make-optional-keyword-procedure` appears in .rktl")]
    [(pair? l)
     (loop (car l))
     (loop (cdr l))]))

(define lifts (make-hash))
(define ordered-lifts null)

(define (lift-set! k v)
  (unless (hash-ref lifts k #f)
    (hash-set! lifts k v)
    (set! ordered-lifts (cons k ordered-lifts))))

;; Ad hoc patterns to deal with a special case in "expander.rktl":
(define (quote? v)
  (and (pair? v)
       (eq? (car v) 'quote)
       (pair? (cdr v))
       (null? (cddr v))))
(define (nested-hash? v)
  (and (pair? v)
       (eq? #f (car v))
       (hash? (cdr v))))
(define (list-of-keywords? v)
  (and (pair? v)
       (list? v)
       (andmap keyword? v)))

;; Gather all literal regexps and hash tables
(define (lift v)
  (cond
   [(or (regexp? v) (byte-regexp? v))
    (define s (gensym 'rx))
    (lift-set! v s)]
   [(or (pregexp? v) (byte-pregexp? v))
    (define s (gensym 'px))
    (lift-set! v s)]
   [(hash? v)
    (define s (gensym 'hash))
    (lift-set! v s)]
   [(and (quote? v)
         (nested-hash? (cadr v)))
    (define s (gensym 'nhash))
    (lift-set! (cadr v) s)]
   [(keyword? v)
    (define s (gensym 'kw))
    (lift-set! v s)]
   [(and (quote? v)
         (list-of-keywords? (cadr v)))
    (define s (gensym 'kws))
    (lift-set! (cadr v) s)]
   [(and (quote? v)
         (extflonum? (cadr v)))
    (define s (gensym 'extfl))
    (lift-set! (cadr v) s)]
   [(pair? v)
    (lift (car v))
    (lift (cdr v))]))

(unless for-cify?
  (lift l))

(define prim-knowns
  (let ([knowns (hasheq)])
    (define-syntax-rule (define-primitive-table id [prim known] ...)
      (begin (set! knowns (hash-set knowns 'prim known)) ...))
    (include "primitive/kernel.ss")
    (include "primitive/unsafe.ss")
    (include "primitive/flfxnum.ss")
    (include "primitive/paramz.ss")
    (include "primitive/extfl.ss")
    (include "primitive/network.ss")
    (include "primitive/futures.ss")
    (include "primitive/place.ss")
    (include "primitive/foreign.ss")
    (include "primitive/linklet.ss")
    (include "primitive/internal.ss")
    knowns))

;; Convert:
(define schemified-body
  (let ()
    (define-values (bodys/constants-lifted lifted-constants)
      (if for-cify?
          (begin
            (printf "Serializable...\n")
            (time (convert-for-serialize l for-cify?)))
          (values l null)))
    (printf "Schemify...\n")
    (define body
      (time
       (schemify-body bodys/constants-lifted prim-knowns #hasheq() #hasheq() for-cify? unsafe-mode? #t)))
    (printf "Lift...\n")
    ;; Lift functions to aviod closure creation:
    (define lifted-body
      (time
       (lift-in-schemified-body body)))
    (append (for/list ([p (in-list lifted-constants)])
              (cons 'define p))
            lifted-body)))

;; ----------------------------------------

(unless for-cify?
  
  ;; Set a hook to redirect literal regexps and
  ;; hash tables to lifted bindings
  (pretty-print-size-hook
   (lambda (v display? out)
     (cond
       [(and (pair? v)
             (pair? (cdr v))
             (eq? 'quote (car v))
             (or (regexp? (cadr v))
                 (byte-regexp? (cadr v))
                 (pregexp? (cadr v))
                 (byte-pregexp? (cadr v))
                 (hash? (cadr v))
                 (nested-hash? (cadr v))
                 (keyword? (cadr v))
                 (list-of-keywords? (cadr v))
                 (extflonum? (cadr v))))
        10]
       [(bytes? v) (* 3 (bytes-length v))]
       [(and (symbol? v) (regexp-match? #rx"#" (symbol->string v)))
        (+ 2 (string-length (symbol->string v)))]
       [(char? v) 5]
       [(single-flonum? v) 5]
       [(or (keyword? v)
            (regexp? v)
            (pregexp? v)
            (hash? v))
        (error 'lift "value that needs lifting is in an unrecognized context: ~v" v)]
       [else #f])))

  ;; This hook goes with `pretty-print-size-hook`
  (pretty-print-print-hook
   (lambda (v display? out)
     (cond
       [(and (pair? v)
             (eq? 'quote (car v))
             (or (regexp? (cadr v))
                 (byte-regexp? (cadr v))
                 (pregexp? (cadr v))
                 (byte-pregexp? (cadr v))
                 (hash? (cadr v))
                 (nested-hash? (cadr v))
                 (keyword? (cadr v))
                 (list-of-keywords? (cadr v))
                 (extflonum? (cadr v))))
        (write (hash-ref lifts (cadr v)) out)]
       [(bytes? v)
        (display "#vu8")
        (write (bytes->list v) out)]
       [(symbol? v)
        (write-string (format "|~a|" v) out)]
       [(char? v)
        (write-string (format "#\\x~x" (char->integer v)) out)]
       [(single-flonum? v)
        (write (real->double-flonum v) out)]
       [else #f]))))

;; ----------------------------------------

;; Startup code as an S-expression uses the pattern
;;   (lambda <formals> (begin '<id> <expr>))
;; or
;;   (case-lambda [<formals> (begin '<id> <expr>)] <clause> ...)
;; to record a name for a function. Detect that pattern and
;; create a `#%name` form. We rely on the fact
;; that the names `lambda`, `case-lambda`, and `quote` are
;; never shadowed, so we don't have to parse expression forms
;; in general.
(define (rename-functions e)
  (cond
    [(not (pair? e)) e]
    [else
     (define (begin-name e)
       (and (pair? e)
            (eq? (car e) 'begin)
            (pair? (cdr e))
            (pair? (cddr e))
            (pair? (cadr e))
            (eq? 'quote (caadr e))
            (cadadr e)))
     (case (car e)
       [(quote) e]
       [(lambda)
        (define new-e (map rename-functions e))
        (define name (begin-name (caddr e)))
        (if name
            `(#%name ,name ,new-e)
            new-e)]
       [(case-lambda)
        (define new-e (map rename-functions e))
        (define name (and (pair? (cdr e))
                          (begin-name (cadadr e))))
        (if name
            `(#%name ,name ,new-e)
            new-e)]
       [else (cons (rename-functions (car e))
                   (rename-functions (cdr e)))])]))

;; ----------------------------------------

(make-parent-directory* out-file)

(with-handlers ([void (lambda (exn)
                        (when (file-exists? out-file)
                          (with-handlers ([void (lambda (exn)
                                                  (log-error "delete failed: ~s" exn))])
                            (delete-file out-file)))
                        (raise exn))])
  (with-output-to-file
   out-file
   #:exists 'truncate
   (lambda ()
     (unless skip-export?
       ;; Write out exports
       (pretty-write
        `(export (rename ,@(caddr content)))))
     ;; Write out lifted regexp and hash-table literals
     (for ([k (in-list (reverse ordered-lifts))])
       (define v (hash-ref lifts k))
       (pretty-write
        `(define ,v
          ,(let loop ([k k])
             (cond
              [(or (regexp? k)
                   (byte-regexp? k))
               `(,(cond [(byte-regexp? k)  'byte-regexp]
                        [(byte-pregexp? k) 'byte-pregexp]
                        [(pregexp? k)      'pregexp]
                        [else              'regexp])
                 ,(object-name k))]
              [(hash? k)
               `(,(cond
                   [(hash-equal? k) 'hash]
                   [(hash-eqv? k) 'hasheqv]
                   [else 'hasheq])
                 ,@(for*/list ([(k v) (in-hash k)]
                               [e (in-list (list k v))])
                     `(quote ,e)))]
              [(pair? k)
               `(cons ,(loop (car k)) ,(loop (cdr k)))]
              [(keyword? k)
               `(string->keyword ,(keyword->string k))]
              [(null? k) ''()]
              [(extflonum? k) `(string->number ,(format "~a" k) 10 'read)]
              [else k])))))

     ;; Write out converted forms
     (for ([v (in-list schemified-body)])
       (unless (equal? v '(void))
         (let loop ([v v])
           (match v
             [`(begin ,vs ...)
              (for-each loop vs)]
             [else
              (pretty-write (rename-functions v))])))))))
