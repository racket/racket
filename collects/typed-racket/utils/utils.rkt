#lang racket/base

#|
This file is for utilities that are of general interest,
at least theoretically.
|#

(require (for-syntax racket/base syntax/parse racket/string)
         racket/require-syntax racket/provide-syntax         
         racket/struct-info "timing.rkt")

;; to move to unstable
(provide list-update list-set)

(provide
 ;; optimization
 optimize?
 ;; timing
 start-timing do-time
 ;; logging
 printf/log show-input?
 ;; struct printing
 custom-printer define-struct/printer
 ;; provide macros
 rep utils typecheck infer env private types)

(define optimize? (make-parameter #t))
(define-for-syntax enable-contracts? #f)

(define-syntax do-contract-req
  (if enable-contracts?
      (lambda (stx) (datum->syntax stx '(require racket/contract/base)))
      (syntax-rules () [(_) (begin)])))
(do-contract-req)

(define show-input? (make-parameter #f))

;; fancy require syntax
(define-syntax (define-requirer stx)
  (syntax-parse stx
    [(_ nm:id nm-out:id)
     #`(...
        (begin
          (define-require-syntax (nm stx)
            (syntax-parse stx
              [(form id:identifier ...)
               (with-syntax ([(id* ...)
                              (map (lambda (id)
                                     (datum->syntax
                                      id
                                      `(lib
					,(datum->syntax
					  #f
					  (string-join
					   (list "typed-racket"
						 (symbol->string (syntax-e #'nm))
						 (string-append (symbol->string (syntax-e id)) ".rkt"))
					   "/")
					  id id))
                                      id id))
                                   (syntax->list #'(id ...)))])
                 (syntax-property (syntax/loc stx (combine-in id* ...))
                                  'disappeared-use
                                  #'form))]))
          (define-provide-syntax (nm-out stx)
            (syntax-parse stx
              [(_ id:identifier ...)
               (with-syntax ([(id* ...)
                              (map (lambda (id)
                                     (datum->syntax
                                      id
                                      `(lib
					,(datum->syntax
					  #f
					  (string-join
					   (list "typed-racket"
						 (symbol->string (syntax-e #'nm))
						 (string-append (symbol->string (syntax-e id)) ".rkt"))
					   "/")
					  id id))))
                                   (syntax->list #'(id ...)))])
                 (syntax/loc stx (combine-out (all-from-out id*) ...)))]))
          (provide nm nm-out)))]))


(define-requirer rep rep-out)
(define-requirer infer infer-out)
(define-requirer typecheck typecheck-out)
(define-requirer utils utils-out)
(define-requirer env env-out)
(define-requirer private private-out)
(define-requirer types types-out)
(define-requirer optimizer optimizer-out)
(define-requirer base-env base-env-out)

;; conditionalized logging
;; there's some logging code in the source
;; which was used for gathering statistics about various programs
;; no longer used, probably bitrotted
(define-for-syntax logging? #f)

(define-syntax (printf/log stx)
  (if logging?
      (syntax-case stx ()
        [(_ fmt . args)
	 #'(log-debug (format fmt . args))])
      #'(void)))

;; custom printing
;; this requires lots of work for two reasons:
;; - 1 printers have to be defined at the same time as the structs
;; - 2 we want to support things printing corectly even when the custom printer is off

(define-syntax-rule (defprinter t ...)
  (begin
    (define t (box (lambda _ (error (format "~a not yet defined" 't))))) ...
    (provide t ...)))

(defprinter
  print-type* print-filter* print-latentfilter* print-object* print-latentobject*
  print-pathelem*)

(define custom-printer (make-parameter #t))

(define-syntax (define-struct/printer stx)
  (syntax-parse stx
    [(form name (flds ...) printer:expr)
     #`(define-struct name (flds ...)
         #:property prop:custom-print-quotable 'never
         #:property prop:custom-write
         (lambda (a b c) (if (custom-printer)
                             (printer a b c)
                             ;; ok to make this case slow, it never runs in real code
                             ((if c
                                  (dynamic-require 'racket/pretty 'pretty-write)
                                  (dynamic-require 'racket/pretty 'pretty-print))
                              a b)))
         #:transparent)]))


;; turn contracts on and off - off by default for performance.
(provide (for-syntax enable-contracts?)
         provide/cond-contract
         with-cond-contract         
         define-struct/cond-contract
         define/cond-contract
         contract-req
         define/cond-contract/provide)

(define-require-syntax contract-req
  (if enable-contracts?
      (lambda (stx) (datum->syntax stx 'racket/contract))
      (syntax-rules ()
        [(_) (combine-in)])))

(define-syntax-rule (define/cond-contract/provide (name . args) c . body)
  (begin (define/cond-contract name c
           (begin
            (define (name . args) body)
            name))
         (provide/cond-contract [name c])))

;; these are versions of the contract forms conditionalized by `enable-contracts?'
(define-syntax provide/cond-contract
  (if enable-contracts?
      (make-rename-transformer #'provide/contract)
      (lambda (stx)
        (define-syntax-class clause
          #:literals ()
          #:attributes (i)
          (pattern [(~datum struct) (~or nm:id (nm:id super:id)) (flds ...)]
		   #:with i #'(struct-out nm))
	  (pattern [(~datum rename) out:id in:id cnt:expr]
                   #:with i #'(rename-out [out in]))
          (pattern [i:id cnt:expr]))
        (syntax-parse stx
          [(_ c:clause ...)
           #'(provide c.i ...)]))))

(define-syntax with-cond-contract
  (if enable-contracts?
      (make-rename-transformer #'with-contract)
      (lambda (stx)
        (syntax-parse stx
          [(_ name (~or #:results #:result) spec . body)
           #'(let () . body)]
          [(_ name specs . body)
           #'(begin . body)]))))

(define-syntax define/cond-contract
  (if enable-contracts?
      (make-rename-transformer #'define/contract)
      (lambda (stx)
        (syntax-parse stx
          [(_ head cnt . body)
           #'(define head . body)]))))

(define-syntax define-struct/cond-contract
  (if enable-contracts?
      (make-rename-transformer #'define-struct/contract)
      (syntax-rules ()
        [(_ hd ([i c] ...) . opts)
         (define-struct hd (i ...) . opts)])))

(define (list-update l i f)
  (cond [(null? l) (error 'list-update "list not long enough" l i f)]
        [(zero? i) (cons (f (car l)) (cdr l))]
        [else (cons (car l) (list-update (cdr l) (sub1 i) f))]))

(define (list-set l k v)
  (if (zero? k)
      (cons v (cdr l))
      (cons (car l) (list-set (cdr l) (sub1 k) v))))


(provide make-struct-info-self-ctor)
;Copied from racket/private/define-struct
;FIXME when multiple bindings are supported
(define (self-ctor-transformer orig stx)
  (define (transfer-srcloc orig stx)
    (datum->syntax orig (syntax-e orig) stx orig))
  (syntax-case stx ()
    [(self arg ...) (datum->syntax stx
                                   (cons (syntax-property (transfer-srcloc orig #'self)
                                                          'constructor-for
                                                          (syntax-local-introduce #'self))
                                         (syntax-e (syntax (arg ...))))
                                   stx
                                   stx)]
    [_ (transfer-srcloc orig stx)]))


(define make-struct-info-self-ctor
 (let ()
  (struct struct-info-self-ctor (id info)
          #:property prop:procedure
                     (lambda (ins stx)
                      (self-ctor-transformer (struct-info-self-ctor-id ins) stx))
          #:property prop:struct-info (lambda (x) (extract-struct-info (struct-info-self-ctor-info x))))
  struct-info-self-ctor))
