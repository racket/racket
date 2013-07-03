#lang scheme/base

(require mzlib/list
         mzlib/contract
         "my-macros.rkt"
         "shared.rkt"
         "syntax-property.rkt"
         #;(file "/Users/clements/clements/scheme-scraps/eli-debug.ss"))

(define-struct full-mark-struct (source label bindings values))

; CONTRACTS
(define mark? (-> ; no args  
               full-mark-struct?))
(define mark-list? (listof procedure?))

(provide/contract 
 ;[make-debug-info (any/c binding-set? varref-set? any/c boolean? . -> . syntax?)] ; (location tail-bound free label lifting? -> mark-stx)
 [expose-mark (-> mark? (list/c any/c symbol? (listof (list/c identifier? any/c))))]
 [make-top-level-mark (syntax? . -> . syntax?)]
 [lookup-all-bindings ((identifier? . -> . boolean?) mark-list? . -> . (listof any/c))]
 [lookup-first-binding ((identifier? . -> . boolean?) mark-list? ( -> any) . -> . any/c)]
 [lookup-binding (mark-list? identifier? . -> . any)])

(provide
 make-debug-info
 wcm-wrap
 skipto-mark?
 skipto-mark
 strip-skiptos
 mark-list?
 mark-source
 mark-bindings
 mark-label
 mark-binding-value
 mark-binding-binding
 display-mark
 all-bindings
 #;lookup-binding-list
 debug-key
 extract-mark-list
 (struct-out normal-breakpoint-info)
 (struct-out error-breakpoint-info)
 (struct-out breakpoint-halt)
 (struct-out expression-finished))

; BREAKPOINT STRUCTURES

(define-struct normal-breakpoint-info (mark-list kind))
(define-struct error-breakpoint-info (message))
(define-struct breakpoint-halt ())
(define-struct expression-finished (returned-value-list))

(define-struct skipto-mark-struct ())
(define skipto-mark? skipto-mark-struct?)
(define skipto-mark (make-skipto-mark-struct))
(define (strip-skiptos mark-list)
  (filter (lx (not (skipto-mark? _))) mark-list))


; debug-key: this key will be used as a key for the continuation marks.
(define-struct debug-key-struct ())
(define debug-key (make-debug-key-struct))

(define (extract-mark-list mark-set)
  (strip-skiptos (continuation-mark-set->list mark-set debug-key)))


; the 'varargs' creator is used to avoid an extra cons cell in every mark:
(define (make-make-full-mark-varargs source label bindings)
  (lambda values 
    (make-full-mark-struct source label bindings values)))

; see module top for type
(define (make-full-mark location label bindings)
  (datum->syntax #'here `(#%plain-lambda () (#%plain-app ,(make-make-full-mark-varargs location label bindings)
                                                         ,@(map make-mark-binding-stx bindings)))))

(define (mark-source mark)
  (full-mark-struct-source (mark)))

(define (make-mark-binding-stx id)
  #`(#%plain-lambda () #,id))

(define (mark-bindings mark)
  (map list 
       (full-mark-struct-bindings (mark)) 
       (full-mark-struct-values (mark))))

(define (mark-label mark)
  (full-mark-struct-label (mark)))

(define (mark-binding-value mark-binding)
  (with-handlers ([(λ (e) #t) (λ (e) "--- undefined ---")])
    ((cadr mark-binding))))

(define (mark-binding-binding mark-binding)
  (car mark-binding))

(define (expose-mark mark)
  (let ([source (mark-source mark)]
        [label (mark-label mark)]
        [bindings (mark-bindings mark)])
    (list source
          label
          (map (lambda (binding)
                 (list (mark-binding-binding binding)
                       (mark-binding-value binding)))
               bindings))))

(define (display-mark mark)
  (apply
   string-append
   (format "source: ~a\n" (syntax->datum (mark-source mark)))
   (format "label: ~a\n" (mark-label mark))
   (format "bindings:\n")
   (map (lambda (binding)
          (format " ~a : ~a\n" (syntax-e (mark-binding-binding binding))
                  (mark-binding-value binding)))
        (mark-bindings mark))))


; possible optimization: rig the mark-maker to guarantee statically that a
; variable can occur at most once in a mark.  

(define (binding-matches matcher mark)
  (filter (lambda (binding-pair) (matcher (mark-binding-binding binding-pair))) (mark-bindings mark)))

(define (lookup-all-bindings matcher mark-list)
  (apply append (map (lambda (m) (binding-matches matcher m)) mark-list)))

(define (lookup-first-binding matcher mark-list fail-thunk)
  (let ([all-bindings (lookup-all-bindings matcher mark-list)])
    (if (null? all-bindings)
        (fail-thunk)
        (car all-bindings))))

(define (lookup-binding mark-list id)
  (mark-binding-value
   (lookup-first-binding (lambda (id2) (free-identifier=? id id2)) 
                         mark-list 
                         (lambda ()
                           (error 'lookup-binding "variable not found in environment: ~a\n" (if (syntax? id) 
                                                                                                (syntax->datum id)
                                                                                                id))))))

(define (all-bindings mark)
  (map mark-binding-binding (mark-bindings mark)))

(define (wcm-wrap debug-info expr)
  #`(with-continuation-mark #,debug-key #,debug-info #,expr))


; DEBUG-INFO STRUCTURES

;;;;;;;;;;
;;
;; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
;; a source expression and a set of binding/value pairs.
;; (syntax-object BINDING-SET VARREF-SET any boolean (union/c false/c integer?)) -> debug-info)
;;
;;;;;;;;;;

(define (make-debug-info source tail-bound free-vars label lifting?)
  (define kept-vars (binding-set-varref-set-intersect tail-bound free-vars))
  (define (let-binding? var)
    (and 
     (not (stepper-syntax-property var 'stepper-no-lifting-info))
     (case (stepper-syntax-property var 'stepper-binding-type)
       ((let-bound macro-bound) #t)
       ((lambda-bound stepper-temp non-lexical) #f)
       (else (error 'make-debug-info 
                    "varref ~a's binding-type info was not recognized: ~a"
                    (syntax-e var)
                    (stepper-syntax-property var 'stepper-binding-type))))))
  (cond [lifting?
         (define let-bindings (filter let-binding? kept-vars))
         (define lifter-syms (map get-lifted-var let-bindings))
         (make-full-mark source label (append kept-vars lifter-syms))]
        [else
         ;; I'm not certain that non-lifting is currently tested: 2005-12, JBC
         (make-full-mark source label kept-vars)]))


(define (make-top-level-mark source-expr)
  (make-full-mark source-expr 'top-level null))
