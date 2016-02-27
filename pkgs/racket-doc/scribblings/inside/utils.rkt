#lang scheme/base

(require scribble/manual
         scribble/struct
         scribble/decode
         scribble/scheme
         (for-syntax scheme/base)
         (for-label scheme/base))

(provide Racket
         mzc cpp cppi cppdef (rename-out [*var var])
         function subfunction
         FormatD
         tech-place
         reference-doc raco-doc
         (except-out (all-from-out scribble/manual) var)
         (for-label (all-from-out scheme/base)))

(define (as-cpp-defn name s)
  (make-target-element #f
                       (list (as-index s))
                       `(cpp ,(format "~a" name))))

(define-syntax (function stx)
  (syntax-case stx ()
    [(_ (ret name [type arg] ...) . body)
     #'(*function (cpp/sym 'ret)
                  (as-cpp-defn 'name (cpp/sym 'name))
                  (list (type/sym 'type) ...)
                  (list (var/sym 'arg) ...)
                  (lambda ()
                    (list . body)))]))

(define-syntax (subfunction stx)
  (syntax-case stx ()
    [(_ (ret name [type arg] ...) . body)
     #'(make-blockquote
        "leftindent"
        (flow-paragraphs
         (decode-flow
          (list
           (*function (cpp/sym 'ret)
                      (var/sym 'name)
                      (list (type/sym 'type) ...)
                      (list (var/sym 'arg) ...)
                      (lambda ()
                        (list . body)))))))]))

(define (to-flow elem)
  (make-flow (list (make-paragraph (list elem)))))

(define (*function ret name types args rest-thunk)
  (let ([spacer (hspace 1)]
        [pair-type (lambda (t v)
                     (if (equal? "..." (element->string t))
                         t
                         (make-element #f
                                       (list
                                        t
                                        (hspace 1)
                                        v))))]
        [super-long? ((+ (element-width ret)
                         1
                         (element-width name)
                         1
                         (apply max 0 (map (lambda (t v)
                                             (+ (element-width t)
                                                1
                                                (element-width v)))
                                           types
                                           args))
                         1)
                      . > .
                      65)])
    (make-splice
     (cons
      (boxed
       (make-table
        #f
        (append
         (if super-long?
             (list (list (to-flow ret) 'cont 'cont))
             null)
         (list
          (append
           (if super-long?
               null
               (list (to-flow ret)
                     (to-flow spacer)))
           (list (to-flow name)
                 (to-flow (tt "("))
                 (if (null? types)
                     (to-flow (tt ")"))
                     (to-flow (make-element
                               #f
                               (cons (pair-type (car types) (car args))
                                     (if (null? (cdr types))
                                         (list (tt ")"))
                                         (list (tt ","))))))))))
         (if (null? types)
             null
             (let loop ([types (cdr types)]
                        [args (cdr args)])
               (if (null? types)
                   null
                   (cons 
                    (append
                     (if super-long?
                         null
                         (list (to-flow spacer)
                               (to-flow spacer)))
                     (list (to-flow spacer)
                           (to-flow spacer)
                           (to-flow (make-element
                                     #f
                                     (cons
                                      (pair-type (car types) (car args))
                                      (if (null? (cdr types))
                                          (list (tt ")"))
                                          (list (tt ","))))))))
                    (loop (cdr types) (cdr args)))))))))
      (rest-thunk)))))

(define (boxed t)
  (make-table
   'boxed
   (list (list (make-flow (list t))))))

(define (cpp/sym s)
  (cpp (symbol->string s)))

(define (type/sym s)
  (cpp (regexp-replace* #rx"-" (symbol->string s) " ")))

(define (var/sym s)
  (*var (symbol->string s)))

(define cpp
  (case-lambda
   [(x)
    (if (string? x)
        (let ([e (tt x)])
          (make-delayed-element
           (lambda (r part ri)
             (let ([d (resolve-get/tentative part ri `(cpp ,x))])
               (list
                (if d
                    (make-link-element syntax-link-color (list e) `(cpp ,x))
                    e))))
           (lambda () e)
           (lambda () e)))
        (tt x))]
   [more (apply tt more)]))
   
(define cppi cpp)
(define cppdef (lambda (x) (as-cpp-defn x (cpp x))))
(define *var italic)

(define mzc (exec "raco ctool"))

(define reference-doc '(lib "scribblings/reference/reference.scrbl"))
(define raco-doc '(lib "scribblings/raco/raco.scrbl"))

(define (refsecref s)
  (secref #:doc reference-doc s))

(define Racket
  (other-manual '(lib "scribblings/reference/reference.scrbl")))

(define tech-place 
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") "place"))

(define (FormatD s)
  (litchar (string-append "%" s)))
