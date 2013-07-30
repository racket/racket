#lang racket/base
(require "../gentest-framework.rkt")
(provide proto:modules)

(define-syntax-rule (testM form . clauses)
  (test (format "~s" 'form) form . clauses))

(define-tests proto:modules "Modules"
  ;; FIXME: Finish adding hidden steps for modules

  (test "module, MB, def"
        (module m '#%kernel (#%module-begin (define-values (x) 'a)))
         #:no-steps
         #:no-hidden-steps)
  (test "module, def"
        (module m '#%kernel (define-values (x) 'a))
        [#:steps
         (tag-module-begin
          (module m '#%kernel (#%module-begin (define-values (x) 'a))))]
        #:no-hidden-steps)
  (test "module, MB, def, use"
        (module m '#%kernel (#%module-begin (define-values (x) 'a) x))
        #:no-steps
        #:no-hidden-steps)
  (test "module, def, use"
        (module m '#%kernel (define-values (x) 'a) x)
        [#:steps
         (tag-module-begin
          (module m '#%kernel (#%module-begin (define-values (x) 'a) x)))]
        #:no-hidden-steps)
  (test "module, MB, quote"
        (module m '#%kernel (#%module-begin 'a))
        #:no-steps
        #:no-hidden-steps)
  (test "module, quote"
        (module m '#%kernel 'a)
        [#:steps
         (tag-module-begin (module m '#%kernel (#%module-begin 'a)))]
        #:no-hidden-steps)
  (test "module, 2 quotes"
        (module m '#%kernel 'a 'b)
        [#:steps
         (tag-module-begin (module m '#%kernel (#%module-begin 'a 'b)))]
        #:no-hidden-steps)
  (test "module, MB, begin"
        (module m '#%kernel (#%module-begin (begin 'a 'b)))
        [#:steps
         (splice-module (module m '#%kernel (#%module-begin 'a 'b)))]
        #:same-hidden-steps)
  (test "module, begin"
        (module m '#%kernel (begin 'a 'b))
        [#:steps
         (tag-module-begin (module m '#%kernel (#%module-begin (begin 'a 'b))))
         (splice-module (module m '#%kernel (#%module-begin 'a 'b)))]
        #:no-hidden-steps)
  (test "module, MB, def in begin"
        (module m '#%kernel (#%module-begin (begin (define-values (x) 'a) x)))
        [#:steps
         (splice-module
          (module m '#%kernel (#%module-begin (define-values (x) 'a) x)))]
        #:same-hidden-steps)
  (test "module, def in begin"
        (module m '#%kernel (begin (define-values (x) 'a) x))
        [#:steps
         (tag-module-begin 
          (module m '#%kernel (#%module-begin (begin (define-values (x) 'a) x))))
         (splice-module
          (module m '#%kernel (#%module-begin (define-values (x) 'a) x)))]
        #:no-hidden-steps)

  (test "module, MB, defstx, use"
        (module m '#%kernel
          (#%module-begin
           (#%require (for-syntax '#%kernel))
           (define-syntaxes (x) (lambda (_) (if '#t (quote-syntax *) '#f)))
           (x)))
        [#:steps
         (rename-lambda
          (module m '#%kernel
            (#%module-begin
             (#%require (for-syntax '#%kernel))
             (define-syntaxes (x) (lambda (_) (if '#t (quote-syntax *) '#f)))
             (x))))
         (macro
             (module m '#%kernel
               (#%module-begin
                (#%require (for-syntax '#%kernel))
                (define-syntaxes (x) (lambda (_) (if '#t (quote-syntax *) '#f)))
                *)))]
        [#:hidden-steps
         (rename-lambda
          (module m '#%kernel
            (#%module-begin
             (#%require (for-syntax '#%kernel))
             (define-syntaxes (x) (lambda (_) (if '#t (quote-syntax *) '#f)))
             (x))))])

  (test "module k+helper, macro use"
        (module m '#%kernel (#%require 'helper) (Tid 'a))
        [#:steps
         (tag-module-begin
          (module m '#%kernel (#%module-begin (#%require 'helper) (Tid 'a))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             'a)))]
        [#:hidden-steps
         (macro
          (module m '#%kernel
            (#%require 'helper)
            'a))])

  (test "module k+helper, defs and opaque macros"
        (module m '#%kernel
          (#%module-begin
           (#%require 'helper)
           (id (define-values (x) (id '1)))
           (id (define-values (y) (id '2)))))
        [#:steps
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) (id '1))
             (id (define-values (y) (id '2))))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) (id '1))
             (define-values (y) (id '2)))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) '1)
             (define-values (y) (id '2)))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) '1)
             (define-values (y) '2))))]
        #:no-hidden-steps)

  (test "module k+helper, defs and mixed macros"
        (module m '#%kernel
          (#%module-begin
           (#%require 'helper)
           (Tid (define-values (x) (id '1)))
           (id (define-values (y) (Tid '2)))))
        [#:steps
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) (id '1))
             (id (define-values (y) (Tid '2))))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) (id '1))
             (define-values (y) (Tid '2)))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) '1)
             (define-values (y) (Tid '2)))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) '1)
             (define-values (y) '2))))]
        [#:hidden-steps
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) (id '1))
             (id (define-values (y) (Tid '2))))))
         (macro
          (module m '#%kernel
            (#%module-begin
             (#%require 'helper)
             (define-values (x) (id '1))
             (id (define-values (y) '2)))))])

  ;; need to test:
  ;;   begin-splicing
  ;;   lifts

  (test "module mz, def, use"
        (module m mzscheme (define-values (x) 'a) x)
        [#:steps
         (tag-module-begin
          (module m mzscheme (#%module-begin (define-values (x) 'a) x)))
         (macro
          (module m mzscheme 
            (#%plain-module-begin
             (#%require (for-syntax mzscheme))
             (define-values (x) 'a)
             x)))]
        #:no-hidden-steps)
  (test "module mz, def"
        (module m mzscheme (define-values (x) 'a))
        [#:steps
         (tag-module-begin
          (module m mzscheme (#%module-begin (define-values (x) 'a))))
         (macro
             (module m mzscheme
               (#%plain-module-begin 
                (#%require (for-syntax mzscheme))
                (define-values (x) 'a))))]
        #:no-hidden-steps)
  (test "module mz, quote"
        (module m mzscheme 'a)
        [#:steps
         (tag-module-begin
          (module m mzscheme (#%module-begin 'a)))
         (macro
             (module m mzscheme
               (#%plain-module-begin
                (#%require (for-syntax mzscheme))
                'a)))]
        #:no-hidden-steps)

  (test "module mz, begin with 2 quotes"
        (module m mzscheme (begin 'a 'b))
        [#:steps
         (tag-module-begin
          (module m mzscheme (#%module-begin (begin 'a 'b))))
         (macro
             (module m mzscheme 
               (#%plain-module-begin
                (#%require (for-syntax mzscheme))
                (begin 'a 'b))))
         (splice-module
          (module m mzscheme
            (#%plain-module-begin
             (#%require (for-syntax mzscheme))
             'a 'b)))]
        #:no-hidden-steps)

  (test "module mz, macro use, quote"
        (module m mzscheme (or 'a 'b) 'c)
        [#:steps
         (tag-module-begin
          (module m mzscheme (#%module-begin (or 'a 'b) 'c)))
         (macro
          (module m mzscheme 
            (#%plain-module-begin
             (#%require (for-syntax mzscheme))
             (or 'a 'b)
             'c)))
         (macro
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let ([or-part 'a])
                (if or-part or-part (or 'b)))
              'c)))
         (macro
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part (or 'b)))
              'c)))
         (rename-let-values
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part (or 'b)))
              'c)))
         (macro
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part (#%expression 'b)))
              'c)))
         (macro ;; FIXME: 'untag-expr
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part 'b))
              'c)))]
        #:no-hidden-steps)

  (test "module mz, macro use"
        (module m mzscheme (or 'a 'b))
        [#:steps
         (macro
          (module m mzscheme
            (let ([or-part 'a]) (if or-part or-part (or 'b)))))
         (macro
          (module m mzscheme
            (let-values ([(or-part) 'a]) (if or-part or-part (or 'b)))))
         (tag-module-begin
          (module m mzscheme
            (#%module-begin
             (let-values ([(or-part) 'a])
               (if or-part or-part (or 'b))))))
         (macro
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part (or 'b))))))
         (rename-let-values
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part (or 'b))))))
         (macro
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part (#%expression 'b))))))
         (macro ;; FIXME: 'untag-expr
           (module m mzscheme
             (#%plain-module-begin
              (#%require (for-syntax mzscheme))
              (let-values ([(or-part) 'a])
                (if or-part or-part 'b)))))])
  ;; FIXME: hidden steps for above, tricky

  (test "module with define-struct"
        (module m mzscheme
          (define-struct P (x y))
          (P? (make-P P-x P-y))))
  (test "module with match"
        (module m mzscheme
          (require (lib "match.rkt"))
          (match '4 [n (add1 n)])))
  (test "module with match before require"
        (module m mzscheme
          (match '4 [n (add1 n)])
          (require (lib "match.rkt")))))
