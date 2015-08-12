#lang racket/base
(require racket/struct
         racket/pretty
         racket/match
         racket/sequence
         racket/contract/base)
(provide make-constructor-style-printer
         (contract-out
          [prop:auto-custom-write
           (struct-type-property/c 'constructor)]))

;; TODO: deal with super struct types better
;;   - see "Problem" below

#|
Constructor-style printer
  - eg 'set' printer
  - in mode 0, "(" + constructor + { " " + elem }* + ")"
  - else,      "#<" + constructor + ":" + ... + ">"
  - print elems w/ same mode
  - never quotable
|#

(define-values (prop:auto-custom-write auto-custom-write? auto-custom-write-proc)
  (make-struct-type-property
   'auto-custom-write
   (lambda (val info)
     (case val
       ((constructor)
        (struct-info->get-constructor+get-contents info))))
   (list (cons prop:custom-print-quotable
               (lambda (auto-write-val) 'never))
         (cons prop:custom-write
               (lambda (auto-write-val)
                 (make-constructor-style-printer
                  (car auto-write-val)
                  (cdr auto-write-val)))))))

(define (struct-info->get-constructor+get-contents info)
  (match info
    [(list name init-ct auto-ct accessor mutator imms super skipped?)
     (let ([get-super-contents
            ;; Problem: if super type was not transparent (to current
            ;; inspector), then we don't get it (ie, skipped? is #t), and so we
            ;; can't tell if super type also has prop:auto-custom-write
            ;; property.
            (cond [skipped?
                   (error 'prop:auto-custom-write
                          "struct super type is inaccessible")]
                  [(not super)
                   #f]
                  [(auto-custom-write? super)
                   (cdr (auto-custom-write-proc super))]
                  [else
                   (let ([super-getters
                          (struct-info->get-constructor+get-contents
                           (call-with-values (lambda () (struct-type-info super))
                             list))])
                     (cdr super-getters))])])
       (define (get-constructor obj)
         name)
       (define (get-new-contents obj)
         (for/list ([i (in-range (+ init-ct auto-ct))])
           (accessor obj i)))
       (cons get-constructor
             (if get-super-contents
                 (lambda (obj)
                   (sequence-append (get-super-contents obj)
                                    (get-new-contents obj)))
                 get-new-contents)))]))
