#lang scheme

(require scribble/basic
         (for-syntax scheme/port)
         (except-in scribble/manual link))

(provide ctc-section
         ctc-link
         exercise
         solution
         external-file)

(define (ctc-section #:tag [tag #f] . rest)
  (keyword-apply section
                 '(#:tag)
                 (list (and tag (str->tag tag)))
                 rest))

(define (ctc-link tag . rest) (apply seclink (str->tag tag) rest))

(define (str->tag tag) (format "contracts-~a" tag))

(define exercise-number 0)
(define (exercise)
  (set! exercise-number (+ exercise-number 1))
  (bold (format "Exercise ~a" exercise-number)))

(define (solution)
  (bold (format "Solution to exercise ~a" exercise-number)))

(define-syntax (external-file stx)
  (syntax-case stx ()
    [(_ filename)
     (call-with-input-file (build-path "contracts-examples" (format "~a.ss" (syntax-e #'filename)))
       (λ (port)
         (define prefix "#reader scribble/comment-reader\n[schememod\nscheme\n")
         (define suffix "]")
         (with-syntax ([s (parameterize ([read-accept-reader #t])
                            (read-syntax 'contract-examples
                                         (input-port-append #f 
                                                            (open-input-string prefix)
                                                            port
                                                            (open-input-string suffix))))])
           #'s)))]))

;(external-file 1)