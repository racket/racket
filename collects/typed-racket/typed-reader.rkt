#lang racket/base

;; Provides raise-read-error and raise-read-eof-error
(require syntax/readerr)

(define (skip-whitespace port)
  ;; Skips whitespace characters, sensitive to the current
  ;; readtable's definition of whitespace
  (let ([ch (peek-char port)])
    (unless (eof-object? ch)
      ;; Consult current readtable:
      (let-values ([(like-ch/sym proc dispatch-proc)
                    (readtable-mapping (current-readtable) ch)])
        ;; If like-ch/sym is whitespace, then ch is whitespace
        (when (and (char? like-ch/sym)
                   (char-whitespace? like-ch/sym))
          (read-char port)
          (skip-whitespace port))))))

(define (skip-comments read-one port src)
  ;; Recursive read, but skip comments and detect EOF
  (let loop ()
    (let ([v (read-one)])
      (cond
        [(special-comment? v) (loop)]
        [(eof-object? v)
         (let-values ([(l c p) (port-next-location port)])
           (raise-read-eof-error "unexpected EOF in type annotation" src l c p 1))]
        [else v]))))

(define (parse port read-one src)
  (skip-whitespace port)
  (let ([name (read-one)])
    (begin0
      (begin (skip-whitespace port)
             (let ([next (read-one)])
               (case (syntax-e next)
                 ;; type annotation
                 [(:) (skip-whitespace port)
                      (syntax-property name 'type-label (syntax->datum (read-one)))]
                 [(::) (skip-whitespace port)
                       (datum->syntax name `(ann ,name : ,(read-one)))]
                 [(@) (let ([elems (let loop ([es '()])
                                     (skip-whitespace port)
                                     (if (equal? #\} (peek-char port))
                                         (reverse es)
                                         (loop (cons (read-one) es))))])
                        (datum->syntax name `(inst ,name : ,@elems)))]
                 ;; arbitrary property annotation
                 [(PROP) (skip-whitespace port)
                         (let* ([prop-name (syntax-e (read-one))])
                           (skip-whitespace port)
                           (syntax-property name prop-name (read-one)))]
                 ;; otherwise error
                 [else
                  (let-values ([(l c p) (port-next-location port)])
                    (raise-read-error (format "typed expression ~a must be followed by :, ::, or @"
                                              (syntax->datum name)) src l c p 1))])))
      (skip-whitespace port)
      (let ([c (read-char port)])
        (unless (equal? #\} c)
          (let-values ([(l c p) (port-next-location port)])
            (raise-read-error (format "typed expression ~a not properly terminated" (syntax->datum name)) src l c p 1)))))))

(define parse-id-type
  (case-lambda
    [(ch port src line col pos)
     ;; `read-syntax' mode
     (datum->syntax
      #f
      (parse port
             (lambda () (read-syntax src port ))
             src)
      (let-values ([(l c p) (port-next-location port)])
        (list src line col pos (and pos (- p pos)))))]))

(define (readtable)
  (make-readtable (current-readtable) #\{ 'dispatch-macro parse-id-type))

(define (*read inp)
  (parameterize ([current-readtable (readtable)])
    (read inp)))

(define (*read-syntax src port)
  (parameterize ([current-readtable (readtable)])
    (read-syntax src port)))

(provide readtable (rename-out [*read read] [*read-syntax read-syntax]))
