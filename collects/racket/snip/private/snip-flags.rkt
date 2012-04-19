#lang racket/base
(require (for-syntax racket/base))

(provide (all-defined-out))

(define NO-FLAGS 0)

(define IS-TEXT #x1)
(define CAN-APPEND #x2)
(define INVISIBLE #x4)
(define NEWLINE #x8) ;; Soft newline, typically inserted by text%
(define HARD-NEWLINE #x10) ;; => Snip must be follwed by newline 
(define HANDLES-EVENTS #x20)
(define WIDTH-DEPENDS-ON-X #x40)
(define HEIGHT-DEPENDS-ON-Y #x80)
(define WIDTH-DEPENDS-ON-Y #x100)
(define HEIGHT-DEPENDS-ON-X #x200)
(define ANCHORED #x400)
(define USES-BUFFER-PATH #x800)
(define CAN-SPLIT #x1000) ;; safety feature
(define OWNED #x2000)
(define CAN-DISOWN #x4000)
(define HANDLES-ALL-MOUSE-EVENTS #x8000)

(define-syntax-rule (has-flag? flags flag)
  (not (zero? (bitwise-and flags flag))))

(define-syntax-rule (add-flag flags flag)
  (bitwise-ior flags flag))

(define-syntax-rule (remove-flag flags flag)
  (bitwise-and flags (bitwise-not flag)))

(define (copy-flag from to flag)
  (if (has-flag? from flag)
      (add-flag to flag)
      (remove-flag to flag)))

(define (flags->symbols flag)
  (let-syntax ([syms
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ id ...)
                     (with-syntax ([(sym ...)
                                    (map (lambda (s)
                                           (string->symbol
                                            (string-downcase
                                             (symbol->string
                                              (syntax-e s)))))
                                         (syntax->list #'(id ...)))])
                       #'(append
                          (if (has-flag? flag id)
                              '(sym)
                              null)
                          ...))]))])
    (syms IS-TEXT 
          CAN-APPEND 
          INVISIBLE 
          NEWLINE 
          HARD-NEWLINE 
          HANDLES-EVENTS 
          WIDTH-DEPENDS-ON-X 
          HEIGHT-DEPENDS-ON-Y 
          WIDTH-DEPENDS-ON-Y 
          HEIGHT-DEPENDS-ON-X
          HANDLES-ALL-MOUSE-EVENTS)))

(define (symbols->flags symbols)
  (let-syntax ([syms
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ id ...)
                     (with-syntax ([(sym ...)
                                    (map (lambda (s)
                                           (string->symbol
                                            (string-downcase
                                             (symbol->string
                                              (syntax-e s)))))
                                         (syntax->list #'(id ...)))])
                       #'(bitwise-ior
                          (if (memq 'sym symbols)
                              id
                              0)
                          ...))]))])
    (syms IS-TEXT 
          CAN-APPEND 
          INVISIBLE 
          NEWLINE 
          HARD-NEWLINE 
          HANDLES-EVENTS 
          WIDTH-DEPENDS-ON-X 
          HEIGHT-DEPENDS-ON-Y 
          WIDTH-DEPENDS-ON-Y 
          HEIGHT-DEPENDS-ON-X
          HANDLES-ALL-MOUSE-EVENTS)))
