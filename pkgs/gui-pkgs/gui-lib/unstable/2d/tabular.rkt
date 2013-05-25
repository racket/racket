#lang racket/base
(require (for-syntax racket/base)
         scribble/base
         scribble/core)
(provide 2dtabular)
(define-syntax (2dtabular stx)
  (syntax-case stx ()
    [(_ cols rows cells ...)
     (let ()
       (define row-count (length (syntax->list #'rows)))
       (define col-count (length (syntax->list #'cols)))
       (define table (make-hash))
       (define the-sep #f)
       (define the-style #f)
       (define ignore-first-line? #f)
       (define has-keywords? #f)
       (for ([cell (in-list (syntax->list #'(cells ...)))])
         (syntax-case cell ()
           [[(coord ...) body ...]
            (let ()
              (define coords
                (sort
                 (for/list ([coord (in-list (syntax->list #'(coord ...)))])
                   (define lst (syntax->datum coord))
                   (cons (car lst)
                         (cadr lst)))
                 <
                 #:key car))
              (define bodies (syntax->list #'(body ...)))
              (unless (or (null? (cdr coords)) (apply = (map cdr coords)))
                (raise-syntax-error '2dtabular
                                    "cells may not span rows"
                                    stx
                                    #f
                                    bodies))
              (define keyword-line? 
                (and (= (+ (cdr (car coords)) 1) row-count)
                     (= (length coords) col-count)
                     (ormap (λ (x) (keyword? (syntax-e x))) bodies)))
              (when keyword-line? (set! has-keywords? #t))
              (cond
                [keyword-line?
                 ;; last row, spans the entire table, contains keywords
                 ;; => treat as keyword arguments to tabular
                 (let loop ([bodies bodies])
                   (syntax-case bodies ()
                     [(#:style style-arg . rest)
                      (begin
                        (set! the-style #'style-arg)
                        (loop #'rest))]
                     [(#:style)
                      (raise-syntax-error '2dtabular 
                                          "expected a style to follow the #:style keyword"
                                          stx
                                          (car bodies))]
                     [(#:sep sep-arg . rest)
                      (begin
                       (set! the-sep #'sep-arg)
                       (loop #'rest))]
                     [(#:sep)
                      (raise-syntax-error '2dtabular 
                                          "expected a separator to follow the #:sep keyword"
                                          stx
                                          (car bodies))]
                     [(#:ignore-first-row . rest)
                      (begin (set! ignore-first-line? #t)
                             (loop #'rest))]
                     [() (void)]
                     [(a . b)
                      (cond
                        [(special-comment? (syntax-e #'a))
                         (loop #'b)]
                        [else
                         (raise-syntax-error '2dtabular
                                             "expected either the keyword #:style #:sep or #:ignore-first-row"
                                             stx
                                             #'a)])]))]
                [else
                 (define no-comment-bodies
                   (for/list ([body (in-list bodies)]
                              #:unless (special-comment? (syntax-e body)))
                     (when (keyword? (syntax-e body))
                       (raise-syntax-error '2dtabular
                                           "unexpected keyword"
                                           stx
                                           body))
                     body))
                 (hash-set! table 
                            (car coords) 
                            #`(build-block #,@no-comment-bodies))
                 (for ([coord (in-list (cdr coords))])
                   (hash-set! table coord #''cont))]))]))
       #`(tabular #,@(if the-style #`(#:style #,the-style) #'())
                  #,@(if the-sep #`(#:sep #,the-sep) #'())
                  (list #,@(for/list ([y (in-range 
                                          (if ignore-first-line? 1 0)
                                          (if has-keywords?
                                              (- row-count 1)
                                              row-count))])
                             #`(list #,@(for/list ([x (in-range col-count)])
                                          (hash-ref table (cons x y))))))))]))

(define (build-block . block-or-contents)
  (define (build-block pending)
    (paragraph (style #f '()) (reverse pending)))
  
  (define blocks
    (let loop ([args block-or-contents]
               [pending '()])
      (cond
        [(null? args) 
         (if (null? pending)
             '()
             (list (build-block pending)))]
        [else
         (define arg (car args))
         (cond
           [(content? arg)
            (loop (cdr args) (cons arg pending))]
           [else
            (if (null? pending)
                (cons arg (loop (cdr args) '()))
                (list* (build-block pending)
                       arg
                       (loop (cdr args) '())))])])))
  
  (nested-flow (style #f '()) blocks))

         
         
         
         