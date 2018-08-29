#lang racket/base
(require racket/match)

(provide check-global)

(define (check-global linklet global-ok)
  (define es (cdddr linklet))

  (define vars (make-hasheq))

  ;; Get all variables that are not under `lambda`. That's not
  ;; necessarily all variables that act like globals, since a
  ;; top-level expression could call a function that allocates a
  ;; mutable variable, but it's close enough to be useful.
  (for ([e (in-list es)])
    (let loop ([e e])
      (match e
        [`(define-values (,ids ...) ,rhs)
         (for ([id (in-list ids)])
           (hash-set! vars id #t))
         (loop rhs)]
        [`(lambda . ,_) (void)]
        [`(case-lambda . ,_) (void)]
        [`(let-values ([,idss ,rhss] ...) ,bodys ...)
         (for* ([ids (in-list idss)]
                [id (in-list ids)])
           (hash-set! vars id #t))
         (for ([rhs (in-list rhss)])
           (loop rhs))
         (for ([body (in-list bodys)])
           (loop body))]
        [`(,es ...)
         (for ([e (in-list es)])
           (loop e))]
        [_ #f])))

  (define complained (make-hasheq))

  (define (found-state! id e)
    (when (hash-ref vars id #f)
      (unless (hash-ref global-ok id #f)
        (unless (hash-ref complained id #f)
          (hash-set! complained id #t)
          (eprintf "Place-spanning global ~s at ~s\n" id e)))))

  ;; A variable acts like a global if it's `set!`ed or if it
  ;; holds a box whose content is mutated, etc. Again, since
  ;; a variable's box could be passed to some other function
  ;; that mutates the box, this check is incomplete, but it's
  ;; likely a useful check.
  (for ([e (in-list es)])
    (let loop ([e e])
      (match e
        [`(set! ,id ,rhs)
         (found-state! id e)
         (loop rhs)]
        [`(set-box! ,target ,rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop rhs)]
        [`(box-cas! ,target ,rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop rhs)]
        [`(hash-set! ,target ,key-rhs ,val-rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop key-rhs)
         (loop val-rhs)]
        [`(vector-set! ,target ,key-rhs ,val-rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop key-rhs)
         (loop val-rhs)]
        [`(,es ...)
         (for ([e (in-list es)])
           (loop e))]
        [_ #f])))

  (when (positive? (hash-count complained))
    (exit 1)))
