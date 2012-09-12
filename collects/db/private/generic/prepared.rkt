#lang racket/base
(require racket/class
         "interfaces.rkt"
         "sql-data.rkt")
(provide prepared-statement%
         statement:after-exec
         apply-type-handlers)

;; A dvec is an opaque value that describes a parameter or result field's type
;; information. (Usually a vector, thus "dvec" for "description vector".)

;; prepared-statement%
(define prepared-statement%
  (class* object% (prepared-statement<%>)
    (init ([-owner owner]))
    (init-field handle            ;; handle, determined by database system, #f means closed
                close-on-exec?    ;; boolean
                param-typeids     ;; (listof typeid)
                result-dvecs      ;; (listof dvec)
                [stmt #f]         ;; string/#f
                [stmt-type #f])   ;; usually symbol or #f (see classify-*-sql)

    (define owner (make-weak-box -owner))
    (define dbsystem (send -owner get-dbsystem))

    (define param-handlers (send dbsystem get-parameter-handlers param-typeids))
    (define result-typeids (send dbsystem field-dvecs->typeids result-dvecs))

    (define/public (get-handle) handle)
    (define/public (set-handle h) (set! handle h))

    (define/public (get-close-on-exec?) close-on-exec?)
    (define/public (after-exec need-lock?)
      (when close-on-exec? ;; indicates ad-hoc prepared statement
        (finalize need-lock?)))

    (define/public (get-stmt) stmt)
    (define/public (get-stmt-type) stmt-type)

    (define/public (get-param-count) (length param-typeids))
    (define/public (get-param-typeids) param-typeids)

    (define/public (get-result-dvecs) result-dvecs)
    (define/public (get-result-count) (length result-dvecs))
    (define/public (get-result-typeids) result-typeids)

    (define/public (get-param-types)
      (send dbsystem describe-params param-typeids))
    (define/public (get-result-types)
      (send dbsystem describe-fields result-dvecs))

    ;; checktype is either #f, 'rows, or exact-positive-integer
    (define/public (check-results fsym checktype obj)
      (cond [(eq? checktype 'rows)
             (unless (positive? (get-result-count))
               (when close-on-exec? (finalize #t))
               (error/want-rows fsym obj #f))]
            [(exact-positive-integer? checktype)
             (unless (= (get-result-count) checktype)
               (when close-on-exec? (finalize #t))
               (error/column-count fsym obj checktype (get-result-count) #f))]
            [else (void)]))

    (define/public (check-owner fsym c obj)
      (unless handle
        (error fsym "prepared statement is closed"))
      (unless (eq? c (weak-box-value owner))
        (error fsym "prepared statement owned by another connection")))

    (define/public (bind fsym params)
      (statement-binding this (apply-type-handlers fsym params param-handlers)))

    (define/public (finalize need-lock?)
      (when handle
        (let ([owner (weak-box-value owner)])
          (when owner
            (send owner free-statement this need-lock?)))))

    (define/public (register-finalizer)
      (thread-resume finalizer-thread (current-thread))
      (will-register will-executor this (lambda (pst) (send pst finalize #t))))

    (super-new)
    (register-finalizer)))

(define (statement:after-exec stmt need-lock?)
  (when (statement-binding? stmt)
    (send (statement-binding-pst stmt) after-exec need-lock?)))

(define (apply-type-handlers fsym params param-handlers)
  (let ([given-len (length params)]
        [expected-len (length param-handlers)])
    (when (not (= given-len expected-len))
      (error/stmt-arity fsym expected-len given-len)))
  (for/list ([handler (in-list param-handlers)]
             [param (in-list params)])
    (cond [(sql-null? param) sql-null]
          [else (handler fsym param)])))

;; ----

(define will-executor (make-will-executor))

(define finalizer-thread
  (thread/suspend-to-kill
   (lambda ()
     (let loop ()
       (with-handlers
           ([(lambda (e) #t)
             (lambda (e)
               ((error-display-handler)
                (cond [(exn? e)
                       (format "prepared statement finalizer thread handled exception:\n~a"
                               (exn-message e))]
                      [else
                       "prepared statement finalizer thread handled non-exception"])
                e))])
         (will-execute will-executor))
       (loop)))))
