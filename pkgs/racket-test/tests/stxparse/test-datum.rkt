#lang racket/base
(require rackunit syntax/parse syntax/datum)

(define (equalish? a b)
  (cond [(and (syntax? a) (syntax? b))
         (equal? (syntax->datum a) (syntax->datum b))]
        [else (equal?/recur a b equalish?)]))

(check equalish?
       (syntax-case #'(a b c) ()
         [(x ...) (datum (x ...))])
       (list #'a #'b #'c))

(struct ast:bind (var rhs) #:prefab)

(define-syntax-class binding
  (pattern [var:id rhs:expr]
           #:attr ast (ast:bind #'var #'rhs)))

(check equalish?
       (syntax-parse #'([x 1] [y 2])
         [(b:binding ...) (datum (b ...))])
       (list #'[x 1] #'[y 2]))

(check equalish?
       (syntax-parse #'([x 1] [y 2])
         [(b:binding ...) (datum ((b.var b.rhs) ...))])
       (list (list #'x #'1) (list #'y #'2)))

(check equalish?
       (syntax-parse #'([x 1] [y 2])
         [(b:binding ...) (datum (b.ast ...))])
       (list (ast:bind #'x #'1) (ast:bind #'y #'2)))

(check equalish?
       (syntax-parse #'([x 1] [y 2])
         [(b:binding ...) (datum ((~@ b.var b.rhs) ...))])
       (list #'x #'1 #'y #'2))

(check equalish?
       (syntax-parse #'([x 1] [y 2])
         [(b:binding ...) (datum ((~@ . b) ...))])
       (list #'x #'1 #'y #'2))

(define-syntax-class obinding
  (pattern [var:id (~optional (rhs:expr ...))]
           #:attr ast (ast:bind #'var (datum (~? (rhs ...) #f)))))

(check equalish?
       (syntax-parse #'([x (1)] [y])
         [(b:obinding ...) (datum ([b.var (~? (b.rhs ...))] ...))])
       (list (list #'x (list #'1)) (list #'y)))

(check equalish?
       (syntax-parse #'([x (1)] [y])
         [(b:obinding ...) (datum (b.ast ...))])
       (list (ast:bind #'x (list #'1)) (ast:bind #'y #f)))

;; ------------------------------------------------------------
;; The corner cases...

(require racket/list racket/promise)

;; The following is a consequences of the decision to make (datum a) equivalent,
;; or nearly so, to (attribute a), where a is an attribute. In particular, if a
;; is "absent" (has the value #f), (datum a) returns #f rather than signaling an
;; error. However, if value of a is a promise, it forces the promise.

;; 1: ~? catches attempts to iterate over absent attrs, but not uses of absent
;; attrs. Maybe add some sort of annotation to get other behavior?

(check-equal? (syntax-parse #'(m)
                [(_ (~optional x:id)) (datum (~? x default))])
              #f)

(check-equal? (syntax-parse #'(m)
                [(_ (~optional (x:id ...))) (datum (~? (x ...) default))])
              'default)

;; 2: Unlike attribute, datum forces promises.

(define-syntax-class nrange #:attributes ([r 0] [k 1])
  (pattern n:nat
           ;; Note: these attribute declarations are identical except for depth.
           #:attr [r 0] (delay (range (syntax-e #'n)))
           #:attr [k 1] (delay (range (syntax-e #'n)))))

(check-equal? (syntax-parse #'(m 10) [(_ n:nrange) (datum (n.k ...))])
              (range 10))

(check-equal? (syntax-parse #'(m 10) [(_ n:nrange) (datum n.r)])
              (range 10))
