#lang racket/base
(require racket/contract
         racket/generic
         racket/stream
         (for-syntax racket/base))

(provide gen:set generic-set? set-implements?

         set-empty? set-member? set-count
         set=? subset? proper-subset?
         set-map set-for-each
         set-copy set-copy-clear
         set->list set->stream set-first set-rest
         set-add set-remove set-clear
         set-union set-intersect set-subtract set-symmetric-difference
         set-add! set-remove! set-clear!
         set-union! set-intersect! set-subtract! set-symmetric-difference!

         (rename-out [*in-set in-set])
         set-implements/c)

;; Method implementations for lists:

(define (list-member? s x) (pair? (member x s)))

(define (list-set=? s1 s2)
  (unless (list? s2)
    (raise-argument-error 'set=? "list?" s2))
  (and (for/and ([x (in-list s1)]) (member x s2))
       (for/and ([x (in-list s2)]) (member x s1))
       #t))

(define (list-subset? s1 s2)
  (unless (list? s2)
    (raise-argument-error 'subset? "list?" s2))
  (and (for/and ([x (in-list s1)]) (member x s2))
       #t))

(define (list-proper-subset? s1 s2)
  (unless (list? s2)
    (raise-argument-error 'proper-subset? "list?" s2))
  (and (for/and ([x (in-list s1)]) (member x s2))
       (for/or ([x (in-list s2)]) (not (member x s1)))
       #t))

(define (list-map s f) (map f s))

(define (list-for-each s f) (for-each f s))

(define (list-add s x)
  (if (member x s) s (cons x s)))

(define (list-remove s . xs) (remove* xs s))

(define (list-clear s) '())

(define (list-union s . sets)
  (for/fold ([s1 s]) ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error 'set-union "list?" i s sets))
    (for/fold ([s1 s1]) ([x (in-list s2)])
      (list-add s1 x))))

(define (list-intersect s . sets)
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (list? s2)
      (apply raise-argument-error 'set-intersect "list?" i s sets)))
  (for/fold
      ([s1 '()])
      ([x (in-list s)]
       #:when (for/and ([s2 (in-list sets)])
                (member x s2)))
    (list-add s1 x)))

(define (list-subtract s . sets)
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (list? s2)
      (apply raise-argument-error 'set-subtract "list?" i s sets)))
  (for/fold
      ([s1 '()])
      ([x (in-list s)]
       #:unless (for/or ([s2 (in-list sets)])
                  (member x s2)))
    (list-add s1 x)))

(define (list-symmetric-difference s . sets)
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (list? s2)
      (apply raise-argument-error 'set-symmetric-difference "list?" i s sets)))
  (for*/fold
      ([s1 s])
      ([s2 (in-list sets)]
       [x (in-list s2)])
    (if (list-member? s1 x)
        (list-remove s1 x)
        (list-add s1 x))))

;; Fallback method implementations:

(define (fallback-empty? s)
  (cond
    [(set-implements? s 'set->stream)
     (stream-empty? (set->stream s))]
    [(set-implements? s 'set-count)
     (zero? (set-count s))]
    [else (raise-support-error 'set-empty? s)]))

(define (fallback-first s)
  (cond
    [(set-implements? s 'set->stream)
     (stream-first (set->stream s))]
    [else (raise-support-error 'set-first s)]))

(define (fallback-rest s)
  (cond
    [(set-implements? s 'set-remove 'set-first)
     (set-remove s (set-first s))]
    [(set-implements? s 'set-remove 'set->stream)
     (set-remove s (stream-first (set->stream s)))]
    [else (raise-support-error 'set-rest s)]))

(define (fallback->stream s)
  (cond
    [(set-implements? s 'in-set) (sequence->stream (in-set s))]
    [(set-implements? s 'set-empty? 'set-first 'set-rest)
     (let loop ([s s])
       (cond
         [(stream-empty? s) empty-stream]
         [else (stream-cons (set-first s)
                            (loop (set-rest s)))]))]
    [(set-implements? s 'set-empty? 'set-first 'set-remove)
     (let loop ([s s])
       (cond
         [(stream-empty? s) empty-stream]
         [else (stream-cons (set-first s)
                            (loop (set-remove s (set-first s))))]))]
    [(set-implements? s 'set-count 'set-first 'set-rest)
     (let loop ([s s])
       (cond
         [(zero? (set-count s)) empty-stream]
         [else (stream-cons (set-first s)
                            (loop (set-rest s)))]))]
    [(set-implements? s 'set-count 'set-first 'set-remove)
     (let loop ([s s])
       (cond
         [(zero? (set-count s)) empty-stream]
         [else (stream-cons (set-first s)
                            (loop (set-remove s (set-first s))))]))]
    [(set-implements? s 'set->list) (set->list s)]
    [else (raise-support-error 'set->stream s)]))

(define (fallback-in-set s)
  (cond
    [(set-implements? s 'set->stream) (set->stream s)]
    [(set-implements? s 'set-empty? 'set-first 'set-rest)
     (make-do-sequence
      (lambda ()
        (values set-first
                set-rest
                s
                (lambda (s) (not (set-empty? s)))
                #f
                #f)))]
    [(set-implements? s 'set-empty? 'set-first 'set-remove)
     (make-do-sequence
      (lambda ()
        (values set-first
                (lambda (s) (set-remove s (set-first s)))
                s
                (lambda (s) (not (set-empty? s)))
                #f
                #f)))]
    [(set-implements? s 'set-count 'set-first 'set-rest)
     (make-do-sequence
      (lambda ()
        (values set-first
                set-rest
                s
                (lambda (s) (not (zero? (set-count s))))
                #f
                #f)))]
    [(set-implements? s 'set-count 'set-first 'set-remove)
     (make-do-sequence
      (lambda ()
        (values set-first
                (lambda (s) (set-remove s (set-first s)))
                s
                (lambda (s) (not (zero? (set-count s))))
                #f
                #f)))]
    [(set-implements? s 'set->list) (set->list s)]
    [else (raise-support-error 'in-set s)]))

(define (fallback-count s)
  (for/sum ([x (*in-set s)]) 1))

(define (fallback-set=? s1 s2)
  (unless (generic-set? s2)
    (raise-argument-error 'set=? "generic-set?" 1 s1 s2))
  (or (eq? s1 s2)
      (cond
        [(set-implements? s2 'set=?) (set=? s1 s2)]
        [else (and (subset? s1 s2)
                   (subset? s2 s1))])))

(define (fallback-proper-subset? s1 s2)
  (unless (generic-set? s2)
    (raise-argument-error 'proper-subset? "generic-set?" 1 s1 s2))
  (and (subset? s1 s2)
       (not (subset? s2 s1))))

(define (fallback-subset? s1 s2)
  (unless (generic-set? s2)
    (raise-argument-error 'subset? "generic-set?" 1 s1 s2))
  (for/and ([x (*in-set s1)])
    (set-member? s2 x)))

(define (fallback-map s f)
  (for/list ([x (*in-set s)])
    (f x)))

(define (fallback-for-each s f)
  (for ([x (*in-set s)])
    (f x)))

(define (fallback-copy s)
  (cond
    [(set-implements? s 'set-copy-clear 'set-add!)
     (define s2 (set-clear s))
     (for ([x (*in-set s)])
       (set-add! s2 x))
     s2]
    [else (raise-support-error 'set-copy s)]))

(define (fallback->list s)
  (for/list ([x (*in-set s)])
    x))

(define (fallback-clear s)
  (cond
    [(set-implements? s 'set-remove)
     (for/fold ([s s]) ([x (*in-set s)])
       (set-remove s x))]
    [else (raise-support-error 'set-clear s)]))

(define (fallback-union s . sets)
  (cond
    [(set-implements? s 'set-add)
     (for/fold ([s1 s]) ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (generic-set? s2)
         (apply raise-argument-error 'set-union "generic-set?" i s sets))
       (for/fold ([s1 s1]) ([x (*in-set s2)])
         (set-add s1 x)))]
    [else (raise-support-error 'set-union s)]))

(define (fallback-intersect s . sets)
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error 'set-intersect "generic-set?" i s sets)))
  (define (keep? x)
    (for/and ([s2 (in-list sets)])
      (set-member? s2 x)))
  (cond
    [(set-implements? s 'set-remove)
     (for/fold ([s1 s]) ([x (*in-set s)] #:unless (keep? x))
       (set-remove s1 x))]
    [(set-implements? s 'set-add 'set-clear)
     (for/fold ([s1 (set-clear s)]) ([x (*in-set s)] #:when (keep? x))
       (set-add s1 x))]
    [else (raise-support-error 'set-intersect s)]))

(define (fallback-subtract s . sets)
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error 'set-subtract "generic-set?" i s sets)))
  (define (remove? x)
    (for/or ([s2 (in-list sets)])
      (set-member? s2 x)))
  (cond
    [(set-implements? s 'set-remove)
     (for/fold ([s1 s]) ([x (*in-set s)] #:when (remove? x))
       (set-remove s1 x))]
    [(set-implements? s 'set-add 'set-clear)
     (for/fold ([s1 (set-clear s)]) ([x (*in-set s)] #:unless (remove? x))
       (set-add s1 x))]
    [else (raise-support-error 'set-subtract s)]))

(define (fallback-symmetric-difference s . sets)
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error
             'set-symmetric-difference
             "generic-set?"
             i
             s
             sets)))
  (define (keep? x)
    (even?
     (for/sum ([s2 (in-list sets)]
               #:when (set-member? s2 x))
       1)))
  (cond
    [(set-implements? s 'set-remove)
     (for/fold ([s1 s]) ([x (*in-set s)] #:unless (keep? x))
       (set-remove s1 x))]
    [(set-implements? s 'set-add 'set-clear)
     (for/fold ([s1 (set-clear s)]) ([x (*in-set s)] #:when (keep? x))
       (set-add s1 x))]
    [else (raise-support-error 'set-symmetric-difference s)]))

(define (fallback-clear! s)
  (cond
    [(set-implements? s 'set-remove! 'set-empty? 'set-first)
     (let loop ()
       (unless (set-empty? s)
         (set-remove! s (set-first s))
         (loop)))]
    [(set-implements? s 'set-remove! 'set->stream)
     (let loop ()
       (define st (set->stream s))
       (unless (stream-empty? st)
         (set-remove! s (stream-first st))
         (loop)))]
    [(set-implements? s 'set-remove! 'set-count 'set-first)
     (let loop ()
       (unless (zero? (set-count s))
         (set-remove! s (set-first s))
         (loop)))]
    [(set-implements? s 'set-remove! 'set->list)
     (for ([x (in-list (set->list s))])
       (set-remove! s x))]
    [else (raise-support-error 'set-clear! s)]))

(define (fallback-union! s . sets)
  (cond
    [(set-implements? s 'set-add!)
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (generic-set? s2)
         (apply raise-argument-error 'set-union! "generic-set?" i s sets))
       (for ([x (*in-set s2)])
         (set-add! s x)))]
    [else (raise-support-error 'set-union! s)]))

(define (fallback-intersect! s . sets)
  (cond
    [(set-implements? s 'set-remove!)
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (generic-set? s2)
         (apply raise-argument-error 'set-intersect! "generic-set?" i s sets)))
     (define (keep? x)
       (for/and ([s2 (in-list sets)])
         (set-member? s2 x)))
     (define to-remove
       (for/list ([x (*in-set s)] #:unless (keep? x))
         x))
     (for ([x (in-list to-remove)])
       (set-remove! s x))]
    [else (raise-support-error 'set-intersect! s)]))

(define (fallback-subtract! s . sets)
  (cond
    [(set-implements? s 'set-remove!)
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (generic-set? s2)
         (apply raise-argument-error 'set-subtract! "generic-set?" i s sets)))
     (define (remove? x)
       (for/or ([s2 (in-list sets)])
         (set-member? s2 x)))
     (define to-remove
       (for/list ([x (*in-set s)] #:when (remove? x))
         x))
     (for ([x (in-list to-remove)])
       (set-remove! s x))]
    [else (raise-support-error 'set-subtract! s)]))

(define (fallback-symmetric-difference! s . sets)
  (cond
    [(set-implements? s 'set-remove!)
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (generic-set? s2)
         (define name 'set-symmetric-difference!)
         (apply raise-argument-error name "generic-set?" i s sets)))
     (define (keep? x)
       (even?
         (for/sum ([s2 (in-list sets)]
                   #:when (set-member? s2 x))
           1)))
     (define to-remove
       (for/list ([x (*in-set s)] #:unless (keep? x))
         x))
     (for ([x (in-list to-remove)])
       (set-remove! s x))]
    [else (raise-support-error 'set-symmetric-difference! s)]))

(define-sequence-syntax *in-set
  (lambda () #'in-set)
  (lambda (stx)
    (syntax-case stx ()
      [[(x) (_ e)]
       #'[(x) (in-set e)]]
      [_ #f])))

(define (set-implements/c . syms)
  (if (null? syms)
      generic-set?
      (flat-named-contract
        `(set-implements/c . ,syms)
        (lambda (x)
          (and (generic-set? x)
               (for/and ([sym (in-list syms)])
                 (set-implements? x sym)))))))

;; Generics definition:

(define-generics set
  #:defined-predicate set-implements?

  (set-empty? set)
  (set-member? set x)
  (set-count set)
  (set=? set set2)
  (subset? set set2)
  (proper-subset? set set2)
  (set-map set f)
  (set-for-each set f)
  (set-copy set)
  (set-copy-clear set)
  (in-set set)
  (set->list set)
  (set->stream set)
  (set-first set)
  (set-rest set)
  (set-add set x)
  (set-remove set x)
  (set-clear set)
  (set-union set . sets)
  (set-intersect set . sets)
  (set-subtract set . sets)
  (set-symmetric-difference set . sets)
  (set-add! set x)
  (set-remove! set x)
  (set-clear! set)
  (set-union! set . sets)
  (set-intersect! set . sets)
  (set-subtract! set . sets)
  (set-symmetric-difference! set . sets)

  #:fast-defaults
  ([list?
    (define set-empty? null?)
    (define set-member? list-member?)
    (define set-count length)
    (define set=? list-set=?)
    (define subset? list-subset?)
    (define proper-subset? list-proper-subset?)
    (define set-map list-map)
    (define set-for-each list-for-each)
    (define set-copy-clear list-clear)
    (define in-set in-list)
    (define set->list values)
    (define set->stream values)
    (define set-first car)
    (define set-rest cdr)
    (define set-add list-add)
    (define set-remove list-remove)
    (define set-clear list-clear)
    (define set-union list-union)
    (define set-intersect list-intersect)
    (define set-subtract list-subtract)
    (define set-symmetric-difference list-symmetric-difference)])

  #:fallbacks
  [(define set-empty? fallback-empty?)
   (define set-count fallback-count)
   (define set=? fallback-set=?)
   (define subset? fallback-subset?)
   (define proper-subset? fallback-proper-subset?)
   (define set-map fallback-map)
   (define set-for-each fallback-for-each)
   (define set-copy fallback-copy)
   (define in-set fallback-in-set)
   (define set->list fallback->list)
   (define set->stream fallback->stream)
   (define set-first fallback-first)
   (define set-rest fallback-rest)
   (define set-clear fallback-clear)
   (define set-union fallback-union)
   (define set-intersect fallback-intersect)
   (define set-subtract fallback-subtract)
   (define set-symmetric-difference fallback-symmetric-difference)
   (define set-clear! fallback-clear!)
   (define set-union! fallback-union!)
   (define set-intersect! fallback-intersect!)
   (define set-subtract! fallback-subtract!)
   (define set-symmetric-difference! fallback-symmetric-difference!)])

(define (generic-set? x) (set? x))
