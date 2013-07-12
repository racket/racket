#lang racket/base
(require racket/contract
         redex/reduction-semantics
         redex/pict
         pict
         racket/list)

;; TO DO:
;; - chained binary operators
;;   eg (+ 1 2 3) => "1 + 2 + 3"
;; - helper for thin spaces around operators/functions ?
;; - rewriters for more conventions (eg subst, env, judgments, ...)

(provide
 (contract-out
  ;; Using and controlling rewriters
  [with-rewriters
   (-> (-> any) any)]
  [current-atomic-rewriters
   (parameter/c (plistof symbol? atomic-rewriter/c))]
  [current-compound-rewriters
   (parameter/c (plistof symbol? compound-rewriter/c))]
  [current-unquote-rewriters
   (parameter/c (plistof (-> lw? any/c) (-> lw? lw?)))]
  [add-atomic-rewriters!
   (->* [] [] #:rest (plistof symbol? atomic-rewriter/c)
        void?)]
  [add-compound-rewriters!
   (->* [] [] #:rest (plistof symbol? compound-rewriter/c)
        void?)]
  [add-unquote-rewriters!
   (->* [] [] #:rest (plistof (-> lw? any/c) (-> lw? lw?))
        void?)]
  ;; Rewriter constructors
  [only-first-rw
   (-> compound-rewriter/c)]
  [binary-rw
   (->* [content/c]
        [#:parenthesize-arg (or/c #t #f (listof symbol?) (-> lw? any/c))
         #:parenthesize-left (or/c #t #f (listof symbol?) (-> lw? any/c))
         #:parenthesize-right (or/c #t #f (listof symbol?) (-> lw? any/c))]
        compound-rewriter/c)]
  [prefix-rw
   (-> content/c
       compound-rewriter/c)]
  [postfix-rw
   (-> content/c
       compound-rewriter/c)]
  [function-rw
   (-> content/c
       compound-rewriter/c)]
  [splice-rw
   (-> compound-rewriter/c)]
  [constant-rw
   (-> content/c
       compound-rewriter/c)]
  [bracket-rw
   (->* [(or/c 'round 'square 'curly 'angle (list/c (or/c string? pict?) (or/c string? pict?)))]
        [#:comma? any/c]
        compound-rewriter/c)]
  [set-cons-rw
   (-> compound-rewriter/c)]
  ;; Contracts
  [compound-rewriter/c
   contract?]
  [plistof
   (-> contract? contract? contract?)]
  #|
  [content/c
   contract?]
  |#))

(define (plistof key/c val/c)
  (letrec ([ctc
            (recursive-contract
             (or/c '()
                   (cons/c key/c (cons/c val/c ctc))))])
    ctc))

(define content/c
  (or/c string? pict? lw? (-> (or/c string? pict? lw?))))

(define atomic-rewriter/c
  (or/c string? pict? (-> (or/c string? pict?))))

(define compound-rewriter/c
  (-> (listof lw?)
      (listof (or/c string? pict? lw?))))

;; ============================================================

(define current-atomic-rewriters (make-parameter null))
(define current-compound-rewriters (make-parameter null))
(define current-unquote-rewriters (make-parameter null))

(define (add-atomic-rewriters! . args)
  (push-rewriters! 'add-atomic-rewriters! current-atomic-rewriters args))
(define (add-compound-rewriters! . args)
  (push-rewriters! 'add-compound-rewrites! current-compound-rewriters args))
(define (add-unquote-rewriters! . args)
  (push-rewriters! 'add-unquote-rewrites! current-unquote-rewriters args))

(define (with-rewriters thunk)
  (with-atomic-rewriter*
   (lambda ()
     (with-compound-rewriter*
      (lambda ()
        (with-unquote-rewriter*
         (lambda ()
           (thunk))
         (reverse (current-unquote-rewriters))))
      (reverse (current-compound-rewriters))))
   (reverse (current-atomic-rewriters))))

;; ============================================================

(define (push-rewriters! who param args)
  (unless (even? (length args))
    (error who "expected even number of arguments, got: ~e" args))
  (param (append args (param))))

;; Note: l is reversed, so rewriter comes first, symbol/pred comes second.
(define (with-atomic-rewriter* thunk l)
  (if (null? l)
      (thunk)
      (with-atomic-rewriter
       (cadr l)
       (let ([rw (car l)])
         (cond [(string? rw)
                (lambda () (text rw (default-style) (default-font-size)))]
               [(pict? rw)
                (lambda () rw)]
               [else rw]))
       (with-atomic-rewriter* thunk (cddr l)))))
(define (with-compound-rewriter* thunk l)
  (if (null? l)
      (thunk)
      (with-compound-rewriter
       (cadr l) (car l)
       (with-compound-rewriter* thunk (cddr l)))))
(define (with-unquote-rewriter* thunk l)
  (with-unquote-rewriter
   (lambda (lw)
     (let loop ([l l])
       (cond [(null? l)
              lw]
             [else
              (let ([pred (cadr l)]
                    [tx (car l)])
                (cond [(pred lw) (tx lw)]
                      [else (loop (cddr l))]))])))
   (thunk)))

;; ============================================================

(define (only-first-rw)
  (compound-rw 'only-first-rw
    (lambda (lws)
      (let ([arg-lw (list-ref lws 2)])
        (list arg-lw)))))

(define (binary-rw op
                   #:parenthesize-arg [parenthesize-arg #f]
                   #:parenthesize-left [parenthesize-left parenthesize-arg]
                   #:parenthesize-right [parenthesize-right parenthesize-arg])
  (compound-rw 'binary-rw
    (lambda (lws)
      (let ([left (list-ref lws 2)]
            [right (list-ref lws 3)])
        ;; (list left (just-after op left) (between "" left right) right)
        ;; (list left (just-after op left) "" right)
        (append (maybe-parenthesize left parenthesize-left)
                (list (between (->string/pict op) left right))
                (maybe-parenthesize right parenthesize-right))))
    2 2))

(define (prefix-rw pre
                   #:parenthesize-arg [parenthesize-arg #f])
  (compound-rw 'prefix-rw
    (lambda (lws)
      (let ([arg-lw (list-ref lws 2)])
        (append (list (just-before (->string/pict pre) arg-lw))
                (maybe-parenthesize arg-lw parenthesize-arg))))
    1 1))

(define (postfix-rw post
                    #:parenthesize-arg [parenthesize-arg #f])
  (compound-rw 'postfix-rw
    (lambda (lws)
      (let ([arg-lw (list-ref lws 2)])
        (append (maybe-parenthesize arg-lw parenthesize-arg)
                (list (just-after (->string/pict post) arg-lw)))))
    1 1))

(define (function-rw name)
  (compound-rw 'function-rw
    (lambda (lws)
      (list* (re-lw (->string/pict name) (list-ref lws 1))
             (between "(" (list-ref lws 1) (list-ref lws 2))
             (comma-ize (cddr lws) #t)))))

(define (bracket-rw brackets #:tall? [tall? #f] #:comma? [comma? #t])
  (let-values ([(left-bracket right-bracket)
                (cond [(symbol? brackets)
                       (case brackets
                         ((round) (values "(" ")"))
                         ((square) (values "[" "]"))
                         ((curly) (values "{" "}"))
                         ((angle) (values "〈" "〉"))
                         ;; FIXME: more
                         (else (error 'bracket-rw "unknown bracket kind: ~e" brackets)))]
                      [(list? brackets)
                       (values (first brackets) (second brackets))])])
    (compound-rw 'bracket-rw
      (lambda (lws)
        (let ([lwA (first lws)]
              [lwZ (last lws)]
              [elems (drop-right (drop lws 2) 1)])
          (append (list (re-lw (if tall? (taller left-bracket) left-bracket) lwA)
                        "")
                  (if comma? (comma-ize elems #f) elems)
                  (list (re-lw (if tall? (taller right-bracket) right-bracket) lwZ))))))))

(define (splice-rw)
  (compound-rw 'splice-rw
    (lambda (lws)
      (drop-right (drop lws 2) 1))))

(define (constant-rw s)
  (compound-rw 'constant-rw
    (lambda (lws)
      (list (re-lw s (first lws))))))

(define (set-cons-rw)
  (compound-rw 'set-cons-rw
    (lambda (lws)
      (list (just-before "{" (list-ref lws 2))
            (list-ref lws 2)
            (just-after "}∪" (list-ref lws 2))
            (between "" (list-ref lws 2) (list-ref lws 3))
            (list-ref lws 3)))))                                  

;; ============================================================

;; Content = (U string symbol pict (listof (U 'spring lw)))

;; between : Content lw lw -> lw
;; Makes an lw with given content and location between given lws.
(define (between s a b)
  (build-lw s
            (lw-line a)
            0
            (+ (lw-column a) (lw-column-span a))
            (max 0 (- (lw-column b)
                      (+ (lw-column a) (lw-column-span a))))))

;; re-lw : Content lw -> lw
;; Makes an lw with locations of old lw and new content.
(define (re-lw new-e lw)
  (build-lw new-e
            (lw-line lw) (lw-line-span lw)
            (lw-column lw) (lw-column-span lw)))

;; refit : (listof lw) (listof lw) -> (listof lw)
;; Add empty lws ("") around new based on locations of old lws (??)
;; Soaks up logical space ????
(define (refit orig new)
  (append (list (between "" (just-before "" (car orig)) (car new)))
          new
          (list (between "" (just-after "" (last new)) (just-after "" (last orig))))))

(define (compound-rw who proc [min-args 0] [max-args +inf.0]
                     #:refit? [refit? #t])
  ((if refit? refit-rw values)
   (lambda (lws)
     (unless (>= (length lws) 3)
       (error who "expected list of at least 3 lws, got: ~e" lws))
     (let ([lwA (first lws)]
           [lwB (second lws)]
           [lwZ (last lws)])
       (unless (member (lw-e lwA) '("(" "[" "{"))
         (error who "expected first lw to contain open-paren, got: ~e" lwA))
       (unless (symbol? (lw-e lwB))
         (error who "expected second lw to contain symbol, got: ~e" lwB))
       (unless (member (lw-e lwZ) '(")" "]" "}"))
         (error who "expected last lw to contain close-paren, got: ~e" lwZ))
       (let ([args (- (length lws) 3)])
         (unless (<= min-args args max-args)
           (if (= min-args max-args)
               (error who "expected ~s argument(s), got ~s: ~e"
                      min-args args lws)
               (error who "expected between ~s and ~s arguments, got ~s: ~e"
                      min-args max-args args lws))))
       (proc lws)))))

(define ((refit-rw proc) lws)
  (refit lws (proc lws)))

(define (taller s)
  (define p
    (cond [(string? s) (text s (default-style) (default-font-size))]
          [(pict? s) s]))
  (define h (pict-height p))
  (drop-below-ascent (scale (launder p) 1 1.3) (* 0.1 h)))

(define (comma-ize lws contains-close-paren?)
  (let loop ([lws lws])
    (cond [(and contains-close-paren?
                (or (null? (cdr lws))
                    (null? (cddr lws))))
           lws]
          [(and (not contains-close-paren?)
                (or (null? lws)
                    (null? (cdr lws))))
           lws]
          [(positive? (lw-line-span (car lws)))
           ;; a line break?
           (cons (car lws) (loop (cdr lws)))]
          [else (list*
                 (car lws)
                 (between ", " (car lws) (cadr lws))
                 (loop (cdr lws)))])))

;; ->string/pict : (U string pict (-> (U string pict)) -> (U string pict)
(define (->string/pict c)
  (cond [(pict? c) c]
        [(string? c) c] ;; ((current-text) c (default-style) (default-font-size))
        [else (c)]))

;; maybe-parenthesize : lw _ -> (nonempty-listof lw)
(define (maybe-parenthesize arg paren-spec)
  (if (parenthesize? arg paren-spec)
      (parenthesize-lws (list arg))
      (list arg)))

;; parenthesize? : lw (U #t #f (listof symbol) (-> lw boolean)) -> boolean
(define (parenthesize? arg parenthesize-arg)
  (cond [(boolean? parenthesize-arg) parenthesize-arg]
        [(list? parenthesize-arg)
         (let ([contents (lw-e arg)])
           (and (not (lw-unq? arg))
                (not (lw-metafunction? arg))
                (list? contents)
                (>= (length contents) 3)
                (member (lw-e (first contents)) '("(" "[" "{"))
                (member (lw-e (last contents)) '(")" "]" "}"))
                (member (lw-e (second contents)) parenthesize-arg)
                #t))]
        [(procedure? parenthesize-arg)
         (parenthesize-arg arg)]))

;; parenthesize-lws : (nonempty-listof lw) -> (nonempty-listof lw)
(define (parenthesize-lws lws)
  (let ([lwA (first lws)]
        [lwZ (last lws)])
    (append (list (just-before "(" lwA))
            lws
            (list (just-after ")" lwZ)))))
