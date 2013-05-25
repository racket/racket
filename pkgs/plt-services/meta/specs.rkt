;; This module defines the specs "language" .  It is basically a simple
;; language of definitions that can expand to anything at all: expanding a spec
;; starts at that symbol and follows definitions until no further expansion is
;; possible.  There are two major points that makes this require a new
;; language: first, expansion is lazy, second, everything is spliced.  To be
;; able to have macros, `lambda' escapes back to Scheme and generates a
;; function.  For more details, see the "distribution-specs" file (large
;; portions of the details there should eventually move here).

#lang scheme/base

;;; ===========================================================================
;;; Utilities etc

(provide mappend)
(define (mappend f l)
  (apply append (map f l)))

(provide filtered-map)
(define (filtered-map f l)
  (reverse
   (foldl (lambda (x y) (let ([x (f x)]) (if x (cons x y) y))) '() l)))

;; a splicing substitution
(define (@subst expr from to)
  (cond [(not (pair? expr))
         (if (equal? expr from) (error '@subst "something bad happened") expr)]
        [(equal? (car expr) from)
         (append to (@subst (cdr expr) from to))]
        [else
         (cons (@subst (car expr) from to) (@subst (cdr expr) from to))]))

;; The input list is expected to be (x1 <sym1> y1 ... x2 <sym2> y2 ... ...),
;; where <symN> is some symbol in the given `syms'.  The result is a list of
;; lists that are split using syms as infix tokens with one element on the
;; left.  The result of the above will be
;;   ((<sym1> x1 y1 ...) (<sym2> x2 y2 ...))
(define (infix-split syms lst)
  (let loop ([l lst] [r '()])
    (cond [(null? l) (reverse r)]
          [(or (null? (cdr l)) (not (memq (cadr l) syms)))
           (error 'infix-split "bad sequence near ~e in ~e" (car l) lst)]
          [else (let sub-loop ([sub (list (car l) (cadr l))] [l (cddr l)])
                  (if (or (null? l)
                          (and (not (null? (cdr l))) (memq (cadr l) syms)))
                    (loop l (cons (reverse sub) r))
                    (sub-loop (cons (car l) sub) (cdr l))))])))

;; Runs the above on all input from a given file.  The default is to add the
;; specs to *specs*.
(provide process-specs)
(define (process-specs input [param *specs*])
  (define-values (specs tags conds)
    (cond [(param) => (lambda (ls) (apply values (map reverse ls)))]
          [else (values '() '() '())]))
  (for-each
   (lambda (b)
     (define-syntax bind!
       (syntax-rules ()
         [(_ loc) (if (assq (cadr b) loc)
                    (error 'loc "got a second `~s' binding in ~s"
                           (cadr b) (list* (cadr b) (car b) (cddr b)))
                    (set! loc (cons (cdr b) loc)))]))
     (define-syntax change!
       (syntax-rules ()
         [(_ loc how)
          (cond [(assq (cadr b) loc)
                 => (lambda (cur)
                      (set! loc (cons (cons (car cur) (how (cdr cur)))
                                      (remq cur loc))))]
                [else (error 'loc "got a `~a' for nonexistent `~s' in ~s"
                             (car b) (cadr b)
                             (list* (cadr b) (car b) (cddr b)))])]))
     (define (appender x) (append x (cddr b)))
     (define (rebinder x) (@subst (cddr b) (cadr b) x))
     (case (car b)
       [(:=)      (bind! specs)]
       [(:=tag)   (bind! tags )]
       [(:=cond)  (bind! conds)]
       [(:=!)     (change! specs rebinder)]
       [(:=!tag)  (change! tags  rebinder)]
       [(:=!cond) (change! conds rebinder)]
       [(:+=)     (change! specs appender)]
       [(:+=tag)  (change! tags  appender)]
       [(:+=cond) (change! conds appender)]
       [else (error 'read-spec-file "something bad happened")]))
   (infix-split
    '(:= :=! :+= :=tag :+=tag :=!tag :=cond :+=cond :=!cond)
    input))
  (param (map reverse (list specs tags conds))))

;;; ===========================================================================
;;; Spec management

;; This holds a triplet of spec, tag, and cond definitions.
(provide *specs*)
(define *specs* (make-parameter #f))

(define (check-valid s) (void))

(provide register-spec!)
(define (register-spec! sym spec)
  (let ([specs (*specs*)])
    (check-valid specs)
    (*specs* (list (cons (list sym spec) (if specs (car specs) '()))
                   (if specs (cadr  specs) '())
                   (if specs (caddr specs) '())))))

(provide get-spec)
(define (get-spec spec)
  (let ([specs (*specs*)])
    (check-valid specs)
    (cond [(assq spec (car specs)) => cdr] [else #f])))
(provide get-tag)
(define (get-tag spec)
  (let ([specs (*specs*)])
    (check-valid specs)
    (cond [(assq spec (cadr specs)) => cdr] [else #f])))
;; no need for get-cond

;; The initial empty tag environment, so it is possible to start with a
;; different set of initial tags.
(provide *environment*)
(define *environment* (make-parameter '()))
;; If this is true, then definitions that are used in expansions are prepended
;; to the result.
(define *collect-definitions* (make-parameter #f))

;; Expanding specs is a little tricky: specs are always a list of things, which
;; means that definitions and macro expansions are always spliced at the usage
;; point.

;; Convenient syntax, similar to the `tag' spec form
(provide tag)
(define-syntax tag
  (syntax-rules ()
    [(_ tags body0 body ...)
     (let* ([ts tags]
            [ts (expand-tags (if (list? ts) ts (list ts)))])
       (parameterize ([*environment* (append (reverse ts) (*environment*))])
         body0 body ...))]))

;; Use this for splicing results into the original place a macro was used
(provide splice)
(define-values (splice spliced?)
  (let ([tag "splice"])
    (values (lambda (list) (cons tag list))
            (lambda (x) (and (pair? x) (eq? tag (car x)))))))

;; Handle cond expansion
;; spec -> spec-list, the input is always a cond spec
(define (expand-cond-spec spec)
  (define (eval-cond c)
    (define (bad-cond) (error 'expand-cond-spec "got a bad condition: ~.s" c))
    (cond [(eq? c 'else) #t]
          [(pair? c)
           (case (car c)
             [(and) (andmap eval-cond (cdr c))]
             [(or)  (ormap eval-cond (cdr c))]
             [(not) (if (= 1 (length (cdr c)))
                      (not (eval-cond (cadr c)))
                      (bad-cond))]
             [else (bad-cond)])]
          [else (member c (*environment*))]))
  (let loop ([clauses (infix-split '(=>) (cdr spec))])
    (cond [(null? clauses) '()]
          [(eval-cond (expand-conds (list (cadar clauses)))) (cddar clauses)]
          [else (loop (cdr clauses))])))

;; Expand usages of spec definitions, macros, and conds.
;; spec -> spec-list
(provide expand-spec)
(define (expand-spec spec)
  (cond [(and (symbol? spec) (get-spec spec)) => expand-specs]
        [(not (pair? spec)) (list spec)]
        [(eq? 'cond (car spec)) (expand-specs (expand-cond-spec spec))]
        [(eq? 'cond* (car spec))
         (expand-specs (map (lambda (cl) (list* 'cond (cadr cl) '=> (cddr cl)))
                            (infix-split '(=>) (cdr spec))))]
        [(eq? 'tag (car spec))
         (if (pair? (cdr spec))
           (tag (cadr spec) (expand-specs (cddr spec)))
           (error 'expand-spec "bad `tag' form: ~.s" spec))]
        [(eq? 'lambda (car spec))
         (if (pair? (cdr spec))
           (list (eval `(lambda ,(cadr spec)
                          (splice (list ,@(cddr spec))))))
           (error 'expand-spec "bad `lambda' form: ~.s" spec))]
        [(procedure? (car spec))
         (let ([newspec (apply (car spec) (expand-specs (cdr spec)))])
           (cond [(spliced? newspec) (expand-specs (cdr newspec))]
                 [(equal? newspec spec) (list spec)]
                 [else (expand-spec newspec)]))]
        [else
         (let ([newspec (append (expand-spec (car spec)) (cdr spec))])
           (cond [(null? newspec) newspec]
                 [(not (equal? spec newspec)) (expand-spec newspec)]
                 [else (list (cons (car spec) (expand-specs (cdr spec))))]))]))

;; spec-list -> spec-list
(provide expand-specs)
(define (expand-specs specs)
  (let ([newspecs (mappend expand-spec specs)])
    (cond [(equal? newspecs specs) specs]
          [(*collect-definitions*)
           (append specs (remove* specs (expand-specs newspecs)))]
          [else (expand-specs newspecs)])))

;; spec [tag ...] -> spec
(provide expand-spec-1)
(define (expand-spec-1 spec)
  (let ([r (expand-spec spec)])
    (if (= 1 (length r))
      (car r)
      (error 'expand-spec-1 "expected a single result for ~.s, but got ~e"
             spec r))))

;; Expand tags
(provide expand-tags)
(define (expand-tags tags)
  (check-valid (*specs*))
  (let ([tags (if (list? tags) tags (list tags))])
    (parameterize ([*specs* (let ([s (*specs*)])
                              (list (cadr s) (cadr s) (caddr s)))]
                   [*collect-definitions* #t])
      (expand-specs tags))))

;; Expand conditions
(define (expand-conds conds)
  (check-valid (*specs*))
  (let ([conds (if (list? conds) conds (list conds))])
    (parameterize ([*specs* (let ([s (*specs*)])
                              (list (caddr s) (cadr s) (caddr s)))])
      (let ([r (expand-specs conds)])
        (if (= 1 (length r))
          (car r)
          (error 'expand-conds "expected a single result for ~.s, but got ~e"
                 conds r))))))
