(load-relative "loadtest.rktl")

(require syntax/id-table
         racket/dict)
  
(Section 'id-table)

(test #t bound-id-table? (make-bound-id-table))
(test #t bound-id-table? (make-immutable-bound-id-table))

(test #t mutable-bound-id-table? (make-bound-id-table))
(test #t immutable-bound-id-table? (make-immutable-bound-id-table))

(let ()
  (define a #'a)
  (define b #'b)
  (define b2 (let ([b 0]) #'b))
  (define b3 ((make-syntax-introducer) #'b)) ;; free=? to b
  (define alist (list (cons a 1) (cons b 2) (cons b2 3) (cons b3 4)))
  (test 4 bound-id-table-count (make-bound-id-table alist))
  (test 4 bound-id-table-count (make-immutable-bound-id-table alist))
  (test 3 free-id-table-count (make-free-id-table alist))
  (test 3 free-id-table-count (make-immutable-free-id-table alist))

  (let ()
    ;; Test in-dict, iteration methods for immutable id-tables
    (define d1 (make-immutable-bound-id-table alist))
    (test (+ 1 2 3 4) (lambda () (for/sum ([(id v) (in-dict d1)]) v)))
    (define d2 (for/fold ([d (make-immutable-bound-id-table)])
                   ([(id v) (in-dict d1)])
                 (dict-set d id (add1 v))))
    (test 2 bound-id-table-ref d2 a)
    (test 3 bound-id-table-ref d2 b)
    (test 4 bound-id-table-ref d2 b2)
    (test 5 bound-id-table-ref d2 b3))

  (let ()
    ;; Test in-dict, iteration methods for mutable id-tables
    ;; In particular, test that -set! of *existing* key does not disrupt iter.
    (define d1 (make-bound-id-table alist))
    (test (+ 1 2 3 4) (lambda () (for/sum ([(id v) (in-dict d1)]) v)))
    (for ([(id v) (in-dict d1)])
      (bound-id-table-set! d1 id (add1 v)))
    (test (+ 2 3 4 5) (lambda () (for/sum ([(id v) (in-dict d1)]) v)))))

(let ()
  ;; contains-same? : (listof x) (listof x) -> boolean
  (define (contains-same? l1 l2)
    (and (andmap (lambda (x) (member x l2)) l1)
         (andmap (lambda (x) (member x l1)) l2)
         #t))

  (let-values ([(x1 x2 x3 x4)
                (syntax-case (expand #'((lambda (x) x) (lambda (x) x))) ()
                  [(x (a (x1) x2) (c (x3) x4))
                   (values (syntax x1)
                           (syntax x2)
                           (syntax x3)
                           (syntax x4))])])

    (let ([check (lambda (=?)
                   (test #t =? x1 x2)
                   (test #t =? x3 x4)
                   (when (=? x1 x3)
                     ((current-print) "huh!?"))
                   (test #f =? x1 x3)
                   (test #f =? x1 x4)
                   (test #f =? x2 x3)
                   (test #f =? x2 x4))])
      (check bound-identifier=?)
      (check free-identifier=?))

    (let ([table (make-bound-id-table)])
      (bound-id-table-set! table x1 #f)
      (test #f bound-id-table-ref table x1)

      (bound-id-table-set! table x1 1)
      (bound-id-table-set! table x2 2)
      (bound-id-table-set! table x3 3)
      (bound-id-table-set! table x4 4)
      (test 2 bound-id-table-ref table x1)
      (test 2 bound-id-table-ref table x2)
      (test 4 bound-id-table-ref table x3)
      (test 4 bound-id-table-ref table x4)
      (test #t
            contains-same?
            (list 2 4)
            (bound-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))

      (test 2 bound-id-table-count table)

      
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (bound-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l)))


    (let ([table (make-free-id-table)])
      (free-id-table-set! table x1 1)
      (free-id-table-set! table x2 2)
      (free-id-table-set! table x3 3)
      (free-id-table-set! table x4 4)
      (test 2 free-id-table-ref table x1)
      (test 2 free-id-table-ref table x2)
      (test 4 free-id-table-ref table x3)
      (test 4 free-id-table-ref table x4)

      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))
      (test 2 free-id-table-count table)

      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (free-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))))

  (let-values ([(y1 y2 y3 y4)
                (syntax-case (expand #'(module m mzscheme (require (prefix x: mzscheme)) + x:+ - x:-)) ()
                  [(a b c (d e f y1 y2 y3 y4))
                   (values (syntax y1)
                           (syntax y2)
                           (syntax y3)
                           (syntax y4))])])

    (let ([table (make-bound-id-table)])
      (bound-id-table-set! table y1 1)
      (bound-id-table-set! table y2 2)
      (bound-id-table-set! table y3 3)
      (bound-id-table-set! table y4 4)
      (test 1 bound-id-table-ref table y1)
      (test 2 bound-id-table-ref table y2)
      (test 3 bound-id-table-ref table y3)
      (test 4 bound-id-table-ref table y4)

      (test #t
            contains-same?
            (list 1 2 3 4)
            (bound-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (dict-map table (lambda (x y) y)))
      (test 4 bound-id-table-count table)

      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (bound-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l)))

    (let ([table (make-free-id-table)])
      (free-id-table-set! table y1 #f)
      (test #f free-id-table-ref table y1)

      (free-id-table-set! table y1 1)
      (free-id-table-set! table y2 2)
      (free-id-table-set! table y3 3)
      (free-id-table-set! table y4 4)
      (test 2 free-id-table-ref table y1)
      (test 2 free-id-table-ref table y2)
      (test 4 free-id-table-ref table y3)
      (test 4 free-id-table-ref table y4)

      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))
      (test 2 free-id-table-count table)

      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (free-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      ))

  (test #t contract? (free-id-table/c any/c number?))
  (test #t contract? (bound-id-table/c any/c number?))
  (test #t contract? (bound-id-table/c any/c number? #:immutable #t))
  (test #t contract? (free-id-table/c any/c number? #:immutable #f))
  (test #t contract? (bound-id-table/c any/c number? #:immutable 'dont-care))

  (test #t chaperone-contract? (free-id-table/c any/c number?))
  (test #f flat-contract? (free-id-table/c any/c number?))
  (test #t flat-contract? (free-id-table/c any/c number? #:immutable #t))
  (test #f flat-contract? (free-id-table/c any/c (vectorof number?) #:immutable #t))
;;  --- ryanc: chaperone contracts only for now
;;  (test #f chaperone-contract? (free-id-table/c any/c (new-∀/c 'v)))
;;  (error-test #'(free-id-table/c (new-∀/c 'v) any/c))

  (let ()

    (define (app-ctc ctc value)
      (contract ctc value 'positive 'negative))

    (define (positive-error? exn)
      (and exn:fail:contract?
           (regexp-match? "blaming: positive" (exn-message exn))))
    (define (negative-error? exn)
      (and exn:fail:contract?
           (regexp-match? "blaming: negative" (exn-message exn))))

    (define-syntax-rule (test/blame-pos e)
     (thunk-error-test (lambda () e) #'e positive-error?))
    (define-syntax-rule (test/blame-neg e)
     (thunk-error-test (lambda () e) #'e negative-error?))

    (define-values (a b c d) (values 'A 'B 'C 'D))
    (define tbl (make-free-id-table))
    (free-id-table-set! tbl #'a a)
    (free-id-table-set! tbl #'b b)
    (free-id-table-set! tbl #'c c)
    (free-id-table-set! tbl #'d d)

    (define im-tbl
      ((compose
         (lambda (tbl) (free-id-table-set tbl #'a a))
         (lambda (tbl) (free-id-table-set tbl #'b b))
         (lambda (tbl) (free-id-table-set tbl #'c c))
         (lambda (tbl) (free-id-table-set tbl #'d d)))
       (make-immutable-free-id-table)))


    (test #t free-id-table? (app-ctc (free-id-table/c any/c any/c) (make-free-id-table)))
    (test #t bound-id-table? (app-ctc (bound-id-table/c identifier? number?) (make-bound-id-table)))
    (test #t free-id-table? (app-ctc (free-id-table/c identifier? symbol?) tbl))
    (test #t free-id-table? (app-ctc (free-id-table/c identifier? symbol?) im-tbl))
    (test #t free-id-table? (app-ctc (free-id-table/c identifier? number?) tbl))

    (test/blame-pos (app-ctc (free-id-table/c symbol? symbol? #:immutable #t) im-tbl))
    (test/blame-pos (app-ctc (free-id-table/c identifier? number? #:immutable #t) im-tbl))

    (test/blame-pos (app-ctc (free-id-table/c identifier? number?) im-tbl))
    (test/blame-pos (app-ctc (free-id-table/c symbol? symbol?) im-tbl))

    (define (short-identifier? id)
      (and (identifier? id) (equal? 1 (string-length (symbol->string (syntax-e id))))))
    (define ctced-tbl (app-ctc (free-id-table/c short-identifier? number?) tbl))
    (test/blame-pos (free-id-table-ref ctced-tbl #'a))
    (test/blame-neg (free-id-table-ref ctced-tbl #'ab))
    (test/blame-neg (free-id-table-set! ctced-tbl #'a 'c))
    (test/blame-neg (free-id-table-set! ctced-tbl #'ab 2))
    (test/blame-neg (free-id-table-remove! ctced-tbl #'ab))

    (test/blame-pos
      (let ((ctced-tbl (app-ctc (free-id-table/c symbol? number?) tbl)))
        (free-id-table-iterate-key ctced-tbl (free-id-table-iterate-first ctced-tbl))))
    
    (define/contract ctc-tbl (free-id-table/c any/c number?) (make-free-id-table))
    (test #t void? (free-id-table-set! ctc-tbl #'a 1))
    (test #t void? (free-id-table-set! ctc-tbl #'b 2))
    (test #t number? (free-id-table-ref ctc-tbl #'b))
    (test #t string? (free-id-table-ref ctc-tbl #'c "3"))
    (test #t void? (free-id-table-set! ctc-tbl #'a 4))
    (test #t void? (free-id-table-remove! ctc-tbl #'b))
    (test #t number? (free-id-table-count ctc-tbl))
    (test #t list? (free-id-table-map ctc-tbl (λ (k v) v)))


    ))

(let ()
  (define-syntax name 'dummy)
  (define-syntax alias (make-rename-transformer #'name))
  (define table (make-free-id-table))
  (free-id-table-set! table #'alias 0)
  (test 0 free-id-table-ref table #'name))

(report-errs)
