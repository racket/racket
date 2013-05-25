
; Test pretty-print.

;; Regression test results in pp-regression.rktd. When things
;;  have to change, re-run with `record-for-regression?' to #t.
(define record-for-regression? #f)
;; Disable `use-regression?' when inspecting results after a
;;  changed; when it's ok, then record the new regression results.
(define use-regression? #t)

(load-relative "loadtest.rktl")

(Section 'pretty)

(require racket/pretty
         racket/fixnum
         racket/flonum)

(print-as-expression #f)

(define (pprec-print pprec port write?)
  (define (print-one n port)
    ((if write? write display) (pprec-ref pprec n) port))
  (let ([pp? (and (pretty-printing)
		  (number? (pretty-print-columns)))])
    (write-string (if write? "W{" "D{") port)
    (let-values ([(l c p) (if pp?
			      (port-next-location port)
			      (values 0 0 0))])
      (print-one 0 port)
      (if pp?
	  (let* ([mx (- (pretty-print-columns) 1)]
		 [tp (make-tentative-pretty-print-output-port
		      port
		      mx
		      void)])
	    (write-string " " tp)
	    (print-one 1 tp)
	    (let-values ([(xl xc xp) (port-next-location tp)])
	      (if (xc . < . mx)
		  (tentative-pretty-print-port-transfer tp port)
		  (begin
		    (tentative-pretty-print-port-cancel tp)
		    (let ([cnt (pretty-print-newline port mx)])
		      (write-string (make-string (max 0 (- c cnt)) #\space) port))
		    (print-one 1 port)))))
	  (begin
	    (write-string " " port)
	    (print-one 1 port)))
      (write-string "}" port))))

(define-values (s:pprec make-pprec pprec? pprec-ref pprec-set!)
  (make-struct-type 'pprec #f 2 0 #f
		    (list (cons prop:custom-write pprec-print))))

(test "10" pretty-format 10)
(test "1/2" pretty-format 1/2)
(test "-1/2" pretty-format -1/2)
(test "1/2+3/4i" pretty-format 1/2+3/4i)
(test "0.333" pretty-format #i0.333)
(test "2.0+1.0i" pretty-format #i2+1i)
(test "'a" pretty-format ''a)
(test "`a" pretty-format '`a)
(test ",a" pretty-format ',a)
(test ",@a" pretty-format ',@a)
(test "#'a" pretty-format '#'a)
(test "W{1 2}" pretty-format (make-pprec 1 2))
(test "#&10" pretty-format (box 10))
(parameterize ([print-box #f])
  (test "#<box>" pretty-format (box 10)))
(test "#(10)" pretty-format (vector 10))
(test "#(10 10)" pretty-format (vector 10 10))
(test "#fl(10.0 10.0)" pretty-format (flvector 10.0 10.0))
(test "#fx(11 11)" pretty-format (fxvector 11 11))
(parameterize ([print-vector-length #t])
  (test "#1(10)" pretty-format (vector 10))
  (test "#2(10)" pretty-format (vector 10 10))
  (test "#2(10 20)" pretty-format (vector 10 20)))
(test "#hasheq((1 . 2))" pretty-format (let ([ht (make-hasheq)])
                                         (hash-set! ht 1 2)
                                         ht))
(test "#hash((1 . 2))" pretty-format (let ([ht (make-hash)])
                                       (hash-set! ht 1 2)
                                       ht))
(test "#hash((1 . 2))" pretty-format #hash((1 . 2)))
(test "#hasheq((1 . 2))" pretty-format #hasheq((1 . 2)))
(parameterize ([print-hash-table #f])
  (test "#<hash>" pretty-format (let ([ht (make-hasheq)])
                                  (hash-set! ht 1 2)
                                  ht)))

(parameterize ([pretty-print-abbreviate-read-macros #f])
  (test "(quote a)" pretty-format ''a)
  (test "(quasiquote a)" pretty-format '`a)
  (test "(unquote a)" pretty-format ',a)
  (test "(unquote-splicing a)" pretty-format ',@a)
  (test "(syntax a)" pretty-format '#'a))


(test "(1\n 2)" pretty-format '(1 2) 2)
(test "(1 2)" pretty-format '(1 2) 'infinity)

(parameterize ([print-hash-table #t])
  (test "#hasheq((1 . 2))" pretty-format (let ([ht (make-hasheq)])
                                       (hash-set! ht 1 2)
                                       ht)))

(test #t pretty-print-style-table? (pretty-print-current-style-table))
(test #t pretty-print-style-table? (pretty-print-extend-style-table #f null null))
(test #t pretty-print-style-table? (pretty-print-extend-style-table (pretty-print-current-style-table) null null))

(define (test-indent-variants pretty-format quote-abbrev?)
  (define-struct a (x y) #:transparent)
  (parameterize ([pretty-print-columns 20])
    (test "(1234567890 1 2 3 4)" pretty-format '(1234567890 1 2 3 4))
    (test "(1234567890xx\n 1\n 2\n 3\n 4)" pretty-format '(1234567890xx 1 2 3 4))
    (test "#(1234567890xx\n  1\n  2\n  3\n  4)" pretty-format (vector '1234567890xx 1 2 3 4))
    (test "#fx(123456789\n    1\n    2\n    3\n    4)" pretty-format (fxvector 123456789 1 2 3 4))
    (test "#fl(1234567890.0\n    1.0\n    2.0\n    3.0\n    4.0)" pretty-format (flvector 1234567890.0 1.0 2.0 3.0 4.0))
    (test "#s(apple\n   1234567890xx\n   1\n   2\n   3\n   4)" pretty-format #s(apple 1234567890xx 1 2 3 4))
    (test "#(struct:a\n  1234567890xx\n  1)" pretty-format (make-a '1234567890xx 1))
    (test "#hash((a\n       .\n       1234567890xx))" pretty-format #hash((a . 1234567890xx)))
    (test "(lambda 1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4))
    (test "(if 12345678903333\n  a\n  b)" pretty-format '(if 12345678903333 a b))
    (test "(cond\n (12345678903333 a)\n (else b))" pretty-format '(cond [12345678903333 a][else b]))
    (test "(case x\n  ((12345678903) a)\n  (else b))" pretty-format '(case x [(12345678903) a][else b]))
    (test "(and 12345678903333\n     a\n     b)" pretty-format '(and 12345678903333 a b))
    (test "(let ((x 12345678))\n  x)" pretty-format '(let ([x 12345678]) x))
    (test "(begin\n  1234567890\n  a\n  b)" pretty-format '(begin 1234567890 a b))
    (parameterize ([pretty-print-columns 35])
      (test "(letrec-syntaxes+values (((a) 1))\n                        (((b) 2))\n  c)"
            pretty-format 
            '(letrec-syntaxes+values ([(a) 1]) ([(b) 2]) c)))
    (test "(class object%\n  (define x 12)\n  (super-new))" pretty-format '(class object% (define x 12) (super-new)))
    (test "(syntax-case stx (a)\n  ((_ a) 10))" pretty-format '(syntax-case stx (a) [(_ a) 10]))
    (test "(make-object foo%\n  1234567890)" pretty-format '(make-object foo% 1234567890))
    (let ([table (pretty-print-extend-style-table #f null null)])
      (parameterize ([pretty-print-current-style-table 
                      (pretty-print-extend-style-table table '(lambda) '(list))])
        (test "(lambda\n 1234567890\n 1\n 2\n 3\n 4)" pretty-format '(lambda 1234567890 1 2 3 4)))
      (test "(lambda 1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4))
      (parameterize ([pretty-print-current-style-table table])
        (test "(lambda 1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4)))
      ;; Make sure special case for lambda, etc, doesn't hide sharing:
      (let ([a (read (open-input-string "#0=((x) 1 . #0#)"))])
        (test "(lambda\n .\n #0=((x) 1 . #0#))" pretty-format `(lambda . ,a)))
      (let ([a (read (open-input-string "#0=((1 . #0#))"))])
        (test "(quote\n .\n #0=((1 . #0#)))" pretty-format `(quote . ,a))
        (when quote-abbrev?
          (test "'#0=((1 . #0#))" pretty-format `(quote ,a)))))))
(test-indent-variants pretty-format #t)
(letrec ([to-mpair
          (lambda (s)
            (let ([ht (make-hasheq)])
              (let to-mpair ([s s])
                (if (pair? s)
                    (or (hash-ref ht s #f)
                        (let ([p (mcons #f #f)])
                          (hash-set! ht s p)
                          (set-mcar! p (to-mpair (car s)))
                          (set-mcdr! p (to-mpair (cdr s)))
                          p))
                    s))))])
  (parameterize ([print-mpair-curly-braces #f])
    (test-indent-variants (lambda (x)
                            (pretty-format (to-mpair x)))
                          #t))
  (test-indent-variants (lambda (x)
                          (regexp-replace* 
                           #rx"}"
                           (regexp-replace* 
                            #rx"{"
                            (pretty-format (to-mpair x))
                            "(")
                           ")"))
                        #f))

(parameterize ([pretty-print-columns 20]
               [print-as-expression #t])
  (test "(fxvector\n 123456789\n 1\n 2\n 3\n 4)" pretty-format (fxvector 123456789 1 2 3 4))
  (test "(flvector\n 1234567890.0\n 1.0\n 2.0\n 3.0\n 4.0)" pretty-format (flvector 1234567890.0 1.0 2.0 3.0 4.0)))

(parameterize ([pretty-print-exact-as-decimal #t])
  (test "10" pretty-format 10)
  (test "0.5" pretty-format 1/2)
  (test "-0.5" pretty-format -1/2)
  (test "3500.5" pretty-format 7001/2)
  (test "0.0001220703125" pretty-format 1/8192)
  (test "0.0000000000000006869768746897623487"
	pretty-format 6869768746897623487/10000000000000000000000000000000000)
  (test "0.00000000000001048576" pretty-format (/ (expt 5 20)))
  
  (test "1/3" pretty-format 1/3)
  (test "1/300000000000000000000000" pretty-format 1/300000000000000000000000)
  
  (test "0.5+0.75i" pretty-format 1/2+3/4i)
  (test "0.5-0.75i" pretty-format 1/2-3/4i)
  (test "1/9+3/17i" pretty-format 1/9+3/17i)
  (test "0.333" pretty-format #i0.333)
  (test "2.0+1.0i" pretty-format #i2+1i))

(let ()
  (define-struct wrap (content))
  (define (add-wrappers x)
    (let loop ([x x])
      (cond
        [(symbol? x) (make-wrap x)]
        [(pair? x) (cons (loop (car x))
                         (loop (cdr x)))]
        [else x])))
  (parameterize ([pretty-print-remap-stylable
                  (λ (x)
                    (and (wrap? x)
                         (wrap-content x)))]
                 [pretty-print-columns 6]
                 [pretty-print-size-hook
                  (λ (val dsp? port)
                    (if (wrap? val)
                        (string-length (format "~s" (wrap-content val)))
                        #f))]
                 [pretty-print-print-hook
                  (λ (val dsp? port)
                    (write (wrap-content val) port))])
    (test "(lambda (x)\n  abcdef)" pretty-format (add-wrappers '(lambda (x) abcdef)))
    (test "(call/cc\n call/cc)" pretty-format (add-wrappers '(call/cc call/cc)))))

(parameterize ([print-struct #t])
  (let ()
    (define-struct s (x) #:inspector (make-inspector))
    (test "#(struct:s 1)" pretty-format (make-s 1))))

(err/rt-test (pretty-print-extend-style-table 'ack '(a) '(b)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) 'a '(b)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(1) '(b)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(a) 'b))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(a) '(1)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(a) '(b c)) exn:application:mismatch?)

(define-struct s (a b c))

(define (make k?)
  (let ([make (if k? make (lambda (x) '(end)))])
    (list
     1
     'a
     "a"
     (list 'long-name-numero-uno-one-the-first-supreme-item
	   'long-name-number-two-di-ar-ge-second-line)
     (map (lambda (v v2)
	    (make-s v 2 v2))
	  (make #f)
	  (reverse (make #f)))
     '(1)
     '(1 2 3)
     '(1 . 2)
     (mcons 1 2)
     (mcons 1 (mcons 2 null))
     #(1 2 3 4 5)
     (fxvector 1 2 3)
     (flvector 1.0 2.0 3.0)
     (read (open-input-string "(#0=() . #0#)"))
     (read (open-input-string "#1=(1 . #1#)"))
     (read (open-input-string "#1={#0={1 2 . #2={#0# . #1#}} . #2#}"))
     (map box (make #f))
     (make #f)
     (make-pprec 1 2)
     (make-pprec 'long-name-numero-uno-one-the-first-supreme-item
		 'long-name-number-two-di-ar-ge-second-line)
     (let ([p (make-pprec "a" "c")])
       (pprec-set! p 0 p)
       p))))

(define vs (make #t))

(define print-line-no
  (lambda (line port offset width) 
    (if line         
	(begin
	  (when (positive? line) (write-char #\newline port))
	  (fprintf port "~s~a~a~a " line
		   (if (< line 10) " " "")
		   (if (< line 100) " " "")
		   (if (< line 1000) " " ""))
	  5)
	(fprintf port "!\n"))))

(define modes
  (list
   (list "DEPTH=2" pretty-print-depth 2)
   (list "GRAPH-ON" print-graph #t)
   (list "HASH-TABLE-ON" print-hash-table #t)
   (list "STRUCT-ON" print-struct #t)
   (list "LINE-NO-ON" pretty-print-print-line print-line-no)
   (list "SUPER-WIDE" pretty-print-columns 300)))

(define num-combinations (arithmetic-shift 1 (length modes)))

(define regression-path
  (build-path (current-load-relative-directory)
	      "pp-regression.rktd"))

(define recorded (if record-for-regression?
		     null
		     (with-input-from-file regression-path
		       read)))

(define (record-or-check id thunk)
  (if use-regression?
      (let ([str (let ([p (open-output-bytes)])
		   (parameterize ([current-output-port p])
		     (thunk))
		   (get-output-bytes p))])
	(if record-for-regression?
	    (set! recorded (cons (cons id str) recorded))
	    (test (cdr (assoc id recorded)) values str)))
      (thunk)))

(let loop ([n 0])
  (when (< n num-combinations)
    (record-or-check
     `(mode ,n)
     (lambda ()
       (let loop ([modes modes][n n])
	 (cond
	  [(null? modes) (printf ":\n") (map pretty-print vs)]
	  [(positive? (bitwise-and n 1))
	   (let ([mode (car modes)])
	     (printf "~s " (car mode))
	     (parameterize ([(cadr mode) (caddr mode)])
	       (loop (cdr modes) (arithmetic-shift n -1))))]
	  [else
	   (loop (cdr modes) (arithmetic-shift n -1))]))))
    (loop (add1 n))))

(when record-for-regression?
  (with-output-to-file regression-path
    #:exists 'truncate/replace
    (lambda () (write recorded))))

(test #t 'use-regression? use-regression?)

(print-as-expression #t)

;; ----------------------------------------

;; Test custom-write hooks through a re-implementation
;; of the transparent-struct printer:

(define (print-constructed port mode name . fields)
  (define (print-sub val port)
    (cond
     [(integer? mode) (print val port mode)]
     [mode (write val port)]
     [else (display val port)]))
  (display "(" port)
  (let-values ([(l c p) (port-next-location port)])
    (write name port)
    (if (not (pretty-printing))
        ;; Not pretty-printing --- simple
        (for ([val (in-list fields)])
          (display " " port)
          (print-sub val port))
        ;; Pretty-printing. First try simple:
        (unless ((let/ec esc
                   (letrec ([sub-port (make-tentative-pretty-print-output-port 
                                       port
                                       (pretty-print-columns)
                                       (lambda ()
                                         (esc (lambda ()
                                                (tentative-pretty-print-port-cancel sub-port)
                                                #f))))])
                     (for ([val (in-list fields)])
                       (display " " sub-port)
                       (print-sub val sub-port))
                     (lambda ()
                       (tentative-pretty-print-port-transfer sub-port port)
                       #t))))
          ;; Simple attempt overflowed the line, so print with newlines.
          (for ([val (in-list fields)])
            (pretty-print-newline port (pretty-print-columns))
            (let-values ([(l2 c2 p2) (port-next-location port)])
              (display (make-string (max 0 (- c c2)) #\space) port))
            (print-sub val port)))))
  (display ")" port))

(let ()
  (struct duo (a b)
    #:property prop:custom-print-quotable 'never
    #:property prop:custom-write (lambda (v port mode)
                                   (print-constructed port
                                                      mode 
                                                      'DUO
                                                      (duo-a v)
                                                      (duo-b v))))
  (let ([try-print
         (lambda (print v cols expect)
           (let ([s (open-output-string)])
             (parameterize ([pretty-print-columns cols])
               (print v s))
             (test expect get-output-string s)))])
    
    (try-print pretty-print 'a 40 "'a\n")
    (try-print pretty-print "a" 40 "\"a\"\n")

    (try-print pretty-print (duo 1 2) 40 "(DUO 1 2)\n")
    (try-print pretty-write (duo 1 2) 40 "(DUO 1 2)\n")
    (try-print pretty-display (duo 1 2) 40 "(DUO 1 2)\n")

    (try-print print (duo "a" 'b) 40 "(DUO \"a\" 'b)")
    (try-print pretty-print (duo "a" 'b) 40 "(DUO \"a\" 'b)\n")
    (try-print write (duo "a" 'b) 40 "(DUO \"a\" b)")
    (try-print pretty-write (duo "a" 'b) 40 "(DUO \"a\" b)\n")
    (try-print display (duo "a" 'b) 40 "(DUO a b)")
    (try-print pretty-display (duo "a" 'b) 40 "(DUO a b)\n")

    (try-print pretty-print (duo "abcdefghijklmno" 'b) 20 "(DUO\n \"abcdefghijklmno\"\n 'b)\n")
    (try-print pretty-write (duo "abcdefghijklmno" 'b) 20 "(DUO\n \"abcdefghijklmno\"\n b)\n")
    (try-print pretty-display (duo "abcdefghijklmno" 'b) 20 "(DUO\n abcdefghijklmno\n b)\n")

    (try-print pretty-print (list (duo "abcdefghijklmno" 'b)) 20 "(list\n (DUO\n  \"abcdefghijklmno\"\n  'b))\n")
    (try-print pretty-write (list (duo "abcdefghijklmno" 'b)) 20 "((DUO\n  \"abcdefghijklmno\"\n  b))\n")
    (try-print pretty-display (list (duo "abcdefghijklmno" 'b)) 20 "((DUO\n  abcdefghijklmno\n  b))\n")

    (let ([val (list (duo '(a b c d e)
                          '(1 2 3 4 5)))])
      (try-print pretty-print val 10 "(list\n (DUO\n  '(a\n    b\n    c\n    d\n    e)\n  '(1\n    2\n    3\n    4\n    5)))\n")
      (try-print pretty-write val 10 "((DUO\n  (a\n   b\n   c\n   d\n   e)\n  (1\n   2\n   3\n   4\n   5)))\n")
      (try-print pretty-display val 10 "((DUO\n  (a\n   b\n   c\n   d\n   e)\n  (1\n   2\n   3\n   4\n   5)))\n"))))

;; ----------------------------------------

;; make sure pretty printer isn't confised about
;; quasiquote
(let ([p (open-output-string)])
  (pretty-print '(quote ,x ,4) p)
  (test "'(quote ,x ,4)\n" get-output-string p))
(let ([p (open-output-string)])
  (pretty-print '```(quote ,,,,,x ,4) p)
  (test "'```(quote ,,,,,x ,4)\n" get-output-string p))

;; ----------------------------------------

(parameterize ([print-boolean-long-form #f])
  (test "#t" pretty-format #t)
  (test "#f" pretty-format #f))
(parameterize ([print-boolean-long-form #t])
  (test "#true" pretty-format #t)
  (test "#false" pretty-format #f))

;; ----------------------------------------

(report-errs)
