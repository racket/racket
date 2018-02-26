#lang at-exp racket/base
(require racket/flonum
         (rename-in "main.rkt"
                    [read main:read]
                    [read-language main:read-language]))

(define (s->p . strs)
  (define p (open-input-string (apply string-append strs)))
  (port-count-lines! p)
  p)

(define test-read
  (case-lambda
    [(in) (main:read in #:source "input")]
    [(in expect)
     (define v (test-read in))
     (unless (equal? v expect)
       (error 'test "fail\n  got: ~s\n  expect: ~s"
              v
              expect))
     v]))

(test-read (s->p "#:a")
           '#:a)
(test-read (s->p "#\\a")
           #\a)
(test-read (s->p "#\\110")
           #\H)
(test-read (s->p "#\\u0001")
           #\u1)
(test-read (s->p "#\\U3BB")
           #\u3BB)
(test-read (s->p "#\\\u3BB")
           #\u3BB)
(test-read (s->p "|ap ple|Pie")
           '|ap plePie|)
(test-read (s->p "\\|\\|")
           '\|\|)
(test-read (s->p "(a b #%c)")
           '(a b #%c))
(test-read (s->p "(a #;z b . c)")
           '(a b . c))
(parameterize ([read-cdot #t])
  (test-read (s->p "(a b . c)")
             '(a (#%dot b c))))
(parameterize ([read-cdot #t])
  (test-read (s->p "a.b.c.d|.|f")
             '(#%dot (#%dot (#%dot a b) c) d.f)))
(test-read (s->p "(b . a . c)")
           '(a b c))
(test-read (s->p "(b . a #| a |# . c)")
           '(a b c))
(test-read (s->p "(a 1.0 ; comment\n c)")
           '(a 1.0 c))
(test-read (s->p "(a \"1.0\" c)")
           '(a "1.0" c))
(test-read (s->p "'('a `b ,c ,@d ,@ e #'f #`g #,h #,@i)")
           ''('a `b ,c ,@d ,@e #'f #`g #,h #,@i))
(test-read (s->p "(#t)")
           '(#t))
(test-read (s->p "#f")
           '#f)
(test-read (s->p "(#true)")
           '(#t))
(test-read (s->p "#ci (#false)")
           '(#f))
(test-read (s->p "#005(fAl Se)")
           '#(fAl Se Se Se Se))
(test-read (s->p "#fl6(1.5 0.33 0.3)")
           (flvector 1.5 0.33 0.3 0.3 0.3 0.3))
(let ([ht (test-read (s->p "#1=#hasheq((#1# . #1#))"))])
  (unless (eq? (hash-ref ht ht) ht)
    (error 'test "fail for cyclic hash table")))
(test-read (s->p "#hash{(fAl . Se) (7 . 9)}")
           #hash{(fAl . Se) (7 . 9)})
(test-read (s->p "#hasheq()")
           #hasheq())
(test-read (s->p "#s(fAl Se)")
           #s(fAl Se))
(test-read (s->p "#&fox")
           #&fox)
(test-read @s->p{#px#"fox"}
           #px#"fox")
(test-read (s->p "{fAl Se}")
           '(fAl Se))
(test-read (s->p "#{fAl Se}")
           '#(fAl Se))
(test-read (s->p "#! ok \\\n more\n 8")
           8)
(test-read @s->p{"apple\n\"\x30\7\07\u3BB\U1F600\uD83D\uDE00"}
           "apple\n\"\x30\7\07\u3BB\U1F600\U1F600")
(test-read @s->p{#"apple\n\"\x30\7\07"}
           #"apple\n\"0\a\a")
(test-read @s->p{#<<HERE
                This is text and
                HERE we go
                to finish the text
                HERE
                not included}
           "This is text and\nHERE we go\nto finish the text\n")
(parameterize ([read-curly-brace-with-tag #t])
  (test-read (s->p "{fAl Se}")
             '(#%braces fAl Se)))
(parameterize ([read-case-sensitive #f])
  (test-read (s->p "Case\\InSens")
             'caseInsens))
(with-handlers ([exn:fail:read? exn-message])
  (test-read (s->p "{  fAl\n Se)")))

(parameterize ([current-readtable (make-readtable #f
                                                  #\$ #\( #f
                                                  #\% #\) #f)])
  (test-read (s->p "$inside%")
             '(inside)))
(parameterize ([current-readtable (make-readtable #f
                                                  #\t 'terminating-macro
                                                  (lambda (a b c d e f) 'TEE)
                                                  #\u 'non-terminating-macro
                                                  (lambda (a b c d e f) 'YOO))])
  (test-read (s->p "(1t2u3)")
             '(1 TEE 2u3)))
(parameterize ([current-readtable (make-readtable #f
                                                  #\t 'dispatch-macro
                                                  (lambda (a b c d e f) 'TEE))])
  (test-read (s->p "(1 #t 2)")
             '(1 TEE 2)))
(parameterize ([current-readtable (make-readtable #f
                                                  #\t 'dispatch-macro
                                                  (lambda (c in src long col pos)
                                                    (unless (equal? c #\t)
                                                      (error "not the expected character"))
                                                    (main:read in
                                                               #:recursive? #t
                                                               #:readtable #f)))])
  (test-read (s->p "(#1=(a) #t #1# #t#t)")
             '((a) (a) #t)))
(parameterize ([read-accept-reader #t])
  (main:read (s->p "#readerok") #:dynamic-require (lambda (lib sym)
                                                    (lambda (in src line col pos)
                                                      'OK))))
(parameterize ([read-accept-reader #t])
  (main:read (s->p "#lang ok ?")
             #:dynamic-require (lambda (lib sym)
                                 (lambda (in src line col pos)
                                   'LANG-OK))
             #:module-declared? (lambda (mp) #f)))
(parameterize ([read-accept-reader #t])
  (main:read (s->p "#!ok ?")
             #:dynamic-require (lambda (lib sym)
                                 (lambda (in)
                                   '|#!-OK|))
             #:module-declared? (lambda (mp) #t)))

(main:read-language (s->p "#lang racket/base") (lambda () (error "fail"))
                    #:dynamic-require (lambda (lib sym fail-k)
                                        (lambda (in src line col pos)
                                          (lambda (x y) 'LANG-INFO)))
                    #:module-declared? (lambda (mp) #f))

(parameterize ([current-readtable (make-readtable #f
                                                  #\# #\a #f)])
  (test-read (s->p "#ab#")
             '|#ab#|))

(with-handlers ([exn:fail:read? exn-message])
  (parameterize ([current-readtable (make-readtable #f
                                                    #\* #\) #f
                                                    #\! #\) #f)])
    (main:read (s->p "(x"))))

(define s (let ([o (open-output-bytes)])
            (display "(" o)
            (for ([i 100000]) (display " " o) (display i o))
            (display ")" o)
            (get-output-string o)))
(collect-garbage)
(require "accum-string.rkt"
         "config.rkt")
(void (time (let ([p (s->p s)])
              ;; Sortof a baseline measurement:
              (define accum-str (accum-string-init! (make-read-config)))
              (let loop ([v #f])
                (unless (eof-object? (peek-char-or-special p))
                  (loop (accum-string-add! accum-str (read-char-or-special p))))))))
(void (time (test-read (s->p s))))
(void (time (read (s->p s))))
