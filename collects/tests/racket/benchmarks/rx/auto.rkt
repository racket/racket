#!/bin/sh
#|
exec racket -qu "$0" ${1+"$@"}
|#

(module auto mzscheme
  (require mzlib/process
           mzlib/port
           mzlib/list
           mzlib/date
           "../common/cmdline.rkt")

  ;; Needed for rxmzold, comment out otherwise:
  (begin
    (define pregexp regexp)
    (define byte-pregexp byte-regexp))

  (define (test-mz input rx iterations)
    (let ([start (current-inexact-milliseconds)])
      (let loop ([n iterations])
	(unless (zero? n)
	  (regexp-match-positions rx input)
	  (loop (sub1 n))))
      (- (current-inexact-milliseconds) start)))

  (define (test-racket input rx iterations)
    (test-mz input (byte-pregexp rx) iterations))

  (define (test-mzunicode input rx iterations)
    ;; Note: input is left as bytes, to avoid overhead of UTF-8
    ;;  conversion.
    (test-mz input
	     (pregexp (bytes->string/latin-1 rx))
	     iterations))

  (define (test-rxmzold input rx iterations)
    (let ([rx (if (regexp-match #rx#"[(][?]s:" rx)
		  (subbytes rx 4 (- (bytes-length rx) 1))
		  rx)])
      (test-mz input (byte-pregexp rx) iterations)))

  (define (encode-newlines rx)
    (regexp-replace* #rx#"\n" rx #"\\\\n"))

  (define (test-perl input rx iterations)
    (with-output-to-file "test.pl"
      (lambda ()
        (with-input-from-file "perl_prefix.pl"
          (lambda ()
            (copy-port (current-input-port)
                       (current-output-port))))
        (printf "test \"~a\", qr/~a/, ~a;\n"
                (encode-newlines input) rx iterations))
      'truncate)
    (let ([s (open-output-bytes)])
      (parameterize ([current-output-port s])
        (system "perl test.pl"))
      (parameterize ([current-input-port (open-input-string (get-output-string s))])
        (* 1000 (read)))))

  (define (pythonize rx)
    ;; Python doesn't recognize (?i:...), so we convert to (?:(?i)...)
    (regexp-replace* #rx#"[(][?]([ims]):" rx #"(?:(?\\1)"))

  (define (test-python input rx iterations)
    (with-output-to-file "test.py"
      (lambda ()
        (with-input-from-file "python_prefix.py"
          (lambda ()
            (copy-port (current-input-port)
                       (current-output-port))))
        (printf "test(~s, \"~a\", ~a)\n"
                (bytes->string/latin-1 (pythonize rx)) (encode-newlines input) iterations))
      'truncate)
    (let ([s (open-output-bytes)])
      (parameterize ([current-output-port s])
        (system "python test.py"))
      (parameterize ([current-input-port (open-input-string (get-output-string s))])
        (* 1000 (read)))))

  (define (test-pcre input rx iterations)
    (let ([pcregexp (dynamic-require "pcre.rkt" 'pcregexp)]
	  [pcregexp-match (dynamic-require "pcre.rkt" 'pcregexp-match)])
      (let ([rx (pcregexp rx)])
	(let ([start (current-inexact-milliseconds)])
	  (let loop ([n iterations])
	    (unless (zero? n)
	      (pcregexp-match rx input)
	      (loop (sub1 n))))
	  (- (current-inexact-milliseconds) start)))))

  (define (random-letters n)
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 43)
      (let loop ([n n][accum null])
        (if (zero? n)
            (list->bytes accum)
            (loop (sub1 n) (cons (+ (char->integer #\a)
                                    (random 26))
                                 accum))))))

  (define (add-index l)
    (let loop ([i 1][j 1][l l])
      (if (null? l)
          l
	  (let ([next-is-diff? 
		 (or (null? (cdr l))
		     (not (eq? (caar l) (caadr l))))])
	    (cons (list* (if (and (= i 1) next-is-diff?)
			     (caar l)
			     (string->symbol (format "~a~a" (caar l) i)))
			 (let ([n (format "00~a" j)])
			   (substring n (- (string-length n) 3)))
			 (cdar l))
		  (loop (if next-is-diff? 1 (add1 i))
			(add1 j)
			(cdr l)))))))

  (define dna-str (bytes-append
		   #"DYttctatagWcRttatYSagacttcaScaacaYtSaattcaaagtgctSaagctattaa"
		   #"agggtaactcacgDKgttaMRgggattttMcgttattaVtccaNagaRgMaataaagaSt"
		   #"agggtaactcacgDKgttaMRgggattttMcgttattaVtccaNagaRgMaataaagaSt"))
  (define dna-up-str (string->bytes/latin-1
		      (string-upcase
		       (bytes->string/latin-1 dna-str))))

  (define inputs
    (add-index
     (list
      (list 'overhead #"x"  #"." 100000 '())
      (list 'overhead #"x"  #"." 1000000 '())
      (list 'overhead #"x"  #"y" 100000 '())
      (list 'overhead #"x"  #"y" 1000000 '())
      (list 'simple #"track1.title:TBlah blah blah" #"([^.]*)\\.([^:]*):[T ]+(.*)" 100000 '())
      (list 'section #"<body><h2><index><_index>8.9.10 A <b><i>very<_i> good<_b> section<_h2><p>Hi.<_p><_body>"
	    #"<h[2-9]>(<[^>]*>)*[0-9.]+(.*)<_h[2-9]>" 100000 '())
      (list 'dna dna-str #"agggtaa[cgt]|[acg]ttaccct" 100000 '())
      (list 'dna dna-str #"[aA][gG][gG][gG][tT][aA][aA][cCgGtT]|[aAcCgG][tT][tT][aA][cC][cC][cC][tT]" 100000 '())
      (list 'dna dna-str #"(?i:agggtaa[cgt]|[acg]ttaccct)" 100000 '(rxmzold))
      (list 'dna dna-up-str #"(?i:agggtaa[cgt]|[acg]ttaccct)" 100000 '(rxmzold))
      (list 'far (bytes-append (make-bytes 10000 (char->integer #\a)) #"z") #"z" 10000 '())
      (list 'farzq (bytes-append (make-bytes 1000 (char->integer #\a)) #"z") #"[zq]" 100000 '())
      (list 'farzqalt (bytes-append (make-bytes 1000 (char->integer #\a)) #"z") #"z|q" 100000 '(perl))
      (list 'fard (bytes-append (make-bytes 1000 (char->integer #\a)) #"0") #"\\d" 100000 '())
      (list 'farw (bytes-append (make-bytes 1000 (char->integer #\space)) #"_") #"\\w" 100000 '())
      (list 'farnz (bytes-append (make-bytes 10000 (char->integer #\a)) #"z") #"(?:)z" 1000 '())
      (list 'farbol (bytes-append (make-bytes 100 (char->integer #\x)) #"\na") #"(?m:^a)" 100000 '(rxmzold))
      (list 'farlook (bytes-append (make-bytes 100 (char->integer #\x)) #"a") #"(?=a)a" 100000 '(rxmzold))
      (list 'farlooknot (bytes-append (make-bytes 100 (char->integer #\x)) #"a") #"(?!b)a" 100000 '(rxmzold))
      (list 'farlookb (bytes-append (make-bytes 100 (char->integer #\x)) #"a") #"(?<=x)a" 100000 '(rxmzold))
      (list 'farlookbnot (bytes-append (make-bytes 100 (char->integer #\x)) #"a") #"(?<!y)a" 100000 '(rxmzold))
      (list 'farempty (bytes-append (make-bytes 100 (char->integer #\x)) #"a") #"()a" 100000 '())
      (list 'farsub (bytes-append (make-bytes 100 (char->integer #\x)) #"a") #"(a)" 100000 '())
      (list 'faraa (bytes-append (make-bytes 100 (char->integer #\x)) #"aa") #"(aa)" 100000 '())
      (list 'farcut (bytes-append (make-bytes 100 (char->integer #\x)) #"ab") #"(?>a)b" 100000 '(rxmzold python))
      (list 'fareol (bytes-append (make-bytes 100 (char->integer #\x)) #"\n") #"(?m:$[^x])" 100000 '(rxmzold))
      (list 'past (bytes-append (make-bytes 10000 (char->integer #\a)) #"z") #"a*z" 10000 '())
      (list 'pastab (bytes-append (make-bytes 1000 (char->integer #\a)) #"z") #"[ab]*z" 100000 '())
      (list 'pastabalt (bytes-append (make-bytes 1000 (char->integer #\a)) #"z") #"(?:a|b)*z" 100000 '(perl))
      (list 'pastd (bytes-append (make-bytes 1000 (char->integer #\0)) #"z") #"\\d*z" 100000 '())
      (list 'pastw (bytes-append (make-bytes 1000 (char->integer #\_)) #":") #"\\w*:" 100000 '())
      (list 'pastna (bytes-append (make-bytes 10000 (char->integer #\a)) #"z") #"(?:(?:)a)*z" 1000 '(python))
      (list 'backtrack (bytes-append (make-bytes 300 (char->integer #\a)) #"zay") #"a*y" 1000 '())
      (list 'backtrack (bytes-append (make-bytes 300 (char->integer #\a)) #"zay") #"a*?z" 10000 '())
      (list 'backtrack (bytes-append (make-bytes 300 (char->integer #\a)) #"zay") #"a*?y" 1000 '())
      (list 'alts #"cataract23" #"(cat(a(ract|tonic)|erpillar))2(3)" 100000 '())
      (list 'alts #"cataract23" #"(?:cat(?:a(?:ract|tonic)|erpillar))23" 100000 '())
      (list 'alts #"caterpillar23" #"(?:cat(?:a(?:ract|tonic)|erpillar))23" 100000 '())
      (list 'alts #"cataract23" #"(?i:(?:cat(?:a(?:ract|tonic)|erpillar)))23" 100000 '(rxmzold))
      (list 'backref #"cataract cataract23" #"(cat(a(ract|tonic)|erpillar)) \\1()2(3)" 100000 '(rxmzold))
      (list 'backref #"cataract cataract23" #"(cat(?:a(?:ract|tonic)|erpillar)) \\1(?:)23" 100000 '(rxmzold))
      (list 'backref #"caterpillar caterpillar23" #"(cat(?:a(?:ract|tonic)|erpillar)) \\1(?:)23" 100000 '(rxmzold))
      (list 'backref #"cataract cataract23" #"(?i:(cat(?:a(?:ract|tonic)|erpillar))) \\1(?:)23" 100000 '(rxmzold))
      (list 'digits #"From abcd  Mon Sep  1 12:33:02 1997" 
	    #"^From\\s+\\S+\\s+([a-zA-Z]{3}\\s+){2}\\d{1,2}\\s+\\d\\d:\\d\\d" 100000 '(rxmzold))
      (list 'digits #"From abcd  Sep 01 12:33:02 1997" 
	    #"^From\\s+\\S+\\s+([a-zA-Z]{3}\\s+){2}\\d{1,2}\\s+\\d\\d:\\d\\d"  100000 '(rxmzold))
      (list 'lines (bytes-append (make-bytes 100 (char->integer #\x)) #"\na\nb\nc\nxxxxxxxx") 
	    #"(?m:^a(?:$)[^a]^b(?:$)[^a]^c(?:$))" 100000 '(rxmzold))
      (list 'lines (bytes-append (make-bytes 100 (char->integer #\newline)) #"\na\nb\nc\nxxxxxxxx") 
	    #"(?m:^a(?:$)[^a]^b(?:$)[^a]^c(?:$))" 100000 '(rxmzold))
      (list 'lookahead #"foobar is foolish see?" #"foo(?!bar).*" 100000 '(rxmzold))
      (list 'lookahead #"foobar crowbar etc" #"(?:(?!foo)...|^.{0,2})bar.*" 100000 '(rxmzold))
      (list 'lookbehind #"foobar is foolish see?" #"(?<=foo)lish.*" 100000 '(rxmzold))
      (list 'lookbehind #"foobar crowbar etc" #"(?<=bar ....)bar.*" 100000 '(rxmzold))
      (list 'cond (bytes-append #"E" (make-bytes 100 (char->integer #\x))) #"^(E)?(?(1)xx|x)*$" 10000 '(python rxmzold))
      (list 'cond (bytes-append #"E" (make-bytes 101 (char->integer #\x))) #"^(E)?(?(1)xx|x)*$" 10000 '(python rxmzold))
      (list 'cond (make-bytes 101 (char->integer #\x)) #"^(E)?(?(1)xx|x)*$" 10000 '(python rxmzold))
      (list 'cut #"now is the time for all good men to come to the aid of the party" 
	    #"^((?>\\w+)|(?>\\s+))*$" 30000 '(python rxmzold))
      (list 'cut #"this is not a line with only words and spaces!" #"^((?>\\w+)|(?>\\s+))*$" 30000 '(python rxmzold))
      (list 'escape #"yesBABthe AAABquickAAAB brown foxABB" #"yesB([^AB]+|A.)*B" 10000 '())
      (list 'escape #"noBABthe AAABquickAAAB brown foxAB" #"noB([^AB]+|A.)*B" 10 '())
      (list 'escape #"yesBABthe AAABquickAAAB brown foxABB" #"yesB(?:[^AB]+|A.)*B" 10000 '())
      (list 'escape #"noBABthe AAABquickAAAB brown foxAB" #"noB(?:[^AB]+|A.)*B" 10 '())
      (list 'escape #"yesbabthe aaabquickaaab frown foxabb" #"(?i:yesB(?:[^AB]+|A.)*B)" 100000 '(rxmzold))
      (list 'escape #"nobabthe aaabquickaaab frown foxab" #"(?i:noB(?:[^AB]+|A.)*B)" 10 '(rxmzold))
      (list 'backtrack1 (make-bytes 1000 (char->integer #\x)) #".*z" 100 '())
      (list 'backtrack1z (bytes-append #"z" (make-bytes 1000 (char->integer #\x))) #".*z" 100 '())
      (list 'backtrack2 (make-bytes 1000 (char->integer #\x))  #".*(z)" 100 '())
      (list 'backtrack2z (bytes-append #"z" (make-bytes 1000 (char->integer #\x)))  #".*(z)" 100 '())
      (list 'backtrack3 (make-bytes 1000 (char->integer #\x))  #".*(?:z)" 100 '())
      (list 'backtrack3z (bytes-append #"z" (make-bytes 1000 (char->integer #\x)))  #".*(?:z)" 100 '())
      (list 'stress-same (make-bytes 1000 (char->integer #\x)) #"^(x|a)\\1*x$" 1000 '(rxmzold))
      (list 'stress-same (make-bytes 1000 (char->integer #\x)) #"^(x*|a)\\1x$" 1000 '(rxmzold))
      (list 'stress-same (make-bytes 1000 (char->integer #\x)) #"^(x*?|a)\\1x$" 1000 '(rxmzold))
      (list 'stress-same (make-bytes 1000 (char->integer #\x)) #"^(x*|a)\\1x" 10000 '(rxmzold))
      (list 'stress-same (make-bytes 1000 (char->integer #\x)) #"^(x{499})\\1x" 10000 '(rxmzold))
      (list 'stress-any (make-bytes 1000 (char->integer #\x))  #"(?s:.*)" 100000 '())
      (list 'stress-any (make-bytes 10000 (char->integer #\x))  #"(?s:.*)" 100000 '(mzunicode))
      (list 'stress-any (make-bytes 100000 (char->integer #\x))  #"(?s:.*)" 100000 '(mzunicode))
      (list 'stress-nonlf (make-bytes 100 (char->integer #\x))  #"(?m:.*)" 100000 '(rxmzold))
      (list 'stress-nonlf (make-bytes 1000 (char->integer #\x))  #"(?m:.*)" 100000 '(mzunicode rxmzold))
      (list 'stress-nonlf (make-bytes 10000 (char->integer #\x))  #"(?m:.*)" 100000 '(mzunicode rxmzold))
      (list 'stress-nonlf (make-bytes 100000 (char->integer #\x))  #"(?m:.*)" 10000 '(mzunicode rxmzold))
      (list 'stress-anysave (make-bytes 100 (char->integer #\x))  #"(?s:(.)*)" 100000 '())
      (list 'stress-anysave (make-bytes 1000 (char->integer #\x))  #"(?s:(.)*)" 100000
	    '(pcre python mzunicode rxmzold))
      (list 'stress-anysave (make-bytes 10000 (char->integer #\x))  #"(?s:(.)*)" 100000
	    '(pcre python mzunicode rxmzold))
      (list 'stress-anysave (make-bytes 100000 (char->integer #\x))  #"(?s:(.)*)" 100000
	    '(pcre python mzunicode rxmzold))
      (list 'stress-xs (make-bytes 100 (char->integer #\x))  #"x*" 100000 '())
      (list 'stress-xs (make-bytes 1000 (char->integer #\x))  #"x*" 100000 '())
      (list 'stress-xs (make-bytes 10000 (char->integer #\x))  #"x*" 10000 '())
      (list 'stress-xs (make-bytes 100000 (char->integer #\x))  #"x*" 1000 '())
      (list 'stress-xy (make-bytes 100 (char->integer #\x))  #"[xy]*" 100000 '())
      (list 'stress-xy (make-bytes 1000 (char->integer #\x))  #"[xy]*" 10000 '())
      (list 'stress-xy (make-bytes 10000 (char->integer #\x))  #"[xy]*" 1000 '())
      (list 'stress-xy (make-bytes 100000 (char->integer #\x))  #"[xy]*" 100 '())
      (list 'stress-xysave (make-bytes 100 (char->integer #\x))  #"([xy])*" 100000 '())
      (list 'stress-xory (make-bytes 100 (char->integer #\x))  #"(?:y|x)*" 10000 '())
      (list 'stress-xory (make-bytes 1000 (char->integer #\x))  #"(?:y|x)*" 1000 '(python))
      (list 'stress-xory (make-bytes 10000 (char->integer #\x))  #"(?:y|x)*" 100 '(python))
      (list 'stress-xory (make-bytes 100000 (char->integer #\x))  #"(?:y|x)*" 10 '(pcre python))
      (list 'stress-xorysave (make-bytes 100 (char->integer #\x))  #"(y|x)*" 10000 '())
      (list 'stress-yzorx (make-bytes 100 (char->integer #\x))  #"(?:[yz]|x)*" 10000 '())
      (list 'stress-yzorx (make-bytes 1000 (char->integer #\x))  #"(?:[yz]|x)*" 1000 '(python))
      (list 'stress-yzorx (make-bytes 10000 (char->integer #\x))  #"(?:[yz]|x)*" 100 '(python))
      (list 'stress-yzorx (make-bytes 100000 (char->integer #\x))  #"(?:[yz]|x)*" 10 '(pcre python))
      (list 'stress-yzorxsave (make-bytes 100 (char->integer #\x))  #"([yz]|x)*" 10000 '())
      (list 'stress-x2 (make-bytes 100 (char->integer #\x))  #"(?:x{2})*" 10000 '(rxmzold))
      (list 'stress-x2 (make-bytes 1000 (char->integer #\x))  #"(?:x{2})*" 10000 '(python rxmzold))
      (list 'stress-x2 (make-bytes 10000 (char->integer #\x))  #"(?:x{2})*" 100 '(python rxmzold))
      (list 'stress-x2 (make-bytes 100000 (char->integer #\x))  #"(?:x{2})*" 100 '(pcre python rxmzold))
      (list 'stress-x2saveall (make-bytes 100 (char->integer #\x))  #"(x{2})*" 10000 '(rxmzold))
      (list 'stress-x2save (make-bytes 100 (char->integer #\x))  #"((x){2})*" 10000 '(rxmzold))
      (list 'stress-foobarbaz (bytes-append (random-letters 100) #"FOOBARBAZ")  #"[a-z]*FOOBARBAZ" 100000 '())
      (list 'stress-foobarbaz (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"[a-z]*FOOBARBAZ" 10000 '())
      (list 'stress-foobarbaz (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"[a-z]*FOOBARBAZ" 1000 '())
      (list 'stress-foobarbazsave (bytes-append (random-letters 100) #"FOOBARBAZ")  #"([a-z])*FOOBARBAZ" 100000 '())
      (list 'stress-nope (bytes-append (random-letters 100) #"FOOBARBAZ")  #"[a-z]*FOOBARNOPE" 1000000 '(python))
      (list 'stress-nope (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"[a-z]*FOOBARNOPE" 100000 '(pcre python))
      (list 'stress-nope (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"[a-z]*FOOBARNOPE" 10000 '(pcre python))
      (list 'stress-nopesave (bytes-append (random-letters 100) #"FOOBARBAZ")  #"([a-z])*FOOBARNOPE" 1000000 '(python))
      (list 'stress-altfoobarbaz (bytes-append (random-letters 100) #"FOOBARBAZ")  #"(?:[a-z]|ab)*FOOBARBAZ" 10000 '())
      (list 'stress-altfoobarbaz (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"(?:[a-z]|ab)*FOOBARBAZ" 1000 '())
      (list 'stress-altfoobarbaz (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"(?:[a-z]|ab)*FOOBARBAZ" 10
	    '(python))
      (list 'stress-altnope (bytes-append (random-letters 100) #"FOOBARBAZ")  #"(?:[a-z]|ab)*FOOBARNOPE" 1000000 
	    '(python))
      (list 'stress-altnope (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"(?:[a-z]|ab)*FOOBARNOPE" 100000
	    '(pcre python))
      (list 'stress-altnope (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"(?:[a-z]|ab)*FOOBARNOPE" 10000 
	    '(pcre python))
      (list 'stress-nopeci (bytes-append (random-letters 100) #"FOOBARBAZ")  #"(?i:[a-z]*FOOBARNOPE)" 10000 '(rxmzold))
      (list 'stress-nopeci (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"(?i:[a-z]*FOOBARNOPE)" 1000 
	    '(pcre perl python rxmzold))
      (list 'stress-nopeci (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"(?i:[a-z]*FOOBARNOPE)" 1000
	    '(pcre perl python rxmzold)))))
  
  (define benchmark-names (map car inputs))
  
  (define testers
    (list (list 'racket test-racket)
          (list 'perl test-perl)
          (list 'python test-python)
          (list 'pcre test-pcre)
	  (list 'mzunicode test-mzunicode)
	  (list 'rxmzold test-rxmzold)))

  (define non-defaults (list 'mzunicode
			     'rxmzold))

  ;; Extract command-line arguments --------------------

  (define-values (actual-benchmarks-to-run 
                  actual-implementations-to-run 
                  num-iterations)
    (process-command-line benchmark-names null
                          (map car testers) non-defaults
                          3))
  
  ;; Run benchmarks -------------------------------

  (define (log10 n)
    (inexact->exact (round (/ (log n) (log 10)))))

  (define (run who which)
    (let ([t (assoc which inputs)])
      (let-values ([(name index input rx iterations skips) (apply values t)])
        #;
        (printf "Testing ~a: ~s on ~a iterations of a ~a-byte input\n" 
        who
        rx
        iterations
        (bytes-length input))
	(let nloop ([n num-iterations])
	  (unless (zero? n)
	    (let ([ms (if (memq who skips)
			  #f
			  ((cadr (assoc who testers)) input rx iterations))])
	      (rprintf "[~a ~s (~a #f #f) #f]\n"
		       who
		       (string->symbol (format "~a ~a/~a/~a" index name (log10 (bytes-length input)) (log10 iterations)))
		       (and ms (inexact->exact (round ms)))))
	    (nloop (sub1 n)))))))
  
  (rprintf "; ~a\n" (date->string (seconds->date (current-seconds)) #t))

  (for-each (lambda (i)
              (for-each (lambda (bm)
                          (run i bm))
                        actual-benchmarks-to-run))
            actual-implementations-to-run))
