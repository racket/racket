
(load-relative "loadtest.rktl")

(Section 'unicode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  UTF-8 boundary tests based on Markus Kuhn's test suite

(define basic-utf-8-tests
  '((#(#x3ba #x1f79 #x3c3 #x3bc #x3b5) complete
     (#o316 #o272 #o341 #o275 #o271 #o317 #o203 #o316 #o274 #o316 #o265))
    (#(0) complete
     (#o0))
    (#(#x00000080) complete
     (#o302 #o200))
    (#(#x00000800) complete
     (#o340 #o240 #o200))
    (#(#x00010000) complete
     (#o360 #o220 #o200 #o200))
    (#(#x0010FFFF) complete
     (#o364 #o217 #o277 #o277))
    (#(#f #f #f #f) error ; would be #x00110000 without UTF-8 limit
     (#o364 #o220 #o200 #o200))
    (#(#f #f  #f #f #f) error
     (#o370 #o210 #o200 #o200 #o200))
    (#(#f #f #f #f #f #f) error
     (#o374 #o204 #o200 #o200 #o200 #o200))
    (#(#x0000007F) complete
     (127))
    (#(#x000007FF) complete
     (#o337 #o277))
    (#(#f #f #f #f) error ; #x001FFFFF
     (#o367 #o277 #o277 #o277))
    (#(#f #f #f #f #f) error ; #x03FFFFFF
     (#o373 #o277 #o277 #o277 #o277))
    (#(#f #f #f #f #f #f) error ; #x7FFFFFFF
     (#o375 #o277 #o277 #o277 #o277 #o277))
    (#(#x0000D7FF) complete
     (#o355 #o237 #o277))
    (#(#x0000E000) complete
     (#o356 #o200 #o200))
    (#(#x0000FFFD) complete
     (#o357 #o277 #o275))
    (#(#x0010FFFF) complete
     (#o364 #o217 #o277 #o277))
    ;; Missing start byte
    (#(#f) error
     (#o200))
    (#(#f) error
     (#o277))
    (#(#f #f) error
     (#o200 #o277))
    (#(#f #f #f) error
     (#o200 #o277 #o200))
    (#(#f #f #f #f) error
     (#o200 #o277 #o200 #o277))
    (#(#f #f #f #f #f) error
     (#o200 #o277 #o200 #o277 #o200))
    (#(#f #f #f #f #f #f) error
     (#o200 #o277 #o200 #o277 #o200 #o277))
    (#(#f #f #f #f #f #f #f) error
     (#o200 #o277 #o200 #o277 #o200 #o277 #o200))
    (#(#f) error
     (#o200))
    (#(#f) error
     (#o201))
    (#(#f) error
     (#o202))
    (#(#f) error
     (#o203))
    (#(#f) error
     (#o204))
    (#(#f) error
     (#o205))
    (#(#f) error
     (#o206))
    (#(#f) error
     (#o207))
    (#(#f) error
     (#o210))
    (#(#f) error
     (#o211))
    (#(#f) error
     (#o212))
    (#(#f) error
     (#o213))
    (#(#f) error
     (#o214))
    (#(#f) error
     (#o215))
    (#(#f) error
     (#o216))
    (#(#f) error
     (#o217))
    (#(#f) error
     (#o220))
    (#(#f) error
     (#o221))
    (#(#f) error
     (#o222))
    (#(#f) error
     (#o223))
    (#(#f) error
     (#o224))
    (#(#f) error
     (#o225))
    (#(#f) error
     (#o226))
    (#(#f) error
     (#o227))
    (#(#f) error
     (#o230))
    (#(#f) error
     (#o231))
    (#(#f) error
     (#o232))
    (#(#f) error
     (#o233))
    (#(#f) error
     (#o234))
    (#(#f) error
     (#o235))
    (#(#f) error
     (#o236))
    (#(#f) error
     (#o237))
    (#(#f) error
     (#o240))
    (#(#f) error
     (#o241))
    (#(#f) error
     (#o242))
    (#(#f) error
     (#o243))
    (#(#f) error
     (#o244))
    (#(#f) error
     (#o245))
    (#(#f) error
     (#o246))
    (#(#f) error
     (#o247))
    (#(#f) error
     (#o250))
    (#(#f) error
     (#o251))
    (#(#f) error
     (#o252))
    (#(#f) error
     (#o253))
    (#(#f) error
     (#o254))
    (#(#f) error
     (#o255))
    (#(#f) error
     (#o256))
    (#(#f) error
     (#o257))
    (#(#f) error
     (#o260))
    (#(#f) error
     (#o261))
    (#(#f) error
     (#o262))
    (#(#f) error
     (#o263))
    (#(#f) error
     (#o264))
    (#(#f) error
     (#o265))
    (#(#f) error
     (#o266))
    (#(#f) error
     (#o267))
    (#(#f) error
     (#o270))
    (#(#f) error
     (#o271))
    (#(#f) error
     (#o272))
    (#(#f) error
     (#o273))
    (#(#f) error
     (#o274))
    (#(#f) error
     (#o275))
    (#(#f) error
     (#o276))
    (#(#f) error
     (#o277))
    ;; 2-byte seqs with no continuation
    (#(#f 32) error
     (#o300 #o40))
    (#(#f 32) error
     (#o301 #o40))
    (#(#f 32) error
     (#o302 #o40))
    (#(#f 32) error
     (#o303 #o40))
    (#(#f 32) error
     (#o304 #o40))
    (#(#f 32) error
     (#o305 #o40))
    (#(#f 32) error
     (#o306 #o40))
    (#(#f 32) error
     (#o307 #o40))
    (#(#f 32) error
     (#o310 #o40))
    (#(#f 32) error
     (#o311 #o40))
    (#(#f 32) error
     (#o312 #o40))
    (#(#f 32) error
     (#o313 #o40))
    (#(#f 32) error
     (#o314 #o40))
    (#(#f 32) error
     (#o315 #o40))
    (#(#f 32) error
     (#o316 #o40))
    (#(#f 32) error
     (#o317 #o40))
    (#(#f 32) error
     (#o320 #o40))
    (#(#f 32) error
     (#o321 #o40))
    (#(#f 32) error
     (#o322 #o40))
    (#(#f 32) error
     (#o323 #o40))
    (#(#f 32) error
     (#o324 #o40))
    (#(#f 32) error
     (#o325 #o40))
    (#(#f 32) error
     (#o326 #o40))
    (#(#f 32) error
     (#o327 #o40))
    (#(#f 32) error
     (#o330 #o40))
    (#(#f 32) error
     (#o331 #o40))
    (#(#f 32) error
     (#o332 #o40))
    (#(#f 32) error
     (#o333 #o40))
    (#(#f 32) error
     (#o334 #o40))
    (#(#f 32) error
     (#o335 #o40))
    (#(#f 32) error
     (#o336 #o40))
    (#(#f 32) error
     (#o337 #o40))
    ;; 3-byte seqs with no continuation
    (#(#f 32) error
     (#o340 #o40))
    (#(#f 32) error
     (#o341 #o40))
    (#(#f 32) error
     (#o342 #o40))
    (#(#f 32) error
     (#o343 #o40))
    (#(#f 32) error
     (#o344 #o40))
    (#(#f 32) error
     (#o345 #o40))
    (#(#f 32) error
     (#o346 #o40))
    (#(#f 32) error
     (#o347 #o40))
    (#(#f 32) error
     (#o350 #o40))
    (#(#f 32) error
     (#o351 #o40))
    (#(#f 32) error
     (#o352 #o40))
    (#(#f 32) error
     (#o353 #o40))
    (#(#f 32) error
     (#o354 #o40))
    (#(#f 32) error
     (#o355 #o40))
    (#(#f 32) error
     (#o356 #o40))
    (#(#f 32) error
     (#o357 #o40))
    ;; 3-byte seqs with partial continuation
    (#(#f #f 32) error
     (#o340 #o203 #o40))
    (#(#f #f 32) error
     (#o341 #o203 #o40))
    (#(#f #f 32) error
     (#o342 #o203 #o40))
    (#(#f #f 32) error
     (#o343 #o203 #o40))
    (#(#f #f 32) error
     (#o344 #o203 #o40))
    (#(#f #f 32) error
     (#o345 #o203 #o40))
    (#(#f #f 32) error
     (#o346 #o203 #o40))
    (#(#f #f 32) error
     (#o347 #o203 #o40))
    (#(#f #f 32) error
     (#o350 #o203 #o40))
    (#(#f #f 32) error
     (#o351 #o203 #o40))
    (#(#f #f 32) error
     (#o352 #o203 #o40))
    (#(#f #f 32) error
     (#o353 #o203 #o40))
    (#(#f #f 32) error
     (#o354 #o203 #o40))
    (#(#f #f 32) error
     (#o355 #o203 #o40))
    (#(#f #f 32) error
     (#o356 #o203 #o40))
    (#(#f #f 32) error
     (#o357 #o203 #o40))
    ;; 4-byte seq with no continuations
    (#(#f 32) error
     (#o360 #o40))
    (#(#f 32) error
     (#o361 #o40))
    (#(#f 32) error
     (#o362 #o40))
    (#(#f 32) error
     (#o363 #o40))
    (#(#f 32) error
     (#o364 #o40))
    (#(#f 32) error
     (#o365 #o40))
    (#(#f 32) error
     (#o366 #o40))
    (#(#f 32) error
     (#o367 #o40))
    ;; 4-byte seq with only 1 continuation
    (#(#f #f 32) error
     (#o360 #o203 #o40))
    (#(#f #f 32) error
     (#o361 #o203 #o40))
    (#(#f #f 32) error
     (#o362 #o203 #o40))
    (#(#f #f 32) error
     (#o363 #o203 #o40))
    (#(#f #f 32) error
     (#o364 #o203 #o40))
    (#(#f #f 32) error
     (#o365 #o203 #o40))
    (#(#f #f 32) error
     (#o366 #o203 #o40))
    (#(#f #f 32) error
     (#o367 #o203 #o40))
    ;; 4-byte seq with only 2 continuation
    (#(#f #f #f 32) error
     (#o360 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o361 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o362 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o363 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o364 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o365 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o366 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o367 #o203 #o203 #o40))
    ;; 5-byte seqs with no continuation
    (#(#f 32) error
     (#o370 #o40))
    (#(#f 32) error
     (#o371 #o40))
    (#(#f 32) error
     (#o372 #o40))
    (#(#f 32) error
     (#o373 #o40))
    ;; 5-byte seqs with only 1 continuation
    (#(#f #f 32) error
     (#o370 #o203 #o40))
    (#(#f #f 32) error
     (#o371 #o203 #o40))
    (#(#f #f 32) error
     (#o372 #o203 #o40))
    (#(#f #f 32) error
     (#o373 #o203 #o40))
    ;; 5-byte seqs with only 2 continuations
    (#(#f #f #f 32) error
     (#o370 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o371 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o372 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o373 #o203 #o203 #o40))
    ;; 5-byte seqs with only 3 continuations
    (#(#f #f #f #f 32) error
     (#o370 #o203 #o203 #o203 #o40))
    (#(#f #f #f #f 32) error
     (#o371 #o203 #o203 #o203 #o40))
    (#(#f #f #f #f 32) error
     (#o372 #o203 #o203 #o203 #o40))
    (#(#f #f #f #f 32) error
     (#o373 #o203 #o203 #o203 #o40))
    ;; 6-byte seqs with no continuation
    (#(#f 32) error
     (#o374 #o40))
    (#(#f 32) error
     (#o375 #o40))
    ;; 6-byte seqs with only 1 continuation
    (#(#f #f 32) error
     (#o374 #o203 #o40))
    (#(#f #f 32) error
     (#o375 #o203 #o40))
    ;; 6-byte seqs with only 2 continuation
    (#(#f #f #f 32) error
     (#o374 #o203 #o203 #o40))
    (#(#f #f #f 32) error
     (#o375 #o203 #o203 #o40))
    ;; 6-byte seqs with only 3 continuation
    (#(#f #f #f #f 32) error
     (#o374 #o203 #o203 #o203 #o40))
    (#(#f #f #f #f 32) error
     (#o375 #o203 #o203 #o203 #o40))
    ;; 6-byte seqs with only 4 continuation
    (#(#f #f #f #f #f 32) error
     (#o374 #o203 #o203 #o203 #o203 #o40))
    (#(#f #f #f #f #f 32) error
     (#o375 #o203 #o203 #o203 #o203 #o40))
    ;; Sequences with last continuation byte missing, eol instead of space
    (#(#f) error ; aborts
     (#o300))
    (#(#f) aborts
     (#o310))
    (#(#f #f) error ; aborts
     (#o340 #o200))
    (#(#f #f) aborts
     (#o350 #o200))
    (#(#f #f) aborts
     (#o340 #o240))
    (#(#f) aborts
     (#o340))
    (#(#f #f #f) error ; aborts
     (#o360 #o200 #o200))
    (#(#f #f #f) aborts
     (#o361 #o200 #o200))
    (#(#f #f) error ; aborts
     (#o360 #o200))
    (#(#f #f) aborts
     (#o361 #o200))
    (#(#f) aborts
     (#o360))
    (#(#f) aborts
     (#o361))
    (#(#f #f #f #f) error ; aborts
     (#o370 #o200 #o200 #o200))
    (#(#f #f #f #f) error ;; aborts
     (#o371 #o200 #o200 #o200))
    (#(#f) error ;; aborts
     (#o370))
    (#(#f) error ;; aborts
     (#o371))
    (#(#f #f) error ; aborts
     (#o370 #o200))
    (#(#f #f) error ;; aborts
     (#o371 #o200))
    (#(#f #f #f) error ; aborts
     (#o370 #o200 #o200))
    (#(#f #f #f) error ;; aborts
     (#o371 #o200 #o200))
    (#(#f #f #f #f #f) error ; aborts
     (#o374 #o200 #o200 #o200 #o200))
    (#(#f #f #f #f #f) error ;; aborts
     (#o375 #o200 #o200 #o200 #o200))
    (#(#f) error ;; aborts
     (#o374))
    (#(#f) error ;; aborts
     (#o375))
    (#(#f #f) error ; aborts
     (#o374 #o200))
    (#(#f #f) error ;; aborts
     (#o375 #o200))
    (#(#f #f #f) error ; aborts
     (#o374 #o200 #o200))
    (#(#f #f #f) error ;; aborts
     (#o375 #o200 #o200))
    (#(#f #f #f #f) error ; aborts
     (#o374 #o200 #o200 #o200))
    (#(#f #f #f #f) error ; aborts
     (#o375 #o200 #o200 #o200))
    (#(#f) aborts
     (#o337))
    (#(#f #f) aborts
     (#o357 #o277))
    (#(#f #f #f) aborts
     (#o364 #o277 #o277))
    (#(#f #f #f) error ;; aborts
     (#o365 #o277 #o277))
    (#(#f #f #f) error ;; aborts
     (#o366 #o277 #o277))
    (#(#f #f #f) error ;; aborts
     (#o367 #o277 #o277))
    (#(#f #f #f #f) error ;; aborts
     (#o373 #o277 #o277 #o277))
    (#(#f #f #f #f #f) error ; aborts
     (#o375 #o277 #o277 #o277 #o277))
    ;; Concatenation of incomplete sequences
    (#(#f #f #f #f #f #f #f #f #f) error/aborts
     (#o300 #o340 #o200 #o360 #o200 #o200 #o361 #o200 #o200)
     3)
    ;; Impossible bytes
    (#(#f) error
     (#o376))
    (#(#f) error
     (#o377))
    (#(#f #f #f #f) error
     (#o376 #o376 #o377 #o377))
    ;; Overlong
    (#(#f #f) error
     (#o300 #o257))
    (#(#f #f #f) error
     (#o340 #o200 #o257))
    (#(#f #f #f #f) error
     (#o360 #o200 #o200 #o257))
    (#(#f #f #f #f #f) error
     (#o370 #o200 #o200 #o200 #o257))
    (#(#f #f #f #f #f #f) error
     (#o374 #o200 #o200 #o200 #o200 #o257))
    (#(#f #f) error
     (#o301 #o277))
    (#(#f #f #f) error
     (#o340 #o237 #o277))
    (#(#f #f #f #f) error
     (#o360 #o217 #o277 #o277))
    (#(#f #f #f #f #f) error
     (#o370 #o207 #o277 #o277 #o277))
    (#(#f #f #f #f #f #f) error
     (#o374 #o203 #o277 #o277 #o277 #o277))
    (#(#f #f) error
     (#o300 #o200))
    (#(#f #f #f) error
     (#o340 #o200 #o200))
    (#(#f #f #f #f) error
     (#o360 #o200 #o200 #o200))
    (#(#f #f #f #f #f) error
     (#o370 #o200 #o200 #o200 #o200))
    (#(#f #f #f #f #f #f) error
     (#o374 #o200 #o200 #o200 #o200 #o200))
    ;; illedgal surrogates
    (#(#f #f #f) surrogate1
     (#o355 #o240 #o200))
    (#(#f #f #f) surrogate1
     (#o355 #o255 #o277))
    (#(#f #f #f) surrogate1
     (#o355 #o256 #o200))
    (#(#f #f #f) surrogate1
     (#o355 #o257 #o277))
    (#(#f #f #f) surrogate2
     (#o355 #o260 #o200))
    (#(#f #f #f) surrogate2
     (#o355 #o276 #o200))
    (#(#f #f #f) surrogate2
     (#o355 #o277 #o277))
    (#(#f #f #f #f #f #f) error
     (#o355 #o240 #o200 #o355 #o260 #o200))
    (#(#f #f #f #f #f #f) error
     (#o355 #o240 #o200 #o355 #o277 #o277))
    (#(#f #f #f #f #f #f) error
     (#o355 #o255 #o277 #o355 #o260 #o200))
    (#(#f #f #f #f #f #f) error
     (#o355 #o255 #o277 #o355 #o277 #o277))
    (#(#f #f #f #f #f #f) error
     (#o355 #o256 #o200 #o355 #o260 #o200))
    (#(#f #f #f #f #f #f) error
     (#o355 #o256 #o200 #o355 #o277 #o277))
    (#(#f #f #f #f #f #f) error
     (#o355 #o257 #o277 #o355 #o260 #o200))
    (#(#f #f #f #f #f #f) error
     (#o355 #o257 #o277 #o355 #o277 #o277))
    ;; 0xFFFE and 0xFFFF
    (#(#xFFFE) complete
     (#o357 #o277 #o276))
    (#(#xFFFF) complete
     (#o357 #o277 #o277))))

(define replace-size 3) ; #\uFFFD -> 3 bytes in UTF

(define (string->print s) (map char->integer (string->list s)))

(define bytes->unicode-vector
  (case-lambda
   [(s)
    (list->vector (map char->integer (string->list (bytes->string/utf-8 s))))]
   [(s start)
    (bytes->unicode-vector (subbytes s start))]
   [(s start end)
    (bytes->unicode-vector (subbytes s start end))]))
(define (bytes-any->unicode-vector s non-v)
  (list->vector 
   (map (lambda (x) (if (= x (char->integer #\?)) non-v x))
	(map char->integer (string->list (bytes->string/utf-8 s #\?))))))
(define (unicode-vector->bytes v)
  (string->bytes/utf-8 (list->string (map integer->char (vector->list v)))))

(define (list->string/eof l)
  (if (and (pair? l) (eof-object? (car l)) (null? (cdr l)))
      eof
      (list->string l)))

(let ([utf-8-iconv (bytes-open-converter "UTF-8" "UTF-8")]
      [utf-8-iconv-p (bytes-open-converter "UTF-8-permissive" "UTF-8")])
  ;; First, check some simple conversions
  (let-values ([(z1 z2 more) (bytes-convert utf-8-iconv #"xyz" 0 3 (bytes))])
    (test '(0 0 continues) list z1 z2 more))
  (let-values ([(z1 z2 more) (bytes-convert utf-8-iconv #"xyz" 0 3 (bytes 0))])
    (test '(1 1 continues) list z1 z2 more))
  (let-values ([(z1 z2 more) (bytes-convert utf-8-iconv #"xyz" 0 3 (bytes 0 0 0))])
    (test '(3 3 complete) list z1 z2 more))

  ;; The real tests:
  (for-each (lambda (p)
	      (let ([code-points (car p)]
		    [parse-status (cadr p)]
		    [s (apply bytes (caddr p))])
		(if (and (positive? (vector-length code-points))
			 (vector-ref code-points 0))
		    (begin
		      (test (vector-length code-points) bytes-utf-8-length s)
		      (test code-points bytes->unicode-vector s)
		      (test code-points bytes-any->unicode-vector s #f)
		      (test s unicode-vector->bytes code-points)
		      (test 0 bytes-utf-8-index s 0)
		      (test (bytes-length s) bytes-utf-8-index 
			    (bytes-append s #"x")
			    (vector-length code-points))
		      (if ((vector-length code-points) . > . 1)
			  (begin
			    (test (integer->char (vector-ref code-points 0))
				  bytes-utf-8-ref s 0)
			    (test (integer->char (vector-ref code-points
							     (sub1 (vector-length code-points))))
				  bytes-utf-8-ref s (sub1 (vector-length code-points)))
			    (let ([post-1 (bytes-utf-8-index s 1)])
			      (test #t positive? post-1)
			      (test (list->vector (cdr (vector->list code-points)))
				    bytes->unicode-vector s post-1))
			    (let ([last-1 (bytes-utf-8-index s (sub1 (vector-length code-points)))])
			      (test #t positive? last-1)
			      (test code-points
				    list->vector (append
						  (vector->list (bytes->unicode-vector s 0 last-1))
						  (vector->list (bytes->unicode-vector s last-1))))))
			  (test #f bytes-utf-8-index s 1))
		      (test-values (list s (bytes-length s) 'complete)
				   (lambda () (bytes-convert utf-8-iconv s)))
		      (test (list s) regexp-match #rx"^.*$" s))
		    (begin
		      (test code-points bytes-any->unicode-vector s #f)
		      (test (list->vector (append '(97) (vector->list code-points) '(98)))
			    bytes-any->unicode-vector (bytes-append #"a" s #"b") #f)
		      (test (list->vector (append (vector->list code-points) (vector->list code-points)))
			    bytes-any->unicode-vector (bytes-append s s) #f)
		      (test #f bytes-utf-8-length s)
		      (test 0 bytes-utf-8-index s 0)
		      (let-values ([(s2 n status) (bytes-convert utf-8-iconv s)])
			(test (case parse-status 
				[(error/aborts surrogate1 surrogate2) 'error]
				[else parse-status ])
			      'status status))
		      (let ([convert
			     (lambda (prefix)
			       (test (+ (vector-length code-points) (bytes-length prefix))
				     bytes-utf-8-length (bytes-append prefix s) #\uFFFD)
			       (test (vector-length code-points)
				     bytes-utf-8-length (bytes-append prefix s) #\uFFFD (bytes-length prefix))
			       (test (if (equal? prefix #"") 
					 #f 
					 (integer->char (bytes-ref prefix 0)))
				     bytes-utf-8-ref (bytes-append prefix s) 0)
			       (test (if (equal? prefix #"") 
					 (if (equal? #() code-points)
					     #f
					     #\uFFFD)
					 (integer->char (bytes-ref prefix 0)))
				     bytes-utf-8-ref (bytes-append prefix s) 0 #\uFFFD)
			       (test (if (equal? #() code-points)
					 (if (equal? #"" prefix)
					     #f
					     (integer->char (bytes-ref prefix (sub1 (bytes-length prefix)))))
					 (integer->char
					  (or (vector-ref code-points (sub1 (vector-length code-points)))
					      (char->integer #\uFFFD))))
				     bytes-utf-8-ref (bytes-append prefix s) 
				     (max 0 (+ (bytes-length prefix) (sub1 (vector-length code-points))))
				     #\uFFFD)

			       (let-values ([(s2 n status) 
					     (bytes-convert utf-8-iconv-p (bytes-append prefix s))]
					    [(pl) (bytes-length prefix)])
				 (case parse-status
				   [(error surrogate1 surrogate2)
				    (test 'complete 'status status)
				    (test (+ (bytes-length s) pl) 'n n)
				    (test (+ (* replace-size (vector-length code-points)) 
                                             (if (and (positive? (vector-length code-points))
                                                      (eq? (vector-ref code-points (sub1 (vector-length code-points)))
                                                           #o40))
                                                 ;; space at end is converted, not replaced by #\xFFFD
                                                 (- 1 replace-size)
                                                 0)
                                             pl) 
                                          bytes-length s2)
				    (test (append (bytes->list prefix)
						  (map 
						   (lambda (i) (or i (char->integer #\uFFFD)))
						   (vector->list code-points)))
					  vector->list (bytes->unicode-vector s2))]
				   [(error/aborts)
				    (test 'aborts 'status status)
				    (let ([code-points (list->vector
							(reverse
							 (list-tail
							  (reverse
							   (vector->list code-points))
							  ;; indicates how many to be unused due to abort:
							  (cadddr p))))])
				      (test (+ (* replace-size (vector-length code-points)) pl) bytes-length s2)
				      (test (append (bytes->list prefix)
						    (map 
						     (lambda (i) (or i (char->integer #\uFFFD)))
						     (vector->list code-points)))
					    vector->list (bytes->unicode-vector s2)))]
				   [else
				    (test parse-status 'status status)])))])
			(convert #"")
			(convert #"so"))
		      (let-values ([(s2 n status) (bytes-convert utf-8-iconv-p (bytes-append s #"xxxxx"))])
			(test 'complete 'status status)
			(test '(#"xxxxx") regexp-match #rx#"xxxxx$" s2)
			(test (+ 5 (bytes-length s)) 'n n))
		      (test #f regexp-match #rx"^.*$" s)))
		;; Test byte reading and port positions
		(let ([v (bytes-any->unicode-vector s #f)])
		  (define (check-full-read read-all-bytes)
		    (let ([p (open-input-bytes s)])
		      (port-count-lines! p)
		      (read-all-bytes p)
		      (let-values ([(l c p) (port-next-location p)])
			(test (vector-length v) 'c c)
			(test (add1 (vector-length v)) 'p p)))
		    (let ([p (open-input-string (format "\t~a\t" s))])
		      (port-count-lines! p)
		      (read-all-bytes p)
		      (let-values ([(l c p) (port-next-location p)])
			(test p 'p (add1 (+ 2 (vector-length v))))
			(test c 'tab (+ 16
					(- (vector-length v)
					   (bitwise-and (vector-length v) 7))))))
		    (let ([p (open-input-string (format "~a\t~a" s s))])
		      (port-count-lines! p)
		      (read-all-bytes p)
		      (let-values ([(l c p) (port-next-location p)])
			(test p 'p (add1 (+ 1 (* 2 (vector-length v)))))
			(test c 'tab (+ 8
					(- (vector-length v)
					   (bitwise-and (vector-length v) 7))
					(vector-length v)))))
		    (let ([p (open-input-string (format "~a\n~a" s s))])
		      (port-count-lines! p)
		      (read-all-bytes p)
		      (let-values ([(l c p) (port-next-location p)])
			(test p 'p (+ 2 (* 2 (vector-length v))))
			(test c 'cr (vector-length v)))))
		  (check-full-read (lambda (p) (read-bytes 500 p)))
		  (check-full-read (lambda (p)
				     (let loop ()
				       (unless (eof-object? (read-byte p))
					 (loop)))))
		  ;; Check result char-by-char:
		  (let ([p (open-input-bytes s)])
		    (port-count-lines! p)
		    (let loop ([old-c 0][old-p 1])
		      (let ([i (read-char p)])
			(let-values ([(l c p) (port-next-location p)])
			  (if (eof-object? i)
			      (begin
				(test (cons s old-c) values (cons s c))
				(test (cons s old-p) values (cons s p)))
			      (begin
				(test (cons s (add1 old-c)) values (cons s c))
				(test (cons s (add1 old-p)) values (cons s p))
				(loop c p))))))))
		;; Test read-string decoding
		(let ([us (apply string (map (lambda (i)
					       (if i (integer->char i) #\uFFFD))
					     (vector->list code-points)))])
		  (test us bytes->string/utf-8 s #\uFFFD)
		  (test us read-string (vector-length code-points) (open-input-bytes s))
		  (test us read-string (* 100 (vector-length code-points)) (open-input-bytes s))
		  (unless (string=? "" us)
		    (test (substring us 0 1) read-string 1 (open-input-bytes s)))
		  (let ([go (lambda (read-string)
			      (let-values ([(in out) (make-pipe)])
				(let loop ([i 0])
				  (unless (= i 10)
				    (write-bytes s out)
				    (loop (add1 i))))
				(close-output-port out)
				(test #t 'read-10
				      (let loop ([i 0])
					(if (= i 10)
					    (let ([v (read-string 1 in)])
					      (or (eof-object? v)
						  (and (test eof `(eof for ,(caddr p) = ,us with ,read-string) v)
						       #t)))
					    (let ([rs (read-string (string-length us) in)])
					      (and (or (equal? us rs)
						       (test (string->print us) `(,i of 10 for ,(caddr p) with ,read-string) 
							     (string->print rs)))
						   (loop (add1 i)))))))))])
		    (go read-string)
		    ;; Peek-string at front, consistent with read-string:
		    (go (lambda (n p)
			  (let ([a (peek-string n 0 p)]
				[b (read-string n p)])
			    (if (equal? a b)
				a
				(list a b)))))
		    ;; Peek-string, using offset calculated from s
		    (go (let ([pos 0])
			  (lambda (n p)
			    (begin0
			     (peek-string n pos p)
			     (set! pos (+ (bytes-length s) pos))))))
		    ;; Read-char
		    (go (lambda (n p)
			  (list->string/eof
			   (let loop ([i 0])
			     (if (= i n)
				 null
				 (cons (read-char p) (loop (add1 i))))))))
		    ;; Peek-char, consistent with read-char
		    (go (lambda (n p)
			    (list->string/eof
			     (let loop ([i 0])
			       (if (= i n)
				   null
				   (let ([a (peek-char p 0)]
					 [b (read-char p)])
				     (if (equal? a b)
					 (cons a (loop (add1 i)))
					 (list* #\* a b #\* (loop (add1 i))))))))))
		    ;; Read-string, one or two at a time
		    (let ([read-n
			   (lambda (n p d)
			     (let loop ([n n][a null])
			       (if (zero? n)
				   (apply string-append (reverse a))
				   (let ([s (read-string (min d n) p)])
				     (if (eof-object? s)
					 (if (null? a)
					     s
					     (loop 0 a))
					 (loop (- n (min d n)) (cons s a)))))))])
		      (go (lambda (n p) (read-n n p 1)))
		      (go (lambda (n p) (read-n n p 2))))))
		;; Test UTF-16
		(let ([c (bytes-open-converter "platform-UTF-8" "platform-UTF-16")])
		  (let-values ([(s2 n status) (bytes-convert c s)])
		    (case parse-status
		      [(surrogate1 surrogate2)
		       (if (eq? (system-type) 'windows)
			   (begin
			     (if (eq? parse-status 'surrogate1)
				 (test 'aborts 'status status)
				 (test 'complete 'status status))
			     ;; Double the surrogate, and then the "unpaired"
			     ;; surrogates are still allowed, and they're doubled
			     (let-values ([(s2 n status) (bytes-convert c (bytes-append s s))])
			       (if (eq? parse-status 'surrogate1)
				   (begin
				     (test 3 'ds-count n)
				     (test 'aborts `(status ,s) status))
				   (test 'complete `(status ,s) status)))
			     (let ([try-xtra
				    (lambda (xtra all? d-size)
				      (when all?
					;; Add non-surrogate to the end, and it should always work:
				        (let-values ([(s2 n status) (bytes-convert c (bytes-append s xtra))])
					  (test 'complete 'status status)
					  (test (+ 3 (bytes-length xtra)) 'cnt n)
					  (test (* 2 (+ 1 d-size)) bytes-length s2)))
				      ;; Same, even if we only accept 2 output bytes:
				      (let-values ([(n0 n status) (bytes-convert c (bytes-append s xtra)
										 0 (+ 3 (bytes-length xtra))
										 (make-bytes 2))])
					 (test 'continues 'status status)
					 (test 3 'cnt n)
					 (test 2 'got n0)))])
			       (try-xtra #"a" #t 1)
			       (try-xtra #"\360\220\200\200" #t 2)
			       (try-xtra #"\355\240\200" #f 1)))
			   (test 'error 'status status))]
		      [(error error/aborts)
		       (test 'error 'status status)]
		      [(aborts)
		       (test 'aborts 'status status)]
		      [else
		       (test parse-status 'status status)])
		    (when (eq? status 'complete)
		      ;; complete => used all bytes
		      (test n 'count (bytes-length s))
		      ;; complete => unconverting to return to original
		      (unless (memq parse-status '(surrogate1 surrogate2))
		        (let ([c2 (bytes-open-converter "platform-UTF-16" "platform-UTF-8")])
			  (let-values ([(s3 n2 status2) (bytes-convert c2 s2)])
			    (test s `(UTF-16-round-trip ,s2) s3)
			    (test n2 'count (bytes-length s2)))
			  ;; Try partial unconvert
			  (let-values ([(s4 n4 status4) (bytes-convert c2 s2 0 (sub1 (bytes-length s2)))])
			    (test 'aborts `(status ,s2) status4))
			  ;; Try unconvert into to-small buffer:
			  (let-values ([(s4 n4 status4) (bytes-convert c2 s2 0 (bytes-length s2) 
								       (make-bytes (sub1 (bytes-length s))))])
			    (test 'continues 'status status4))))
		      ;; complete => can add bytes onto the end
		      (let-values ([(s2 n status) (bytes-convert c (bytes-append s #"x"))])
			(test 'complete 'status status)
			(test n 'count (add1 (bytes-length s))))
		      ;; complete => need enough room for result
		      (when (positive? (bytes-length s2))
			(let ([dest (make-bytes (bytes-length s2))])
			  (let-values ([(s3 n2 status) (bytes-convert c s 0 n dest 0 (sub1 (bytes-length s2)))])
			    (test 'continues 'status status)
			    (when (positive? n2)
			      (let-values ([(s4 n3 status) (bytes-convert c s n2 n dest s3)])
				(test 'complete 'status status)
				(test s2 'bytes-append dest)
				(test n + n2 n3)
				(test (bytes-length s2) + s3 s4)))))))
		    (when (and (eq? status 'error)
			       (not (eq? parse-status 'error/aborts)))
		      ;; permissive decoder should work:
		      (let ([c2 (bytes-open-converter "platform-UTF-8-permissive" "platform-UTF-16")])
			(let-values ([(s2 n status) (bytes-convert c2 s)])
			  (test 'complete 'status status)
			  ;; Should be the same as decoding corrected UTF-8:
			  (let-values ([(s3 n status) (bytes-convert c (string->bytes/utf-8
									(bytes->string/utf-8 s #\uFFFD)))])
			    (test s3 `(permissive ,s) s2)))))))))
	    basic-utf-8-tests))

;; Further UTF-16 tests
(let ([c (bytes-open-converter "platform-UTF-16" "platform-UTF-8")])
  (let-values ([(s n status) (bytes-convert c (bytes-append
					       (integer->integer-bytes #xD800 2 #f)
					       (integer->integer-bytes #xDC00 2 #f)))])
    (test-values (list #"" 0 'aborts)
		 (lambda () (bytes-convert c (integer->integer-bytes #xD800 2 #f) )))
    ;; Windows: unpaired surrogates allowed:
    (when (eq? 'windows (system-type))
      (test-values (list #"" 0 'aborts)
		   (lambda () (bytes-convert c (integer->integer-bytes #xD8FF 2 #f))))
      (test-values (list #"\355\277\277" 2 'complete)
		   (lambda () (bytes-convert c (integer->integer-bytes #xDFFF 2 #f)))))
    ;; Non-windows: after #xD800 bits, surrogate pair is assumed
    (unless (eq? 'windows (system-type))
      (test-values (list #"" 0 'aborts)
		   (lambda () (bytes-convert c (integer->integer-bytes #xD800 2 #f))))
      (test-values (list #"" 0 'aborts)
		   (lambda () (bytes-convert c (integer->integer-bytes #xDFFF 2 #f) )))
      (test-values (list s 4 'complete)
		   (lambda ()
		     (bytes-convert c
				    (bytes-append (integer->integer-bytes #xD800 2 #f) 
						  (integer->integer-bytes #xD800 2 #f)))))
      (test-values (list s 4 'complete)
		   (lambda ()
		     (bytes-convert c
				    (bytes-append (integer->integer-bytes #xD800 2 #f) 
						  (integer->integer-bytes #x0000 2 #f)))))
      (test-values (list s 4 'complete)
		   (lambda ()
		     (bytes-convert c
				    (bytes-append (integer->integer-bytes #xDC00 2 #f) 
						  (integer->integer-bytes #x1000 2 #f))))))))

;; Check a corner of UTF-16 conversion:
(let ([c (bytes-open-converter "platform-UTF-8" "platform-UTF-16")])
  (let-values ([(s n status) (bytes-convert c (string->bytes/utf-8 "\U171D3"))])
    (let ([c2 (bytes-open-converter "platform-UTF-16" "platform-UTF-8")])
      (let-values ([(s2 n2 status2) (bytes-convert c2 s)])
        (bytes->string/utf-8 s2)))))

;; Check that `bytes-convert-end` does nothing for UTF-8 and UTF-16 conversion:
(let ([c (bytes-open-converter "platform-UTF-16" "platform-UTF-8")])
  (test-values '(#"" complete)
               (lambda () (bytes-convert-end c))))
(let ([c (bytes-open-converter "platform-UTF-8" "platform-UTF-16")])
  (test-values '(#"" complete)
               (lambda () (bytes-convert-end c))))
(let ([c (bytes-open-converter "UTF-8-permissive" "UTF-8")])
  (test-values '(#"" complete)
               (lambda () (bytes-convert-end c))))

(when (eq? (system-type) 'windows)
  (let ([c (bytes-open-converter "platform-UTF-8-permissive" "platform-UTF-16")])
    ;; Check that we use all 6 bytes of #"\355\240\200\355\260\200" or none
    (test-values (list 12 6 'complete)
		 (lambda ()
		   (bytes-convert c #"\355\240\200\355\260\200" 0 6 (make-bytes 12))))
    ;; If we can't look all the way to the end, reliably abort without writing:
    (let ([s (make-bytes 12 (char->integer #\x))])
      (let loop ([n 1])
	(unless (= n 6)
	  (test-values (list 0 0 'aborts)
		       (lambda ()
		         (bytes-convert c #"\355\240\200\355\260\200" 0 n s)))
	  (test #"xxxxxxxxxxxx" values s) ; no writes to bytes string
	  (loop (add1 n)))))
    (let ([s (make-bytes 12 (char->integer #\x))])
      (let loop ([n 0])
	(unless (= n 12)
	  (test-values (list 0 0 'continues)
		       (lambda ()
			 (bytes-convert c #"\355\240\200\355\260\200" 0 6 (make-bytes n))))
	  (test #"xxxxxxxxxxxx" values s) ; no writes to bytes string
	  (loop (add1 n)))))))

;; Seems like this sort of thing should be covered above, and maybe it
;;  it after some other corrections. But just in case:
(let ([check-one
       (lambda (str)
	 (for-each
	  (lambda (str)
	    (for-each
	     (lambda (peek?)
	       (for-each
		(lambda (byte?)
		  (let ([p (open-input-string str)])
		    (test str read-string (string-length str) p)
		    (test eof 
			  (if peek? 
			      (if byte? peek-byte peek-char)
			      (if byte? read-byte read-char))
			  p)
		    (test eof (if byte? read-byte read-char) p)))
		'(#t #f)))
	     '(#t #f)))
	  (list str 
		(string-append str "a") 
		(string-append "a" str) 
		(string-append str str))))])
  (check-one "a")
  (check-one "\uA4")
  (check-one "\u104")
  (check-one "\u7238")
  (check-one "\Ua7238")
  (check-one "\U107238"))

(test '(#o302 #o251) bytes->list (unicode-vector->bytes (vector 169)))
(test '(#o304 #o250) bytes->list (unicode-vector->bytes (vector 296)))

(test "a\0b" bytes->string/utf-8 #"a\xFFb" (integer->char 0))

(test '("\uA9") regexp-match #rx"." "\uA9")
(test '(#"\302") regexp-match #rx#"." #"\302\251")
(test '(#"\302") regexp-match #rx#"." "\uA9")
(test '(#"\302\251") regexp-match #rx"." #"\302\251")

(test #f regexp-match #rx"[a-z]" "\uA9")
(test '("\uA9") regexp-match #rx"\uA9" "\uA9")
(test '("\uA9") regexp-match #rx"\uA9+" "\uA9")
(test '("\uA9\uA9") regexp-match #rx"\uA9+" "\uA9\uA9")
(test '("\uA9\uA9") regexp-match #rx"\uA9+" "x\uA9\uA9y")
(test '("\uA9") regexp-match #rx"[a-z\uA9]" "\uA9mm")
(test '("\uA9mm") regexp-match #rx"[a-z\uA9]+" "\uA9mm")
(test '("\uA9") regexp-match #rx"[\uA9-\uE9]+" "xx\uA9mm")
(test '("\uE9") regexp-match #rx"[\uA9-\uE9]+" "xx\uE9mm")
(test #f regexp-match #rx"[\uA9-\uE9]+" "xx\u129")
(test #f regexp-match #rx"[\uA9-\uE9]+" "xx\uA8mm")
(test '("\uE9\uA9") regexp-match #rx"[\uA9-\u128]+" "xx\uE9\uA9mm")

(test #f regexp-match #rx"[^a-z][^a-z]" "\uA9")
(test #f regexp-match #rx"[^a-z][^a-z]" (string->bytes/utf-8 "\uA9"))
(test (list (string->bytes/utf-8 "\uA9")) regexp-match #rx#"[^a-z][^a-z]" "\uA9")
(test (list (string->bytes/utf-8 "\uA9")) regexp-match #rx#"[^a-z][^a-z]" (string->bytes/utf-8 "\uA9"))

;; Nots of patterns and ranges:
(test #f regexp-match #rx"[^a-z\uA9]" "\uA9mm")
(test '("") regexp-match #rx"[^a-d\uA9]*" "\uA9mm")
(test '("xx") regexp-match #rx"[^\uA9-\u128]+" "xx\uA9mm")
(test '("xx") regexp-match #rx"[^\uA9-\u128]+" "xx\303\251mm")
(test '("xx\u129mm") regexp-match #rx"[^\uA9-\u128]+" "xx\u129mm")
(test '("xx\uA8mm") regexp-match #rx"[^\uA9-\u128]+" "xx\uA8mm")
(test '("xx") regexp-match #rx"[^\uA9-\u128]+" "xx\uE9\uA9mm")

;; 3-char seqs
(test '("\u1F79") regexp-match #rx"\u1F79" "a\u1F79\u1F79b")
(test '("\u1F79\u1F79") regexp-match #rx"\u1F79+" "a\u1F79\u1F79b")

(test '("\u1F79\uA9\u1F79r") regexp-match #rx"[c-\u1F79]+" "a\u1F79\uA9\u1F79r")
(test '("d\u1F79\uA9\u1F79r") regexp-match #rx"[c-\u1F79]+" "d\u1F79\uA9\u1F79r")

(test '("\u2F79") regexp-match #rx"[\u1F79-\u3F79]" "\u2F79")
(test '("\u1F79") regexp-match #rx"[\u1F79-\u3F79]" "\u1F79")
(test #f regexp-match #rx"[\u1F79-\u3F79]" "\u1F39")
(test #f regexp-match #rx"[\u1F79-\u3F79]" "\u1F78")

;; Nots of 3-char seqs:
(test #f regexp-match #rx"[^\u1F79-\u3F79]" "\u2F79")
(test #f regexp-match #rx"[^\u1F79-\u3F79]" "\u1F79")
(test '("\u1F39") regexp-match #rx"[^\u1F79-\u3F79]" "\u1F39")
(test '("\u1F78") regexp-match #rx"[^\u1F79-\u3F79]" "\u1F78")


;; Regexps that shouldn't parse:
(err/rt-test (regexp "[a--b\u1F78]") exn:fail?)
(err/rt-test (regexp "[a-b-c\u1F78]") exn:fail?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let Matthew perform some basic sanity checks for locale-sensitive
;; comparisons:
(define known-locale? (and (regexp-match "mflatt|matthewf" (path->string (find-system-path 'home-dir)))
			   (or (regexp-match "linux" (path->string (system-library-subpath)))
			       (eq? 'macosx (system-type)))))

(printf "Known locale?: ~a\n" known-locale?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String comparison. Relies on the default locale knowing
;;  about upper A with hat (\303\202) and lower a with hat (\303\242),
;;  and also relies on a "C" locale that can't encode those
;;  two characters. It doesn't rely on a relative order of A-hat
;;  and a-hat --- only that they're the same case-insensitively.
(when known-locale?
  (let ()
    (define (stest r comp? a b)
      (test r comp? a b)
      (test r comp? (format "xx~ayy" a) (format "xx~ayy" b))
      (test r comp? (format "x\000x~ay" a) (format "x\000x~ay" b))
      (test r comp? (format "x\000~ay" a) (format "x\000~ay" b))
      (test r comp? (format "x\000~a\000y" a) (format "x\000~a\000y" b)))
    (define (go c?)
      (stest #f string=? "A" "a")
      (stest #t string-ci=? "A" "a")
      (stest #t string-locale-ci=? "A" "a")
      (stest #f string=? "\uC2" "\uE2") 
      (stest #t string-ci=? "\uC2" "\uE2")
      (stest #f string-locale=? "\uC2" "\uE2")
      (stest (if c? #f #t) string-locale-ci=? "\uC2" "\uE2")
      (stest #f string<? "\uE2" "b")
      (stest (if c? #f #t) string-locale<? "\uE2" "b")
      (stest #t string>? "\uE2" "b")
      (stest (if c? #t #f) string-locale>? "\uE2" "b")
      (stest #t string<? "b" "\uE2")
      (stest (if c? #t #f) string-locale<? "b" "\uE2")
      (stest #f string>? "b" "\uE2")
      (stest (if c? #f #t) string-locale>? "b" "\uE2")
      (test "ABC" string-locale-upcase "aBc")
      (test (if c? "\uE2" "\uC2") string-locale-upcase "\uE2")
      (test (if c? "A\uE2\0B" "A\uC2\0B") string-locale-upcase "a\uE2\0b")
      (test (if c? "A\uE2\0\uE2\0B" "A\uC2\0\uC2\0B") string-locale-upcase "a\uE2\0\uE2\0b"))
    (go #f)
    (parameterize ([current-locale "C"])
      (go #t))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  locale<->UTF-8 conversions

(test #f bytes-open-converter "UTF-8" "nowhere")
(test #f bytes-open-converter "nowhere" "UTF-8")
(when known-locale?
  (let ([c (bytes-open-converter "UTF-8" "")])
    (test (void) bytes-close-converter c)
    (test (void) bytes-close-converter c)
    (err/rt-test (bytes-convert c #"hello") exn:application:mismatch?)))

(define (bytes-encode-... from to)
  (lambda args
    (let ([c (bytes-open-converter from to)])
      (dynamic-wind
	  void
	  (lambda ()
	    (apply bytes-convert c args))
	  (lambda ()
	    (bytes-close-converter c))))))

(define bytes-encode-locale->utf-8 (bytes-encode-... "" "UTF-8"))
(define bytes-encode-utf-8->locale (bytes-encode-... "UTF-8" ""))

;; Relies on a default locale that extends Latin-1:
(when known-locale?
  (let-values ([(s l ok?) (bytes-encode-utf-8->locale #"abc")])
    (test 3 'count l)
    (test 'complete 'ok ok?))
  (let-values ([(s l ok?) (bytes-encode-utf-8->locale #"a\300\300")])
    (test 1 'count l)
    (test 'error 'ok ok?))
  (let-values ([(s l ok?) (bytes-encode-utf-8->locale #"a\302\200")])
    (test 3 'count l)
    (test 'complete 'ok ok?))
  (test-values '(#"abcd" 4 complete) (lambda () (bytes-encode-locale->utf-8 #"abcd")))
  (test-values '(#"bcd" 3 complete) (lambda () (bytes-encode-locale->utf-8 #"abcd" 1)))
  (test-values '(#"bc" 2 complete) (lambda () (bytes-encode-locale->utf-8 #"abcd" 1 3)))
  (test-values '(#"a" 1 aborts) (lambda () (bytes-encode-locale->utf-8 #"a\302\200" 0 2)))
  (test-values '(2 2 complete) (lambda () (bytes-encode-locale->utf-8 #"abcd" 1 3 (make-bytes 2))))
  (let ([s (make-bytes 10)])
    (test-values '(2 2 complete) (lambda () (bytes-encode-locale->utf-8 #"abcd" 1 3 s 4 7)))
    (test 98 bytes-ref s 4)
    (test 99 bytes-ref s 5))
  (let ([s (make-bytes 10)])
    (test-values '(1 1 continues) (lambda () (bytes-encode-locale->utf-8 #"abcd" 1 3 s 4 5)))
    (test 98 bytes-ref s 4))
  ;; The rest relies on the "C" locale:
  (parameterize ([current-locale "C"])
    (test-values '(#"abc" 3 complete) (lambda () (bytes-encode-utf-8->locale #"abc")))
    (test-values '(#"a" 1 error) (lambda () (bytes-encode-utf-8->locale #"a\300\300")))
    (test-values '(#"ab" 2 aborts) (lambda () (bytes-encode-utf-8->locale #"ab\303")))
    ;; Well-formed, but can't be converted to "C":
    (test-values '(#"a" 1 error) (lambda () (bytes-encode-utf-8->locale #"a\303\342")))))

(err/rt-test (bytes-encode-utf-8->locale 'ok))
(err/rt-test (bytes-encode-utf-8->locale "ok"))
(err/rt-test (bytes-encode-utf-8->locale #"ok" -1))
(err/rt-test (bytes-encode-utf-8->locale #"ok" 3) exn:application:mismatch?)
(err/rt-test (bytes-encode-utf-8->locale #"ok" 1 0) exn:application:mismatch?)
(err/rt-test (bytes-encode-utf-8->locale #"ok" 1 3) exn:application:mismatch?)
(err/rt-test (bytes-encode-utf-8->locale #"ok" 1 2 'nope))
(err/rt-test (bytes-encode-utf-8->locale #"ok" 1 2 #"nope"))
(let ([s (make-string 4)])
  (err/rt-test (bytes-encode-utf-8->locale #"ok" 1 3 s -1) exn:application:mismatch?)
  (err/rt-test (bytes-encode-utf-8->locale #"ok" 1 3 s 5) exn:application:mismatch?)
  (err/rt-test (bytes-encode-utf-8->locale #"ok" 1 3 s 2 1) exn:application:mismatch?)
  (err/rt-test (bytes-encode-utf-8->locale #"ok" 1 3 s 2 7) exn:application:mismatch?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following tests are derived from SRFI-14

(define (check-all-latin-1 ? l)
  (for-each (lambda (c)
	      (test #t ? c))
	    l)
  (let loop ([i 0])
    (unless (= i 256)
      (unless (memq (integer->char i) l)
	(test #f ? (integer->char i)))
      (loop (add1 i)))))

(define (check-all-unicode ? l)
  (define (unless-in-l ? code c)
    (and (? c)
	 (not (member c l))))
  (define (qtest expect f . args)
    (unless (equal? expect (apply f args))
      (apply test expect f args)))
  (for-each (lambda (c)
	      (test #t ? c))
	    l)
  (for-each (lambda (r)
	      (if (caddr r)
		  (qtest #f unless-in-l ? (car r) (integer->char (car r)))
		  (let loop ([i (car r)])
		    (qtest #f unless-in-l  ? i (integer->char i))
		    (unless (= i (cadr r))
		      (loop (add1 i))))))
	    (make-known-char-range-list)))

(check-all-latin-1
 char-lower-case?
 '(#\a
   #\b
   #\c
   #\d
   #\e
   #\f
   #\g
   #\h
   #\i
   #\j
   #\k
   #\l
   #\m
   #\n
   #\o
   #\p
   #\q
   #\r
   #\s
   #\t
   #\u
   #\v
   #\w
   #\x
   #\y
   #\z
   #\u00B5
   #\u00DF
   #\u00E0
   #\u00E1
   #\u00E2
   #\u00E3
   #\u00E4
   #\u00E5
   #\u00E6
   #\u00E7
   #\u00E8
   #\u00E9
   #\u00EA
   #\u00EB
   #\u00EC
   #\u00ED
   #\u00EE
   #\u00EF
   #\u00F0
   #\u00F1
   #\u00F2
   #\u00F3
   #\u00F4
   #\u00F5
   #\u00F6
   #\u00F8
   #\u00F9
   #\u00FA
   #\u00FB
   #\u00FC
   #\u00FD
   #\u00FE
   #\u00FF
   ;; New definition of lower case:
   #\u00AA
   #\u00BA))

;; No upper case in latin-1
(check-all-latin-1
 (lambda (x) (and (char-lower-case? x)
		  (or (eq? x (char-upcase x))
		      (> (char->integer (char-upcase x)) 255))))
 '(#\u00B5
   #\u00DF
   #\u00FF
   #\u00AA
   #\u00BA))

;; Latin-1 uppercase:
(check-all-latin-1
 char-upper-case?
 '(#\A
   #\B
   #\C
   #\D
   #\E
   #\F
   #\G
   #\H
   #\I
   #\J
   #\K
   #\L
   #\M
   #\N
   #\O
   #\P
   #\Q
   #\R
   #\S
   #\T
   #\U
   #\V
   #\W
   #\X
   #\Y
   #\Z
   #\u00C0
   #\u00C1
   #\u00C2
   #\u00C3
   #\u00C4
   #\u00C5
   #\u00C6
   #\u00C7
   #\u00C8
   #\u00C9
   #\u00CA
   #\u00CB
   #\u00CC
   #\u00CD
   #\u00CE
   #\u00CF
   #\u00D0
   #\u00D1
   #\u00D2
   #\u00D3
   #\u00D4
   #\u00D5
   #\u00D6
   #\u00D8
   #\u00D9
   #\u00DA
   #\u00DB
   #\u00DC
   #\u00DD
   #\u00DE))

;; Extra letters (not lower or upper) in Latin-1:
(check-all-latin-1
 (lambda (c)
   (and (char-alphabetic? c)
	(not (char-upper-case? c))
	(not (char-lower-case? c))))
 '())

;; Complete titlecase list:
(check-all-unicode
 char-title-case?
 '(#\u01C5
   #\u01C8
   #\u01CB
   #\u01F2
   #\u1F88
   #\u1F89
   #\u1F8A
   #\u1F8B
   #\u1F8C
   #\u1F8D
   #\u1F8E
   #\u1F8F
   #\u1F98
   #\u1F99
   #\u1F9A
   #\u1F9B
   #\u1F9C
   #\u1F9D
   #\u1F9E
   #\u1F9F
   #\u1FA8
   #\u1FA9
   #\u1FAA
   #\u1FAB
   #\u1FAC
   #\u1FAD
   #\u1FAE
   #\u1FAF
   #\u1FBC
   #\u1FCC
   #\u1FFC))

;; Complete whitspace list:
(check-all-unicode
 char-whitespace?
 '(#\u0009
   #\u000A
   #\u000B
   #\u000C
   #\u000D
   #\u0020
   #\u0085
   #\u00A0
   #\u1680
   #\u2000
   #\u2001
   #\u2002
   #\u2003
   #\u2004
   #\u2005
   #\u2006
   #\u2007
   #\u2008
   #\u2009
   #\u200A
   ;; #\u200B --- in Unicode 4.0, this code point changed from Zs to Cf
   #\u2028
   #\u2029
   #\u202F
   #\u3000
   ;; Post SRFI-14?
   #\u205F
   ;; #\u180E --- in Unicode 7, this code point changed from Zs to Cf
   ))

;; Punctuation in Latin-1:
(check-all-latin-1
 char-punctuation?
 '(#\!
   #\"
   #\#
   #\%
   #\&
   #\'
   #\(
   #\)
   #\*
   #\,
   #\-
   #\.
   #\/
   #\:
   #\;
   #\?
   #\@
   #\[
   #\\
   #\]
   #\_
   #\{
   #\}
   #\u00A1
   #\u00A7 ;; Made punctuation in Unicode 7.0
   #\u00AB
   ;; #\u00AD ;; Treated as a control character now?
   #\u00B6 ;; Made punctuation in Unicode 7.0
   #\u00B7
   #\u00BB
   #\u00BF))

;; Symbols in Latin-1
(check-all-latin-1
 char-symbolic?
 '(#\$
   #\+
   #\<
   #\=
   #\>
   #\^
   #\`
   #\|
   #\~
   #\u00A2
   #\u00A3
   #\u00A4
   #\u00A5
   #\u00A6
   ;; #\u00A7 ;; Made punctuation in Unicode 7.0
   #\u00A8
   #\u00A9
   #\u00AC
   #\u00AE
   #\u00AF
   #\u00B0
   #\u00B1
   #\u00B4
   ;; #\u00B6 ;; Made punctuation in Unicode 7.0
   #\u00B8
   #\u00D7
   #\u00F7))

;; Complete blank list:
(check-all-unicode
 char-blank?
 '(#\u0009
   #\u0020
   #\u00A0
   #\u1680
   #\u2000
   #\u2001
   #\u2002
   #\u2003
   #\u2004
   #\u2005
   #\u2006
   #\u2007
   #\u2008
   #\u2009
   #\u200A
   ;; #\u200B --- see note above
   #\u202F
   #\u3000
   ;; Post SRFI-14?
   #\u205F
   ;; #\u180E --- in Unicode 7, this code point changed from Zs to Cf
   ))


;; Every letter, digit, punct, and symbol is graphic
(check-all-unicode
 (lambda (c)
   (and (not (char-graphic? c))
	(or (char-alphabetic? c)
	    (char-numeric? c)
	    (char-punctuation? c)
	    (char-symbolic? c))))
 null)

;; Letter, digit, punct, and symbol are distinct
(check-all-unicode
 (lambda (c)
   (> (+ (if (char-alphabetic? c) 1 0)
	 (if (char-numeric? c) 1 0)
	 (if (char-punctuation? c) 1 0)
	 (if (char-symbolic? c) 
             (if (or (char<=? #\u24B6 c #\u24E9)
                     (char<=? #\U1F130 c #\U1F189)) ;; added in Unicode 6.0
                 0 ;; Those are both alphabetic and symbolic
                 1)
             0))
      1))
 null)

(check-all-unicode
 char-iso-control?
 (append
  (let loop ([i 0])
    (if (= i #x20)
	null
	(cons (integer->char i) (loop (add1 i)))))
  (let loop ([i #x7F])
    (if (= i #xA0)
	null
	(cons (integer->char i) (loop (add1 i)))))))


;; ----------------------------------------
;; Whatever bytes->string/locale does with a bad locale, it shouldn't crash

(parameterize ([current-locale "no such locale"])
  (with-handlers ([exn:fail:contract? void]) (bytes->string/locale #"xxx"))
  (with-handlers ([exn:fail:contract? void]) (string->bytes/locale "xxx")))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  String-upcase, etc.:

(test "ABC!" string-upcase "abc!")
(test "Z\u7238Z" string-upcase "z\u7238z")
(test "STRASSE" string-upcase "Stra\xDFe")
(test "\u039A\u0391\u039F\u03A3" string-upcase "\u039A\u03b1\u03BF\u03C2")
(test "\u039A\u0391\u039F\u03A3" string-upcase "\u039A\u03b1\u03BF\u03C3")

(test "abc!" string-downcase "aBC!")
(test "z\u7238z" string-downcase "z\u7238Z")
(test "stra\xDFe" string-downcase "Stra\xDFe")
(test "\u03BA\u03b1\u03BF\u03C2" string-downcase "\u039A\u0391\u039F\u03A3")
(test "\u03C3" string-downcase "\u03A3")
(test "x\u03C2" string-downcase "X\u03A3")
(test "\u03BA\u03b1\u03BF\u03C3\u03C2" string-downcase "\u039A\u0391\u039F\u03A3\u03A3")
(test "\u03BA\u03b1\u03BF\u03C2 x" string-downcase "\u039A\u0391\u039F\u03A3 x")

(test "abc!" string-foldcase "aBC!")
(test "z\u7238z" string-foldcase "z\u7238Z")
(test "strasse" string-foldcase "Stra\xDFe")
(test "\u03BA\u03b1\u03BF\u03C3" string-foldcase "\u039A\u0391\u039F\u03A3")
(test "\u03C3" string-foldcase "\u03A3")
(test "x\u03C3" string-foldcase "X\u03A3")
(test "\u03BA\u03b1\u03BF\u03C3\u03C3" string-foldcase "\u039A\u0391\u039F\u03A3\u03A3")
(test "\u03BA\u03b1\u03BF\u03C3 x" string-foldcase "\u039A\u0391\u039F\u03A3 x")

(test "Abc!" string-titlecase "aBC!")
(test "Abc  Two" string-titlecase "aBC  twO")
(test "Abc!Two" string-titlecase "aBC!twO")
(test "Z\u7238Z" string-titlecase "z\u7238Z")
(test "Stra\xDFe" string-titlecase "stra\xDFe")
(test "Stra Sse" string-titlecase "stra \xDFe")
(test "\u039A\u03b1\u03BF\u03C2" string-titlecase "\u039A\u0391\u039F\u03A3")
(test "\u039A\u03b1\u03BF \u03A3x" string-titlecase "\u039A\u0391\u039F \u03A3x")
(test "\u03A3" string-titlecase "\u03A3")
(test "X\u03C2" string-titlecase "x\u03A3")
(test "\u039A\u03b1\u03BF\u03C3\u03C2" string-titlecase "\u039A\u0391\u039F\u03A3\u03A3")
(test "\u039A\u03b1\u03BF\u03C2 X" string-titlecase "\u039A\u0391\u039F\u03A3 x")


(report-errs)
