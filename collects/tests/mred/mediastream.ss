
(define out-base (make-object wx:media-stream-out-string-base%))
(define out (make-object wx:media-stream-out% out-base))

(define items (list 10 3.5 100 0 -1 -100 -3.5 "howdy"))

(define (write-all)
  (for-each
   (lambda (i)
     (send out put i))
   items))

(write-all)

(let ([start (send out tell)])
  (send out put-fixed 100)
  (write-all)
  (let ([end (send out tell)])
    (send out jump-to start)
    (send out put-fixed 99)
    (send out jump-to end)
    (send out put "End Second")))

(define file (send out-base get-string))

(define in-base (make-object wx:media-stream-in-string-base% file))
(define in (make-object wx:media-stream-in% in-base))

(define (test expected got)
  (unless (equal? expected got)
    (error 'media-stream-test "expected ~s, got ~s~n" expected got)))

(define (read-all)
  (for-each
   (lambda (i)
     (test i
	   (cond
	    [(string? i) (send in get-string)]
	    [(inexact? i) (send in get-inexact)]
	    [else (send in get-exact)])))
   items))
(read-all)
(test 99 (let ([b (box 0)])
	   (send in get-fixed b)
	   (unbox b)))
(read-all)
(test "End Second" (send in get-string))

(define example-file-name (build-path (current-load-relative-directory) "mediastream.example"))
(define expect (if (file-exists? example-file-name)
		   (with-input-from-file example-file-name
		     (lambda ()
		       (read-string (+ (string-length file) 10))))
		   (begin
		     (fprintf (current-error-port) "Warning: ~a does not exist; creating it.~n" example-file-name)
		     (with-output-to-file example-file-name
		       (lambda () (display file)))
		     file)))
(unless (string=? file expect)
  (error "generated file does not match expected file"))
