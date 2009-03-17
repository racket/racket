#lang scheme/base
(require file/gzip file/gunzip scheme/file tests/eli-tester)

(define ((io->str-op io) buf [check-ratio #f])
  (let* ([b? (bytes? buf)]
         [i (if b? (open-input-bytes buf) (open-input-string buf))]
         [o (if b? (open-output-bytes)    (open-output-string))])
    (io i o)
    (let ([res (if b? (get-output-bytes o) (get-output-string o))])
      (when check-ratio
        (if b?
          (check-ratio (bytes-length  buf) (bytes-length  res))
          (check-ratio (string-length buf) (string-length res))))
      res)))

(define deflate* (io->str-op deflate))
(define inflate* (io->str-op inflate))

(define (id* buf [ratio #f])
  (test (inflate* (deflate* buf (and ratio (lambda (i o)
                                             (test (< (/ o i) ratio))))))
        => buf))

(define (test-big-file)
  (define big-file
    (build-path (collection-path "drscheme/private") "unit.ss"))
  ;; should be around 6 times smaller
  (id* (file->bytes big-file) 4))

(define (run-tests)
  (define (rand-bytes)
    (list->bytes (for/list ([j (in-range (random 1000))]) (random 256))))
  (test-big-file)
  (for ([i (in-range 100)]) (id* (rand-bytes))))

(provide tests)
(define (tests) (test do (run-tests)))


#|

;; ELI: These are the old tests; I think that the only thing that
;; should be added from this to the above is trying the file-related
;; functionality (check that the filename is kept etc).

(require mzlib/deflate
	 mzlib/inflate)

(for-each (lambda (f)
	    (when (file-exists? f)
	      (printf "trying ~a~n" f)
	      (let ([str
		     (call-with-input-file f
		       (lambda (p)
			 (let-values ([(in out) (make-pipe 4096)]
				      [(out2) (open-output-bytes)])
			   (thread
			    (lambda ()
			      (gzip-through-ports p out "x" 0)
			      (close-output-port out)))
			   (thread-wait
			    (thread
			     (lambda ()
			       (gunzip-through-ports in out2)
			       (close-output-port out2))))
			   (get-output-bytes out2))))])
		(let ([orig-str (call-with-input-file f
				  (lambda (p)
				    (read-bytes (file-size f) p)))])
		  (unless (bytes=? str orig-str)
		    (printf "not the same for ~a" f))))))
	  (directory-list))


#|

;; Uses (unix) `gzip' program from your path.
;; Run this in a directory with lots of files to use as tests

(require mzlib/deflate
	 mzlib/process)

(define (check-file/fastest p in)
  (let ([s1 (make-string 5000)]
	[s2 (make-string 5000)])
    (let loop ([leftover 0][startpos 0][pos 0])
      (let* ([n1 (if (zero? leftover)
		     (read-string-avail! s1 p)
		     leftover)]
	     [n2 (read-string-avail! s2 in 0 (if (eof-object? n1)
						 1
						 n1))])
	(unless (if (or (eof-object? n1)
			(eof-object? n2))
		    (and (eof-object? n1)
			 (eof-object? n2))
		    (if (= n2 n1 5000)
			(string=? s1 s2)
			(string=? (substring s1 startpos (+ startpos n2))
				  (substring s2 0 n2))))
	  (error 'check "failed at ~a (~a@~a ~a)" pos n1 startpos n2))
	(unless (eof-object? n1)
	  (loop (- n1 n2) 
		(if (= n1 n2)
		    0
		    (+ startpos n2))
		(+ pos n2)))))))

(define gzip (find-executable-path "gzip" #f))
(unless gzip
  (error "cannot find gzip"))

(for-each
 (lambda (f)
   (when (file-exists? f)
     (printf "trying ~a~n" f)
     (let-values ([(zo zi zn ze zf) 
		   (apply values (process* gzip "-c" f))]
		  [(mi mo) (make-pipe 4096)])
     (close-output-port zi)
       (close-input-port ze)
       (thread 
	(lambda ()
	  (let ([p (open-input-file f)]
		[gz (lambda (p mo)
		      (gzip-through-ports p mo 
					  (let-values ([(base name dir?) (split-path f)])
					    name)
					  (file-or-directory-modify-seconds f)))])
	    (gz p mo)
	    (close-output-port mo))))
       ;; Compare output
       (check-file/fastest mi zo)
       (close-input-port zo))))
 (directory-list))

|#

|#
