
; Tests image saving/loading by dumping an image
;  and loading it with every report-errs

(define dump/restore
  (lambda ()
    (printf "Dumping image...~n")
    (let ([result (write-image-to-file "tmp9")])
      (if (vector? result)
	  (printf "Continuing ~a~n" result)
	  (read-image-from-file "tmp9" #("after" "restore"))))))

(define ll null)
(define load-relative
  (lambda (f)
    (set! ll (append ll (list f)))))

(#%load-relative "all.ss")

(define load-relative #%load-relative)

(define go
  (let ([d (current-load-relative-directory)])
    (lambda ()
      (parameterize ([current-load-relative-directory d])
		    (for-each
		     (lambda (f)
		       (load-relative f)
		       (dump/restore))
		     ll)))))

(printf "Run `(go)'~n")
