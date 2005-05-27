(module my-url mzscheme
  (require (all-except (lib "url.ss" "net") purify-port))
  (provide purify-port
           (struct mime-header (name value)))
  ;(provide (all-from-except url purify-port))
  ;(provide (all-from-except (lib "url.ss" "net") purify-port))
  ; the -except is not needed, since purify-port was not imported.
  (provide (all-from (lib "url.ss" "net")))

  ; mime-header = (make-mime-header str str)
  (define-struct mime-header (name value))

  (define COLON:REGEXP (regexp (format "^([^:]*):[ ~a]*(.*)" #\tab)))

  ; match-colon : str -> (list str str str)
  (define (match-colon s) (regexp-match COLON:REGEXP s))

  ; purify-port : iport -> (listof mime-header)
  ; Note: this function is silently robust.  It ignores invalid input
  (define (purify-port in)
    (read-line in) ; skip HTTP/x.y NNN mumble
    (let read-headers ()
      (let ([line (read-line in 'any)])
        (cond
	  [(eof-object? line) null]
	  [(zero? (string-length line)) null]
	  [(match-colon line) =>
	   (lambda (x)
	     (cons (make-mime-header (cadr x) (caddr x))
		   (read-headers)))]
	  [else ; error - bad header
	   (read-headers)])))))
