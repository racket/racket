(module match-error mzscheme
  (provide (all-defined))
  
  (require (lib "pregexp.ss"))
  
  (define-struct (exn:misc:match exn:fail) (value))
  
  (define match:error
    (case-lambda
      ((val)
       (raise
        (make-exn:misc:match
         (format "match: no matching clause for ~e" val)
         (current-continuation-marks)
         val)))
      ((val expr)
       (raise
        (make-exn:misc:match
         (format "match: no matching clause for ~e: ~s" val expr)
         (current-continuation-marks)
         val)))))
  
  ;;! (function match:syntax-err
  ;;          (form (match:syntax-err object message . detail) -> void)
  ;;          (contract (any string . any) -> void)
  ;;          (example (match:syntax-err (syntax here) "Bad error" (vector))
  ;;                   -> void)
  ;;          (contract object -> (normally a syntax object that
  ;;                               that helps determine the source location
  ;;                               of the error)))
  ;; This function is used to report malformed match expressions.
  
  (define match:syntax-err (lambda (obj msg . detail)
                             (apply
                              raise-syntax-error
                              'match
                              msg
                              obj
                              detail)))
  
  (define (match:internal-err obj msg  . detail)
    (apply raise-syntax-error '|internal match error| msg obj detail))
  
  
  
  ;;!(function unreachable
  ;;          (form (unreachable plist match-expr) -> void)
  ;;          (contract (list syntax-object) -> void)
  ;;          (contract plist -> (is a list of unreached pattern clauses))
  ;;          (contract match-expr -> (is the origional match expr
  ;;                                   the clauses came from)))
  ;; This function takes a list of unreached clauses and the original
  ;; match expression and prints a warning for each of the unreached
  ;; match clauses to the current error port
  (define unreachable
    (lambda (plist match-expr)
      (map
       (lambda (x)
         (if (not (cdr x))
             (fprintf
              (current-error-port)
              "Warning: unreachable match clause ~e in ~e~n"
              (syntax-object->datum (car x))
              (syntax-object->datum match-expr))))
       plist)))
  
   ;; this makes pregexp errors a little more friendly
  (define (pregexp-match-with-error regex str)
    (if (or (string? regex)
	    (bytes? regex)
	    (regexp? regex)
	    (byte-regexp? regex))
        (pregexp-match regex str)
        (error 'match:pregex 
               (string-append 
                "this pattern expects either a string, byte string, regexp or byte regexp,"
                " given " (format "~e" regex) "; "
                "other argument was " (format "~e" str)))))
     
  
  )
