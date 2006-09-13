
(module pcre mzscheme
  (require (lib "foreign.ss"))
  (unsafe!)

  (define pcre-lib (ffi-lib "libpcre"))

  (define pcre-compile 
    (get-ffi-obj "pcre_compile" pcre-lib 
                 (_fun _bytes _int _pointer _pointer _pointer 
                       -> _pointer)))
  (define pcre-study
    (get-ffi-obj "pcre_study" pcre-lib 
                 (_fun _pointer _int _pointer
                       -> _pointer)))
  (define pcre-exec
    (get-ffi-obj "pcre_exec" pcre-lib 
                 (_fun _pointer _pointer _bytes _int 
                       _int _int _bytes _int
                       -> _int)))
  
  (define (pcregexp s)
    (let* ([pat (pcre-compile s 0 #f #f #f)]
           [extra #f #;(pcre-study pat 0 #f)])
      (cons pat extra)))

  (define random-vector (make-bytes 100))

  (define (pcregexp-match re bytes)
    (pcre-exec (car re) (cdr re) bytes (bytes-length bytes)
               0 0 random-vector 10))

  (display (pcregexp-match (pcregexp #".*") #"abc")))


