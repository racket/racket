
;; This test modifies registry entries under Windows
;; within HKEY_CURRENT_USER\Software\PLT

(load-relative "loadtest.rktl")

(Section 'resource)

(require file/resource)

(let ()
  (define key "HKEY_CURRENT_USER")
  (define (entry s) (string-append "SOFTWARE\\PLT\\" s))
  (define (rtest* kws kvs r . l)
    (if (eq? 'windows (system-type))
        (keyword-apply test kws kvs r l)
        (keyword-apply test kws kvs #f l)))
  (define rtest (make-keyword-procedure rtest*))
  (define (xtest r alt-r . l)
    (if (eq? 'windows (system-type))
        (apply test r l)
        (apply test alt-r l)))

  (rtest #t 'init (write-resource key (entry "Stuff") "Hello" #:create-key? #t))
  
  ;; A string-valued resource:
  (rtest #t write-resource key (entry "Stuff") "Hola")
  (rtest "Hola" get-resource key (entry "Stuff"))
  (rtest #"Hola" get-resource key (entry "Stuff") #:type 'bytes)
  (rtest 0 get-resource key (entry "Stuff") #:type 'integer)
  (let ([b (box "")])
    (rtest #t get-resource key (entry "Stuff") b)
    (xtest "Hola" "" unbox b))
  (let ([b (box #"")])
    (rtest #t get-resource key (entry "Stuff") b)
    (xtest #"Hola" #"" unbox b))
  (let ([b (box 10)])
    (rtest #t get-resource key (entry "Stuff") b)
    (xtest 0 10 unbox b))

  ;; An integer-valued resource
  (rtest #t write-resource key (entry "Count") 17 #:type 'dword)
  (rtest "17" get-resource key (entry "Count"))
  (rtest #"17" get-resource key (entry "Count") #:type 'bytes)
  (rtest 17 get-resource key (entry "Count") #:type 'integer)
  (rtest #t write-resource key (entry "Count") -17 #:type 'dword)
  (rtest -17 get-resource key (entry "Count") #:type 'integer)

  ;; A bytes-valued resource:
  (rtest #t write-resource key (entry "Data") #"i\377mage" #:type 'bytes)
  (rtest "i?mage" get-resource key (entry "Data"))
  (rtest #"i\377mage" get-resource key (entry "Data") #:type 'bytes)
  (rtest 0 get-resource key (entry "Data") #:type 'integer)

  (void))

(report-errs)

  
