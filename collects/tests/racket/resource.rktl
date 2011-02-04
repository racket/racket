
;; This test modifies registry entries under Windows
;; within HKEY_CURRENT_USER\Software\PLT

(load-relative "loadtest.rktl")

(Section 'resource)

(require file/resource)

(when (eq? 'windows (system-type))
  (define key "HKEY_CURRENT_USER")
  (define (entry s) (string-append "SOFTWARE\\PLT\\" s))

  (test #t 'init (write-resource key (entry "Stuff") "Hello" #:create-key? #t))
  
  ;; A string-valued resource:
  (test #t write-resource key (entry "Stuff") "Hola")
  (test "Hola" get-resource key (entry "Stuff"))
  (test #"Hola" get-resource key (entry "Stuff") #:type 'bytes)
  (test 0 get-resource key (entry "Stuff") #:type 'integer)
  (let ([b (box "")])
    (test #t get-resource key (entry "Stuff") b)
    (test "Hola" unbox b))
  (let ([b (box #"")])
    (test #t get-resource key (entry "Stuff") b)
    (test #"Hola" unbox b))
  (let ([b (box 10)])
    (test #t get-resource key (entry "Stuff") b)
    (test 0 unbox b))

  ;; An integer-valued resource
  (test #t write-resource key (entry "Count") 17 #:type 'dword)
  (test "17" get-resource key (entry "Count"))
  (test #"17" get-resource key (entry "Count") #:type 'bytes)
  (test 17 get-resource key (entry "Count") #:type 'integer)
  (test #t write-resource key (entry "Count") -17 #:type 'dword)
  (test -17 get-resource key (entry "Count") #:type 'integer)

  ;; A bytes-valued resource:
  (test #t write-resource key (entry "Data") #"i\377mage" #:type 'bytes)
  (test "i?mage" get-resource key (entry "Data"))
  (test #"i\377mage" get-resource key (entry "Data") #:type 'bytes)
  (test 0 get-resource key (entry "Data") #:type 'integer)

  (void))

(report-errs)

  
