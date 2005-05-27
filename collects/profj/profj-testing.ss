#cs
(module profj-testing mzscheme
  
  (require (lib "compile.ss" "profj")
           (lib "parameters.ss" "profj")
           (lib "tool.ss" "profj")
           (lib "class.ss"))
  
  (define interaction-errors (make-parameter 0))
  (define execution-errors (make-parameter 0))
  (define file-errors (make-parameter 0))
  (define interaction-msgs (make-parameter null))
  (define execution-msgs (make-parameter null))
  (define file-msgs (make-parameter null))
  (define expected-error-messages (make-parameter null))

  (provide java-values-equal?)
  (define (java-values-equal? v1 v2)
    (java-equal? v1 v2 null null))
  
  ;java-equal?: 'a 'a (list 'a) (list 'a)-> bool
  (define (java-equal? v1 v2 visited-v1 visited-v2)
    (or (eq? v1 v2)
        (already-seen? v1 v2 visited-v1 visited-v2)
        (and (number? v1) (number? v2) (= v1 v2))
        (cond
          ((and (object? v1) (object? v2))
           (cond
             ((equal? "String" (send v1 my-name))
              (and (equal? "String" (send v2 my-name))
                   (equal? (send v1 get-mzscheme-string) (send v2 get-mzscheme-string))))
             ((equal? "array" (send v1 my-name))
              (and (equal? "array" (send v2 my-name))
                   (= (send v1 length) (send v2 length))
                   (let ((v1-vals (array->list v1))
                         (v2-vals (array->list v2)))
                   (andmap (lambda (x) x)
                           (map java-equal? v1-vals v2-vals 
                                (map (lambda (v) (cons v1 visited-v1)) v1-vals)
                                (map (lambda (v) (cons v2 visited-v2)) v2-vals))))))
             (else
              (and (equal? (send v1 my-name) (send v2 my-name))
                   (let ((v1-fields (send v1 field-values))
                         (v2-fields (send v2 field-values)))
                     (and (= (length v1-fields) (length v2-fields))
                          (andmap (lambda (x) x) 
                                  (map java-equal? v1-fields v2-fields 
                                       (map (lambda (v) (cons v1 visited-v1)) v1-fields)
                                       (map (lambda (v) (cons v2 visited-v2)) v2-fields)))))))))
          ((and (not (object? v1)) (not (object? v2))) (equal? v1 v2))
          (else #f))))
  
  ;array->list: java-array -> (list 'a)
  (define (array->list v)
    (letrec ((len (send v length))
             (build-up
              (lambda (c)
                (if (= c len)
                    null
                    (cons (send v access c)
                          (build-up (add1 c)))))))
      (build-up 0)))
  
  
  ;already-seen?: 'a 'a (list 'a) (list 'a)-> bool
  (define (already-seen? v1 v2 visited-v1 visited-v2)
    (cond
      ((and (null? visited-v1) (null? visited-v2)) #f)
      ((memq v1 visited-v1)
       (let ((position-v1 (get-position v1 visited-v1 0)))
         (eq? v2 (list-ref visited-v2 position-v1))))
      (else #f)))
  
  ;get-position: 'a (list 'a) int -> int
  (define (get-position v1 visited pos)
    (if (eq? v1 (car visited))
        pos
        (get-position v1 (cdr visited) (add1 pos))))
  
  ;interact-internal: symbol (list string) (list evalable-value) string type-record -> void
  (define (interact-internal level interacts vals msg type-recs)
    (for-each (lambda (ent val)
                (let ((st (open-input-string ent)))
                  (with-handlers 
                      ([exn?
                        (lambda (exn)
                          (cond 
                            ((and (eq? val 'error) (report-expected-error-messages))
                             (expected-error-messages (cons (cons msg (exn-message exn)) (expected-error-messages))))
                            ((not (eq? val 'error))
                             (interaction-errors (add1 (interaction-errors)))
                             (interaction-msgs (cons
                                                (format "Test ~a: Exception raised for ~a : ~a"
                                                        msg ent (exn-message exn)) (interaction-msgs))))))])
                    (let ((new-val (eval `(begin
                                            (require (lib "class.ss")
                                                     (prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java")))
                                            ,(compile-interactions st st type-recs level)))))
                      (unless (java-equal? (eval val) new-val null null)
                        (interaction-errors (add1 (interaction-errors)))
                        (interaction-msgs (cons (format "Test ~a: ~a evaluated to ~a instead of ~a"
                                                        msg ent new-val val) (interaction-msgs))))))))
              interacts vals))
       
  ;interact-test: symbol (list string) (list evalable-value) string |
  ;             : string stymbol (list string) (list evalable-value) string -> void
  (define interact-test
    (case-lambda
      [(level in val msg)
       (interact-internal level in val msg (create-type-record))]
      ((defn level in val msg)
       (let* ((type-recs (create-type-record))
              (def-st (open-input-string defn)))
         (with-handlers
             ([exn?
               (lambda (exn)
                 (interaction-errors (add1 (interaction-errors)))
                 (interaction-msgs (cons (format "Test ~a: Exception raised in definition : ~a"
                                                 msg (exn-message exn))
                                         (interaction-msgs))))])
           (execution? #t)
           (eval-modules (compile-java 'port 'port level #f def-st def-st type-recs))
           (interact-internal level in val msg type-recs))))))
  
  ;interact-test-java-expected: string symbol (list string) (list string) string -> void
  (define (interact-test-java-expected defn level in val msg)
    (let* ((type-recs (create-type-record))
           (def-st (open-input-string defn)))
      (with-handlers
          ([exn?
            (lambda (exn)
              (interaction-errors (add1 (interaction-errors)))
              (interaction-msgs (cons (format "Test ~a: Exception raised in definition : ~a"
                                              msg (exn-message exn))
                                      (interaction-msgs))))])
        (execution? #t)
        (eval-modules (compile-java 'port 'port level #f def-st def-st type-recs))
        (let ((vals (map (lambda (ex-val)
                           (let ((st (open-input-string ex-val)))
                             (eval `(begin (require (lib "class.ss")
                                                    (prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java")))
                                           ,(compile-interactions st st type-recs level)))))
                         val)))
          (interact-internal level in vals msg type-recs)))))
  
  (define (execute-test defn level error? msg)
    (let ((st (open-input-string defn)))
      (with-handlers
          ([exn?
            (lambda (exn)
              (cond
                ((and error? (report-expected-error-messages))
                 (expected-error-messages (cons (cons msg (exn-message exn)) (expected-error-messages))))
                ((not error?)
                 (execution-errors (add1 (execution-errors)))
                 (execution-msgs (cons
                                  (format "Test ~a : Exception-raised: ~a" msg (exn-message exn)) (execution-msgs))))))])
        (eval-modules (compile-java 'port 'port level #f st st)))))
  
  ;run-test: symbol string (U string (list string)) (U string (list string)) -> (U (list (list symbol bool string)) (list ...))
  (define (run-test level defn interact val)
    (let* ((type-recs (create-type-record))
           (def-st (open-input-string defn))
           (check-vals 
            (lambda (interact val)
              (with-handlers
                  ([exn?
                    (lambda (exn)
                      (list 'interact #f (exn-message exn)))])
                (let* ((get-val (lambda (v-st v-pe)
                                  (eval `(begin (require (lib "class.ss"))
                                                (require (prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java")))
                                                ,(compile-interactions v-st v-st type-recs level)))))
                       (i-st (open-input-string interact))
                       (v-st (open-input-string val))
                       (i-pe (lambda () (open-input-string interact)))
                       (v-pe (lambda () (open-input-string val)))
                       (given-val (get-val i-st i-pe))
                       (exp-val (get-val v-st v-pe)))
                  (list 'interact (java-equal? given-val exp-val null null) (format-java given-val #t 'field null #f 0)))))))
      (with-handlers
          ([exn?
            (lambda (exn)
              (list 'defn #f (exn-message exn)))])
        (execution? #t)
        (eval-modules (compile-java 'port 'port level #f def-st def-st type-recs))
        (cond
          ((and (pair? interact) (pair? val))
           (map check-vals interact val))
          ((and (string? interact) (string? val))
           (check-vals interact val))))))
                  
  (define (file-test file level error? msg)
    (with-handlers
        ([exn?
          (lambda (exn) 
            (unless error?
              (file-errors (add1 (file-errors)))
              (file-msgs (cons
                          (format "Test ~a :Exception-raised: ~a" msg (exn-message exn)) (file-msgs)))))])
      (eval-modules (compile-java 'file 'port level file #f #f))))
  
  (define (eval-modules modules)
    (for-each eval 
         (apply append
                (map compilation-unit-code modules))))
  
  ;prepare-for-tests: String -> void
  (define (prepare-for-tests lang-level)
    (printf "Running tests for ~a~n" lang-level)
    (interaction-errors 0)
    (interaction-msgs null)
    (execution-errors 0)
    (execution-msgs null)
    (file-errors 0)
    (file-msgs null))
  
  (define report-expected-error-messages (make-parameter #f))
  
  ;report-test-results: -> void
  (define (report-test-results)
    (when (> (interaction-errors) 0)
      (printf "~a Interaction errors occurred~n" (interaction-errors))
      (for-each (lambda (m) (printf "~a~n" m)) (interaction-msgs))
      (newline))
    (when (> (execution-errors) 0)
      (printf "~a Execution errors occurred~n" (execution-errors))
      (for-each (lambda (m) (printf "~a~n" m)) (execution-msgs))
      (newline))
    (when (> (file-errors) 0)
      (printf "~a file errors occurred~n" (file-errors))
      (for-each (lambda (m) (printf "~a~n" m)) (file-msgs))
      (newline))
    (when (report-expected-error-messages)
      (printf "Received these expected error messages:~n")
      (for-each (lambda (m) (printf "Error for test ~a : ~a~n" (car m) (cdr m))) (expected-error-messages)))
    (printf "Tests completed~n"))
  
  (provide interact-test execute-test interact-test-java-expected file-test run-test 
           report-test-results prepare-for-tests report-expected-error-messages)
  )
        