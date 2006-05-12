(module servlet-tables mzscheme
  (require (lib "contract.ss"))
  (require "timer.ss")
  (provide (struct exn:servlet:instance ())
           (struct exn:servlet:no-current-instance ())
           (struct exn:servlet:continuation (expiration-handler))
           (struct servlet (handler custodian namespace connection-interval-timeout instance-expiration-handler))
           (struct execution-context (connection request suspend))
           (struct servlet-instance (id k-table custodian context mutex timer))
           current-servlet-instance)

  ;; current-servlet-instance. The server will parameterize
  ;; over the current-servlet-instance before invoking a servlet
  ;; or invoking a continuation. The current-servlet-instance
  ;; will be in affect for the entire dynamic extent of every
  ;; continuation associated with that instance.
  (define current-servlet-instance (make-thread-cell #f))
  (define-struct servlet (handler custodian namespace connection-interval-timeout instance-expiration-handler))
  (define-struct servlet-instance (id k-table custodian context mutex timer))
  (define-struct execution-context (connection request suspend))

  ;; Notes:
  ;; * The servlet-instance-id is the key used for finding the servlet-instance in
  ;;   instance table.
  ;; * The servlet-instance-k-table stores continuations that were created
  ;;   during this instance.
  ;; * The servlet-instance-execution-context stores the context in which the
  ;;   instance is executing. The servlet-instance can have only one
  ;;   execution-context at any particular time. The execution-context will be
  ;;   updated whenever a continuation associated with this instance is
  ;;   invoked.
  ;; * The servlet-instance-mutex is used to guarentee mutual-exclusion in the
  ;;   case when it is attempted to invoke multiple continuations
  ;;   simultaneously.
  (provide/contract
   [store-continuation! (procedure? procedure? servlet-instance? . -> . (list/c symbol? integer? integer?))]
   [create-new-instance! (hash-table? custodian? execution-context? semaphore? timer?
                                      . -> . servlet-instance?)]
   [remove-instance! (hash-table? servlet-instance? . -> . any)]
   [clear-continuations! (servlet-instance? . -> . any)])

  ;; not found in the instance table
  (define-struct (exn:servlet:instance exn) ())
  ;; not found in the continuatin table
  (define-struct (exn:servlet:continuation exn) (expiration-handler))
  ;; not in dynamic extent of servlet
  (define-struct (exn:servlet:no-current-instance exn) ())

  (define-values (make-k-table reset-k-table get-k-id!)
    (let ([id-slot 'next-k-id])
      (values

       ;; make-k-table: -> (hash-table-of (continuation x expiration handler x salt))
       ;; Create a continuation table with an initial value for the next
       ;; continuation id.
       (lambda ()
         (let ([k-table (make-hash-table)])
           (hash-table-put! k-table id-slot 0)
           k-table))
       
       ;; reset-k-table : hash-table -> (hash-table-of (#f x expiration handler x salt ))
       ;; Remove the continuations from the k-table
       (lambda (k-table0)
         (let ([k-table1 (make-hash-table)]
               [next-id (hash-table-get k-table0 id-slot)])
           (hash-table-for-each
            k-table0
            (lambda (id v)
              (if (eq? id id-slot)
                  ; Save old next-id
                  (hash-table-put! k-table1 id v)
                  ; Replace continuations with #f
                  (hash-table-put! k-table1 id (list* #f (cdr v))))))
           k-table1))

       ;; get-k-id!: hash-table -> number
       ;; get the current-continuation id and increment the internal value
       (lambda (k-table)
         (let ([id (hash-table-get k-table id-slot)])
           (hash-table-put! k-table id-slot (add1 id))
           id)))))

  ;; store-continuation!: continuation expiration-handler servlet-instance -> (list symbol? integer? integer?)
  ;; store a continuation in a k-table for the provided servlet-instance
  (define (store-continuation! k expiration-handler inst)
    (let ([k-table (servlet-instance-k-table inst)])
      (let ([next-k-id (get-k-id! k-table)]
            [salt      (random 100000000)])
        (hash-table-put! k-table next-k-id (list k expiration-handler salt))
        (list (servlet-instance-id inst) next-k-id salt))))

  ;; clear-continuations!: servlet-instance -> void
  ;; replace the k-table for the given servlet-instance
  (define (clear-continuations! inst)
    (set-servlet-instance-k-table!
     inst
     (reset-k-table
      (servlet-instance-k-table inst))))

  ;; create-new-instance! hash-table custodian execution-context semaphore -> servlet-instance
  (define (create-new-instance! instance-table cust ctxt sema timer)
    (let* ([inst-id (string->symbol (symbol->string (gensym 'id)))]
           [inst
            (make-servlet-instance
             inst-id (make-k-table) cust ctxt sema timer)])
      (hash-table-put! instance-table inst-id inst)
      inst))

  ;; remove-instance!: hash-table servlet-instance -> void
  (define (remove-instance! instance-table inst)
    (hash-table-remove! instance-table (servlet-instance-id inst))))