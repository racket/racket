(module servlet-tables mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "plt-match.ss")
           "timer.ss")
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
  (provide
   match-url-params)
  (provide/contract
   [continuation-url? (url? . -> . (union boolean? (list/c symbol? number? number?)))]
   [embed-ids (symbol? number? number? url? . -> . string?)]
   [store-continuation! (procedure? procedure? url? servlet-instance? . -> . string?)]
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

  ;; store-continuation!: continuation expiration-handler uri servlet-instance -> url-string
  ;; store a continuation in a k-table for the provided servlet-instance
  (define (store-continuation! k expiration-handler uri inst)
    (let ([k-table (servlet-instance-k-table inst)])
      (let ([next-k-id (get-k-id! k-table)]
            [salt      (random 100000000)])
        (hash-table-put! k-table next-k-id (list k expiration-handler salt))
        (embed-ids (servlet-instance-id inst) next-k-id salt uri))))

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
    (hash-table-remove! instance-table (servlet-instance-id inst)))

  ;; ********************************************************************************
  ;; Parameter Embedding

  (define URL-PARAMS:REGEXP (regexp "([^\\*]*)\\*([^\\*]*)\\*([^\\*]*)"))

  (define (match-url-params x) (regexp-match URL-PARAMS:REGEXP x))

  ;; embed-ids: number number number url -> string
  (define (embed-ids inst-id k-id salt in-url)
    (insert-param
     in-url
     (format "~a*~a*~a" inst-id k-id salt)))

  ;; continuation-url?: url -> (union (list number number number) #f)
  ;; determine if this url encodes a continuation and extract the instance id and
  ;; continuation id.
  (define (continuation-url? a-url)
    (let ([k-params (filter match-url-params
                            (apply append (map path/param-param (url-path a-url))))])
      (if (empty? k-params)
          #f
          (match (match-url-params (first k-params))
            [(list s instance k-id salt)
             (let ([k-id/n (string->number k-id)]
                   [salt/n (string->number salt)])
               (if (and (number? k-id/n)
                        (number? salt/n))
                   (list (string->symbol instance)
                         k-id/n
                         salt/n)
                   ; XXX: Maybe log this in some way?
                   #f))]))))
  
  ;; insert-param: url string -> string
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url new-param-str)
    (url->string
     (replace-path
      (lambda (old-path)        
        (if (empty? old-path)
            (list (make-path/param "" (list new-param-str)))
            (list* (make-path/param (path/param-path (first old-path))
                                    (list new-param-str))
                   (rest old-path))))
      in-url)))

  ;; replace-path: (url-path -> url-path) url -> url
  ;; make a new url by replacing the path part of a url with a function
  ;; of the url's old path
  ;; also remove the query
  (define (replace-path proc in-url)
    (let ([new-path (proc (url-path in-url))])
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       (url-path-absolute? in-url)
       new-path
       empty
       (url-fragment in-url)))))