#lang scheme
(require xml
         "notify.ss"
         (prefix-in ffi: (planet jaymccarthy/svn-prop)))

(define svn-path 
  (make-parameter "/opt/local/bin/svn"))

;; Running SVN w/ XML parsing
(define (svn/xml-parse . in-args)
  (define args
    (list* "--xml" in-args))     
  (define-values
    (the-process stdout stdin stderr)
    (apply
     subprocess
     #f #f #f 
     (svn-path)
     args))
  #;(notify! "Parsing SVN XML output: ~a ~a" (svn-path) args)
  (begin0
    (dynamic-wind void
                  (lambda () 
                    (with-handlers ([exn:xml? (lambda (x) x)])
                      (parameterize ([collapse-whitespace #t]
                                     [xexpr-drop-empty-attributes #t])
                        (xml->xexpr (document-element (read-xml stdout))))))
                  (lambda ()
                    (close-input-port stdout)))
    (close-output-port stdin)
    (close-input-port stderr)
    (sync the-process
          (handle-evt (alarm-evt (+ (current-inexact-milliseconds) (* 1000 2)))
                      (lambda (_)
                        (subprocess-kill the-process #t)
                        #f)))))

;; Finding out a property going towards the root
(define (sublists l)
  (if (empty? l)
      empty
      (list* l (sublists (rest l)))))

(define (svn-property-value/real working-copy-path property)
  #;(printf "propget ~a @ ~a~n" property working-copy-path)
  (with-handlers ([exn:fail? (lambda (x) 'error)])
    (ffi:svn-property-value working-copy-path property))
  #;(match (svn/xml-parse "propget" property working-copy-path)
      [(? exn:xml? x)
       'error]
      [`(properties " ")
       'none]
      [`(properties " " (target ((path ,_path)) " " (property ((name ,_prop)) ,value) " ") " ")
       value]))

(define property-cache (make-hash))
(define (svn-property-value working-copy-path property)
  (define key (cons working-copy-path property))
  (hash-ref! property-cache key
             (lambda ()
               (svn-property-value/real working-copy-path property)))
  #;(if (hash-has-key? property-cache key)
        (or (weak-box-value (hash-ref property-cache key))
            (begin (hash-remove! property-cache key)
                   (svn-property-value working-copy-path property)))
        (local [(define val (svn-property-value/real working-copy-path property))]
          (hash-set! property-cache key (make-weak-box val))
          val)))

(define (svn-property-value/root working-copy-path property)
  (define wc-path-parts (reverse (explode-path working-copy-path)))
  (define potentials (sublists wc-path-parts))
  (for/or ([potential (in-list potentials)])
    (define val (svn-property-value (path->string (apply build-path (reverse potential))) property))
    (if (string? val) val #f)))

(provide/contract
 [svn-property-value/root (path-string? string? . -> . (or/c false/c string?))])

;;; Finding out about SVN revisions

(define-struct svn-rev () #:prefab)
(define-struct (svn-rev-nolog svn-rev) () #:prefab)
(define-struct (svn-rev-log svn-rev) (num author date msg changes) #:prefab)
(define-struct svn-change (action path) #:prefab)

(define (svn-revision-log-xml rev trunk)
  (notify! "Getting log file for r~a in ~a" rev trunk)
  (svn/xml-parse 
   "log"
   "-r" rev
   "-v"
   #;"--with-all-revprops" ; v1.5
   trunk))

(define parse-log-entry
  (match-lambda
    [`(logentry ((revision ,rev)) " "
                (author ,author) " "
                (date ,date) " "
                (paths ,path ...)
                " " (msg . ,msg) " ")
     (make-svn-rev-log
      (string->number rev)
      author date (apply string-append msg) 
      (filter-map (match-lambda
                    [`(path ((action ,action) . ,any) ,file)
                     (make-svn-change (string->symbol action) file)]
                    [" " 
                     #f])
                  path))]
    [" " #f]))

(define parse-svn-log-xml
  (match-lambda
    [(? exn:fail? x)
     (fprintf (current-error-port) "Error: ~a" (exn-message x))
     #f]
    [`(log " ")
     (make-svn-rev-nolog)]
    [`(log 
       " " ,le " ")
     (parse-log-entry le)]))

(define (svn-revision-log rev trunk)
  (define rev-string
    (cond
      [(number? rev) (number->string rev)]
      [(symbol? rev)
       (case rev
         [(HEAD) "HEAD"])]))
  (parse-svn-log-xml
   (svn-revision-log-xml rev-string trunk)))

(define (svn-revision-logs-after-xml rev trunk)
  (notify! "Getting logs for revision after r~a in ~a" rev trunk)
  (svn/xml-parse 
   "log"
   "-r" (format "~a:HEAD" rev)
   "-v"
   #;"--with-all-revprops" ; v1.5
   trunk))

(define (parse-svn-logs-xml xexpr)
  (match xexpr
    [(? exn:fail? x)
     (fprintf (current-error-port) "Error: ~a" (exn-message x))
     empty]
    [`(log " ")
     empty]
    [`(log . ,les)
     (filter-map parse-log-entry les)]))

(define (svn-revision-logs-after rev trunk)
  (parse-svn-logs-xml
   (svn-revision-logs-after-xml rev trunk)))

(provide/contract
 [svn-path (parameter/c string?)]
 [svn-revision-log 
  ((or/c exact-nonnegative-integer? (symbols 'HEAD))
   string?
   . -> .
   (or/c false/c
         svn-rev?))]
 [svn-revision-logs-after 
  (exact-nonnegative-integer?
   string?
   . -> .
   (listof svn-rev-log?))]
 [struct svn-rev ()]
 [struct (svn-rev-nolog svn-rev) ()]
 [struct (svn-rev-log svn-rev)
         ([num exact-nonnegative-integer?]
          [author string?]
          [date string?]
          [msg string?]
          [changes (listof svn-change?)])]
 [struct svn-change 
         ([action symbol?]
          [path path-string?])])