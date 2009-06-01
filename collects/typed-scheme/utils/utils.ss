#lang scheme/base

#|
This file is for utilities that are of general interest, 
at least theoretically.
|#

(require (for-syntax scheme/base stxclass scheme/string)
         scheme/contract mzlib/plt-match scheme/require-syntax scheme/provide-syntax
         mzlib/struct scheme/unit
         (except-in stxclass id))

(provide with-syntax* syntax-map start-timing do-time reverse-begin printf/log
         with-logging-to-file log-file-name ==
         define-struct/printer
         (rename-out [id mk-id])
         filter-multiple
         hash-union
         in-pairs
         in-list-forever
         extend
         debug
         in-syntax
	 symbol-append
         custom-printer
	 rep utils typecheck infer env private
         hashof)

(define-syntax (define-requirer stx)
  (syntax-parse stx
    [(_ nm:id nm-out:id)
     #`(...
        (begin
          (define-require-syntax (nm stx)
            (syntax-parse stx
              [(form id:identifier ...)
               (with-syntax ([(id* ...)
                              (map (lambda (id) 
                                     (datum->syntax 
                                      id
                                      `(lib
					,(datum->syntax 
					  #f
					  (string-join
					   (list "typed-scheme" 
						 (symbol->string (syntax-e #'nm))
						 (string-append (symbol->string (syntax-e id)) ".ss"))
					   "/")
					  id id))
                                      id id))
                                   (syntax->list #'(id ...)))])
                 (syntax-property (syntax/loc stx (combine-in id* ...))
                                  'disappeared-use
                                  #'form))]))
          (define-provide-syntax (nm-out stx)
            (syntax-parse stx
              [(_ id:identifier ...)
               (with-syntax ([(id* ...)
                              (map (lambda (id) 
                                     (datum->syntax 
                                      id
                                      `(lib
					,(datum->syntax 
					  #f
					  (string-join
					   (list "typed-scheme" 
						 (symbol->string (syntax-e #'nm))
						 (string-append (symbol->string (syntax-e id)) ".ss"))
					   "/")
					  id id))))
                                   (syntax->list #'(id ...)))])
                 (syntax/loc stx (combine-out (all-from-out id*) ...)))]))
          (provide nm nm-out)))]))


(define-requirer rep rep-out)
(define-requirer infer infer-out)
(define-requirer typecheck typecheck-out)
(define-requirer utils utils-out)
(define-requirer env env-out)
(define-requirer private private-out)
(define-requirer types types-out)

(define-sequence-syntax in-syntax 
  (lambda () #'syntax->list)
  (lambda (stx)
    (syntax-case stx ()
      [[ids (_ arg)]
       #'[ids (in-list (syntax->list arg))]])))

(define-syntax debug
  (syntax-rules ()
    [(_ (f . args))
     (begin (printf "starting ~a~n" 'f)
            (let ([l (list . args)])
              (printf "arguments are:~n")
              (for/list ([arg 'args]
                         [val l])
                (printf "\t~a: ~a~n" arg val))
              (let ([e (apply f l)])
                (printf "result was ~a~n" e)
                e)))]
    [(_ . args)
     (begin (printf "starting ~a~n" 'args)
            (let ([e args])
              (printf "result was ~a~n" e)
              e))]))

(define-syntax (with-syntax* stx)
  (syntax-case stx ()
    [(_ (cl) body ...) #'(with-syntax (cl) body ...)]
    [(_ (cl cls ...) body ...)
     #'(with-syntax (cl) (with-syntax* (cls ...) body ...))]
    ))

(define (filter-multiple l . fs)
  (apply values
         (map (lambda (f) (filter f l)) fs)))

(define (syntax-map f stxl)
  (map f (syntax->list stxl)))

(define-syntax reverse-begin
  (syntax-rules () [(_ h . forms) (begin0 (begin . forms) h)]))

#;
(define-syntax define-simple-syntax
  (syntax-rules ()
    [(dss (n . pattern) template)
     (define-syntax n (syntax-rules () [(n . pattern) template]))]))

(define log-file (make-parameter #f))
(define-for-syntax logging? #f)

(require (only-in mzlib/file file-name-from-path))

(define-syntax (printf/log stx)
  (if logging?         
      (syntax-case stx ()
        [(_ fmt . args) 
         #'(when (log-file)
             (fprintf (log-file) (string-append "~a: " fmt) 
                      (file-name-from-path (object-name (log-file)))
                      . args))])
      #'(void)))

(define (log-file-name src module-name)
  (if (path? src)
      (path-replace-suffix src ".log")
      (format "~a.log" module-name)))

(define-syntax (with-logging-to-file stx)
  (syntax-case stx ()
    [(_ file . body)
     (if logging?
         #'(parameterize ([log-file (open-output-file file #:exists 'append)])
             . body)
         #'(begin . body))]))


(define-for-syntax timing? #f)

(define last-time (make-parameter #f))
(define-syntaxes (start-timing do-time)
  (if timing?        
      (values
       (syntax-rules ()
         [(_ msg)
          (begin
            (when (last-time)
              (error #f "Timing already started"))
            (last-time (current-process-milliseconds))
            (printf "Starting ~a at ~a~n" msg (last-time)))])
       (syntax-rules ()
         [(_ msg)
          (begin
            (unless (last-time)
              (start-timing msg))
            (let* ([t (current-process-milliseconds)]
                   [old (last-time)]
                   [diff (- t old)])
              (last-time t)
              (printf "Timing ~a at ~a@~a~n" msg diff t)))]))
      (values (lambda _ #'(void)) (lambda _ #'(void)))))


(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define-match-expander
  ==
  (lambda (stx)
    (syntax-case stx ()
      [(_ val)
       #'(? (lambda (x) (equal? val x)))])))

(define-for-syntax printing? #t)

(define-syntax-rule (defprinter t ...)
  (begin
    (define t (box (lambda _ (error (format "~a not yet defined" 't))))) ...
    (provide t ...)))

(defprinter
  print-type* print-filter* print-latentfilter* print-object* print-latentobject*
  print-pathelem*)

(define pseudo-printer
  (lambda (s port mode)
    (parameterize ([current-output-port port]
                   [show-sharing #f]
                   [booleans-as-true/false #f]
                   [constructor-style-printing #t])
      (newline)
      (pretty-print (print-convert s))
      (newline))))

(define custom-printer (make-parameter #t))
  
(require scheme/pretty mzlib/pconvert)

(define-syntax (define-struct/printer stx)
  (syntax-case stx ()
    [(form name (flds ...) printer)
     #`(define-struct/properties name (flds ...) 
         #,(if printing?
               #'([prop:custom-write (lambda (a b c) (if (custom-printer) (printer a b c) (pseudo-printer a b c)))]) 
               #'([prop:custom-write pseudo-printer]))
         #f)]))

(define (id kw . args)
  (define (f v)
    (cond [(string? v) v]
          [(symbol? v) (symbol->string v)]
          [(char? v) (string v)]
          [(identifier? v) (symbol->string (syntax-e v))]))
  (datum->syntax kw (string->symbol (apply string-append (map f args)))))


;; map map (key val val -> val) -> map
(define (hash-union h1 h2 f)
  (for/fold ([h* h1])
    ([(k v2) h2])
    (let* ([v1 (hash-ref h1 k #f)]
           [new-val (if v1
                        (f k v1 v2)
                        v2)])      
    (hash-set h* k new-val))))


(define (in-pairs seq)
  (make-do-sequence
   (lambda ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (lambda (e) (let ([e (gen)]) (values (car e) (cdr e))))
               (lambda (_) #t)
               #t
               (lambda (_) (more?))
               (lambda _ #t)
               (lambda _ #t))))))

(define (in-list-forever seq val)
  (make-do-sequence
   (lambda ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (lambda (e) (let ([e (if (more?) (gen) val)]) e))
               (lambda (_) #t)
               #t
               (lambda (_) #t)
               (lambda _ #t)
               (lambda _ #t))))))

;; Listof[A] Listof[B] B -> Listof[B]
;; pads out t to be as long as s
(define (extend s t extra)
  (append t (build-list (- (length s) (length t)) (lambda _ extra))))

(define-for-syntax enable-contracts? #f)
(provide (for-syntax enable-contracts?) p/c w/c cnt d-s/c d/c)

(define-syntax p/c
  (if enable-contracts?
      (make-rename-transformer #'provide/contract)
      (lambda (stx)
        (define-syntax-class clause
          #:literals ()
          #:attributes (i)
          (pattern [rename out:id in:id cnt:expr]
                   #:when (eq? (syntax-e #'rename) 'rename)
                   #:with i #'(rename-out [out in]))
          (pattern [i:id cnt:expr]))
        (syntax-parse stx
          [(_ c:clause ...)
           #'(provide c.i ...)]))))

(define-syntax w/c
  (if enable-contracts?
      (make-rename-transformer #'with-contract)
      (lambda (stx)        
        (syntax-parse stx
          [(_ name specs . body)
           #'(begin . body)]))))

(define-syntax d/c
  (if enable-contracts?
      (make-rename-transformer #'define/contract)
      (lambda (stx)        
        (syntax-parse stx
          [(_ head cnt . body)
           #'(define head . body)]))))

(define-syntax d-s/c
  (if enable-contracts?
      (make-rename-transformer #'define-struct/contract)
      (syntax-rules ()
        [(_ hd ([i c] ...) . opts)
         (define-struct hd (i ...) . opts)])))

(define-signature-form (cnt stx)
  (syntax-case stx ()
    [(_ nm cnt)
     (if enable-contracts?
         (list #'[contracted (nm cnt)])     
         (list #'nm))]))


(define (hashof k/c v/c)
  (flat-named-contract
   (format "#<hashof ~a ~a>" k/c v/c)
   (lambda (h)
     (define k/c? (if (flat-contract? k/c) (flat-contract-predicate k/c) k/c))
     (define v/c? (if (flat-contract? v/c) (flat-contract-predicate v/c) v/c))
     (and (hash? h)
          (for/and ([(k v) h])
            (and (k/c? k) 
                 (v/c? v)))))))