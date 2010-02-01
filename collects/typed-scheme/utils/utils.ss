#lang scheme/base

#|
This file is for utilities that are of general interest, 
at least theoretically.
|#

(require (for-syntax scheme/base syntax/parse scheme/string)
         scheme/contract scheme/match scheme/require-syntax 
	 scheme/provide-syntax mzlib/struct scheme/unit
	 scheme/pretty mzlib/pconvert syntax/parse)

;; to move to unstable
(provide reverse-begin)

(provide
 ;; timing
 start-timing do-time  
 ;; logging
 printf/log
 ;; struct printing
 custom-printer define-struct/printer
 ;; provide macros
 rep utils typecheck infer env private)

;; fancy require syntax
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

;; run `h' last, but drop its return value
(define-syntax-rule (reverse-begin h . forms) (begin0 (begin . forms) h))

;; conditionalized logging
;; there's some logging code in the source
;; which was used for gathering statistics about various programs
;; no longer used, probably bitrotted
(define-for-syntax logging? #f)

(define-syntax (printf/log stx)
  (if logging?         
      (syntax-case stx ()
        [(_ fmt . args) 
	 #'(log-debug (format fmt . args))])
      #'(void)))

;; some macros to do some timing, only when `timing?' is #t
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

;; custom printing
;; this requires lots of work for two reasons:
;; - 1 printers have to be defined at the same time as the structs
;; - 2 we want to support things printing corectly even when the custom printer is off

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
  
(define-syntax (define-struct/printer stx)
  (syntax-parse stx
    [(form name (flds ...) printer:expr)
     #`(define-struct name (flds ...) 
         #:property prop:custom-write 
         #,(if printing? 
               #'(lambda (a b c) (if (custom-printer) (printer a b c) (pseudo-printer a b c)))
               #'pseudo-printer)
         #:inspector #f)]))


;; turn contracts on and off - off by default for performance.
(define-for-syntax enable-contracts? #t)
(provide (for-syntax enable-contracts?) p/c w/c cnt d-s/c d/c)

;; these are versions of the contract forms conditionalized by `enable-contracts?'
(define-syntax p/c
  (if enable-contracts?
      (make-rename-transformer #'provide/contract)
      (lambda (stx)
        (define-syntax-class clause
          #:literals ()
          #:attributes (i)
          (pattern [struct nm:id (flds ...)]
                   #:fail-unless (eq? (syntax-e #'struct) 'struct) #f
		   #:with i #'(struct-out nm))
	  (pattern [rename out:id in:id cnt:expr]
                   #:fail-unless (eq? (syntax-e #'rename) 'rename) #f
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
