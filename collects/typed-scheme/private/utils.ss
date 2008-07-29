#lang scheme/base

(require (for-syntax scheme/base)
         mzlib/plt-match
         mzlib/struct)

(provide with-syntax* syntax-map start-timing do-time reverse-begin printf/log
         with-logging-to-file log-file-name ==
         print-type*
         print-effect*
         define-struct/printer
         id
         filter-multiple
         hash-union
         in-pairs
         in-list-forever
         extend
         debug
         in-syntax)

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

(define print-type* (box (lambda _ (error "print-type* not yet defined"))))
(define print-effect* (box (lambda _ (error "print-effect* not yet defined"))))

(define-syntax (define-struct/printer stx)
  (syntax-case stx ()
    [(form name (flds ...) printer)
     #`(define-struct/properties name (flds ...) 
         #,(if printing? #'([prop:custom-write printer]) #'())
         #f)]))

(define (id kw . args)
  (define (f v)
    (cond [(string? v) v]
          [(symbol? v) (symbol->string v)]
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