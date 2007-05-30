(module util mzscheme
  (require (lib "contract.ss")
           (lib "string.ss")
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net"))
  (require "../request-structs.ss")
  
  ;; ripped this off from url-unit.ss
  (define (url-path->string strs)
    (apply string-append
           (apply append
                  (map (lambda (s) (list "/" (maybe-join-params s)))
                       strs))))
  
  ;; needs to unquote things!
  (define (maybe-join-params s)
    (if (string? s)
        s
        (let ([s (path/param-path s)])
          (if (string? s)
              s
              (case s
                [(same) "."]
                [(up)   ".."]
                [else (error 'maybe-join-params
                             "bad value from path/param-path: ~e" s)])))))
  
  ;; network-error: symbol string . values -> void
  ;; throws a formatted exn:fail:network
  (define (network-error src fmt . args)
    (raise (make-exn:fail:network (format "~a: ~a" src (apply format fmt args))
                                  (current-continuation-marks))))
  
  ;; build-path-unless-absolute : path-string? path-string? -> path?
  (define (build-path-unless-absolute base path)
    (if (absolute-path? path)
        (build-path path)
        (build-path base path)))
  
  ;; exn->string : (or/c exn any) -> string
  (define (exn->string exn)
    (if (exn? exn)
        (parameterize ([current-error-port (open-output-string)])
          ((error-display-handler) (exn-message exn) exn)
          (get-output-string (current-error-port)))
        (format "~s\n" exn)))
  
  ; lowercase-symbol! : (or/c string bytes) -> symbol
  (define (lowercase-symbol! s)
    (let ([s (if (bytes? s)
                 (bytes->string/utf-8 s)
                 s)])
      (string-lowercase! s)
      (string->symbol s)))
   
  (define (directory-part path)
    (let-values ([(base name must-be-dir) (split-path path)])
      (cond
        [(eq? 'relative base) (current-directory)]
        [(not base) (error 'directory-part "~a is a top-level directory" path)]
        [(path? base) base])))
  
  ; to convert a platform dependent path into a listof path parts such that
  ; (forall x (equal? (path->list x) (path->list (apply build-path (path->list x)))))
  (define (path->list p)
    (let loop ([p p] [acc null])
      (let-values ([(base name must-be-dir?) (split-path p)])
        (let ([new-acc (cons name acc)])
          (cond
            [(string? base) (loop base new-acc)]
            [else ; conflate 'relative and #f
             new-acc])))))
  
  ; this is used by launchers
  ; extract-flag : sym (listof (cons sym alpha)) alpha -> alpha
  ; XXX remove
  (define (extract-flag name flags default)
    (let ([x (assq name flags)])
      (if x
          (cdr x)
          default)))
  
  (provide/contract
   [url-path->string ((listof (or/c string? path/param?)) . -> . string?)]
   [extract-flag (symbol? (listof (cons/c symbol? any/c)) any/c . -> . any/c)]
   [network-error ((symbol? string?) (listof any/c) . ->* . (void))]
   [path->list  (path? . -> . (cons/c (or/c path? (symbols 'up 'same))
                                      (listof (or/c path? (symbols 'up 'same)))))]
   [directory-part (path? . -> . path?)]
   [lowercase-symbol! ((or/c string? bytes?) . -> . symbol?)]
   [exn->string ((or/c exn? any/c) . -> . string?)]
   [build-path-unless-absolute (path-string? path-string? . -> . path?)]))