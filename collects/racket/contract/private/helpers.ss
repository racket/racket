#lang racket/base

(provide mangle-id mangle-id-for-maker
         build-struct-names
         lookup-struct-info
         nums-up-to
         add-name-prop
         all-but-last
         known-good-contract?)

(require setup/main-collects
         racket/struct-info
         (for-template racket/base))

;; lookup-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
(define (lookup-struct-info stx provide-stx)
  (let ([id (syntax-case stx ()
              [(a b) (syntax a)]
              [_ stx])])
    (let ([v (syntax-local-value id (λ () #f))])
      (if (struct-info? v)
          (extract-struct-info v)
          (raise-syntax-error 'provide/contract
                              "expected a struct name" 
                              provide-stx
                              id)))))


(define (add-name-prop name stx)
  (cond
    [(identifier? name)
     (syntax-property stx 'inferred-name (syntax-e name))]
    [(symbol? name)
     (syntax-property stx 'inferred-name name)]
    [else stx]))

;; mangle-id : syntax string syntax ... -> syntax
;; constructs a mangled name of an identifier from an identifier
;; the name isn't fresh, so `id' combined with `ids' must already be unique.
(define (mangle-id main-stx prefix id . ids)
  (datum->syntax
   #f
   (string->symbol
    (string-append
     prefix
     (format 
      "-~a~a"
      (syntax->datum id)
      (apply 
       string-append 
       (map 
        (lambda (id)
          (format "-~a" (syntax->datum id)))
        ids)))))))

(define (mangle-id-for-maker main-stx prefix id . ids)
  (let ([id-w/out-make (regexp-replace #rx"^make-" (format "~a" (syntax->datum id)) "")])
    (datum->syntax
     #f
     (string->symbol
      (string-append
       "make-"
       prefix
       (format 
        "-~a~a"
        id-w/out-make
        (apply 
         string-append 
         (map 
          (lambda (id)
            (format "-~a" (syntax->datum id)))
          ids))))))))

;; (cons X (listof X)) -> (listof X)
;; returns the elements of `l', minus the last element
;; special case: if l is an improper list, it leaves off
;; the contents of the last cdr (ie, making a proper list
;; out of the input), so (all-but-last '(1 2 . 3)) = '(1 2)
(define (all-but-last l)
  (cond
    [(null? l) (error 'all-but-last "bad input")]
    [(not (pair? l)) '()]
    [(null? (cdr l)) null]
    [(pair? (cdr l)) (cons (car l) (all-but-last (cdr l)))]
    [else (list (car l))]))

;; helper for build-src-loc-string
(define (source->name src)
  (let* ([bs (cond [(bytes? src)  src]
                   [(path? src)   (path->bytes src)]
                   [(string? src) (string->bytes/locale src)]
                   [else #f])]
         [r (and bs (path->main-collects-relative bs))])
    (and bs
         (bytes->string/locale (if (and (pair? r) (eq? 'collects (car r)))
                                   (apply bytes-append 
                                          #"<collects>" 
                                          (map (lambda (s)
                                                 (bytes-append #"/" s))
                                               (cdr r)))
                                   bs)))))

;; build-src-loc-string : (or/c srcloc syntax) -> (union #f string)
(define (build-src-loc-string stx)
  (let-values ([(source line col pos)
                (if (syntax? stx)
                    (values (source->name (syntax-source stx))
                            (syntax-line stx)
                            (syntax-column stx)
                            (syntax-position stx))
                    (if (syntax? (srcloc-source stx))
                      (values (source->name 
                               (resolved-module-path-name 
                                (module-path-index-resolve 
                                 (syntax-source-module
                                  (srcloc-source stx)))))
                              (srcloc-line stx)
                              (srcloc-column stx)
                              (srcloc-position stx))
                      (error 'contract
                             "malformed srcloc has non-syntax source: ~e"
                             stx)))])
    (let ([location (cond [(and line col) (format "~a:~a" line col)]
                          [pos (format "~a" pos)]
                          [else #f])])
      (if (and source location)
          (string-append source ":" location)
          (or location source)))))

(define build-struct-names
  (lambda (name-stx fields omit-sel? omit-set? srcloc-stx)
    (let ([name (symbol->string (syntax-e name-stx))]
          [fields (map symbol->string (map syntax-e fields))]
          [+ string-append])
      (map (lambda (s)
             (datum->syntax name-stx (string->symbol s) srcloc-stx))
           (append
            (list 
             (+ "struct:" name)
             (+ "make-" name)
             (+ name "?"))
            (let loop ([l fields])
              (if (null? l)
                  null
                  (append
                   (if omit-sel?
                       null
                       (list (+ name "-" (car l))))
                   (if omit-set?
                       null
                       (list (+ "set-" name "-" (car l) "!")))
                   (loop (cdr l))))))))))

(define (nums-up-to n)
  (let loop ([i 0])
    (cond
      [(= i n) '()]
      [else (cons i (loop (+ i 1)))])))

(define known-good-ids
  (list #'absolute-path?
        #'bound-identifier=?
        #'box?
        #'byte-pregexp?
        #'byte-regexp?
        #'byte?
        #'bytes-converter?
        #'bytes=?
        #'bytes?
        #'channel?
        #'char-alphabetic?
        #'char-blank?
        #'char-graphic?
        #'char-iso-control?
        #'char-lower-case?
        #'char-numeric?
        #'char-punctuation?
        #'char-symbolic?
        #'char-title-case?
        #'char-upper-case?
        #'char-whitespace?
        #'compiled-expression?
        #'compiled-module-expression?
        #'complete-path?
        #'continuation-mark-set?
        #'continuation-prompt-available?
        #'custodian-box?
        #'custodian-memory-accounting-available?
        #'custodian?
        #'directory-exists?
        #'ephemeron?
        #'evt?
        #'exn:break?
        #'exn:fail:contract:arity?
        #'exn:fail:contract:continuation?
        #'exn:fail:contract:divide-by-zero?
        #'exn:fail:contract:variable?
        #'exn:fail:contract?
        #'exn:fail:filesystem:exists?
        #'exn:fail:filesystem:version?
        #'exn:fail:filesystem?
        #'exn:fail:network?
        #'exn:fail:out-of-memory?
        #'exn:fail:read:eof?
        #'exn:fail:read:non-char?
        #'exn:fail:read?
        #'exn:fail:syntax?
        #'exn:fail:unsupported?
        #'exn:fail:user?
        #'exn:fail?
        #'exn?
        #'file-exists?
        #'file-stream-port?
        #'free-identifier=?
        #'handle-evt?
        #'hash-table?
        #'identifier?
        #'immutable?
        #'inspector?
        #'keyword?
        #'link-exists?
        #'module-identifier=?
        #'module-path-index?
        #'module-provide-protected?
        #'module-template-identifier=?
        #'module-transformer-identifier=?
        #'namespace?
        #'parameter-procedure=?
        #'parameter?
        #'parameterization?
        #'path-for-some-system?
        #'path-string?
        #'path?
        #'port-closed?
        #'port-provides-progress-evts?
        #'port-writes-atomic?
        #'port-writes-special?
        #'port?
        #'pregexp?
        #'primitive-closure?
        #'primitive?
        #'procedure-arity-includes?
        #'procedure-closure-contents-eq?
        #'procedure-struct-type?
        #'promise?
        #'pseudo-random-generator?
        #'regexp-match?
        #'regexp?
        #'relative-path?
        #'rename-transformer?
        #'security-guard?
        #'semaphore-try-wait?
        #'semaphore?
        #'set!-transformer?
        #'special-comment?
        #'string-locale-ci=?
        #'string-locale=?
        #'struct-accessor-procedure?
        #'struct-constructor-procedure?
        #'struct-mutator-procedure?
        #'struct-predicate-procedure?
        #'struct-type-property?
        #'struct-type?
        #'struct?
        #'subprocess?
        #'syntax-graph?
        #'syntax-original?
        #'syntax-transforming?
        #'syntax?
        #'system-big-endian?
        #'tcp-accept-ready?
        #'tcp-listener?
        #'tcp-port?
        #'terminal-port?
        #'thread-cell?
        #'thread-dead?
        #'thread-group?
        #'thread-running?
        #'thread?
        #'udp-bound?
        #'udp-connected?
        #'udp?
        #'void?
        #'weak-box?
        #'will-executor?
        #'arity-at-least?
        #'exn:srclocs?
        #'srcloc?))

(define (known-good-contract? id)
  (and (identifier? id)
       (ormap (λ (x) (free-identifier=? x id))
              known-good-ids)))
