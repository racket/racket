#lang racket/base
(require ffi/unsafe/vm
         ffi/unsafe
         racket/promise)

(provide decompile-chez-procedure
         unwrap-chez-interpret-jitified
         current-can-disassemble
         current-partial-fasl
         disassemble-in-description)

(define current-can-disassemble (make-parameter #t))
(define current-partial-fasl (make-parameter #f))

(define (decompile-chez-procedure p)
  (unless (procedure? p)
    (error 'decompile-chez-procedure "not a procedure"))
  (define seen (make-hasheq))
  ((vm-primitive 'call-with-system-wind)
   (lambda ()
     (define proc ((vm-primitive 'inspect/object) p))
     (define code (proc 'code))
     (decompile-code code seen #:unwrap-body? #t))))

(define (decompile obj closure seen)
  (define type (obj 'type))
  (cond
    [(eq? type 'variable)
     null]
    [(hash-ref seen (obj 'value) #f)
     null]
    [else
     (hash-set! seen (obj 'value) #t)
     (case type
       [(code) (decompile-code obj seen)]
       [(variable)
        (decompile (obj 'ref) #f seen)]
       [(procedure)
        (decompile (obj 'code) obj seen)]
       [else null])]))

(define (decompile-value v seen)
  (decompile ((vm-primitive 'inspect/object) v) #f seen))

(define (decompile-code code seen
                        #:unwrap-body? [unwrap-body? #f])
  (define $generation (vm-eval '($primitive $generation)))
  (define $code? (vm-eval '($primitive $code?)))
  (define max-gen (vm-eval '(collect-maximum-generation)))
  (append
   (apply
    append
    (for/list ([v (in-list ((code 'reloc) 'value))]
               #:unless (and ($code? v)
                             (($generation v) . > . max-gen)))
      (decompile-value v seen)))
   (if unwrap-body?
       (decompile-code-body code)
       (list
        `(define ,(let* ([name (code 'name)])
                    (if name
                        (string->symbol
                         (if (and ((string-length name) . > . 0)
                                  (eqv? (string-ref name 0) #\[))
                             (substring name 1)
                             name))
                        '....))
           (lambda ,(arity-mask->args (code 'arity-mask))
             ,@(decompile-code-body code)))))))

(define (decompile-code-body code-obj)
  (define code-pointer-adjust 1)
  (define code-prefix-words 8)   ; see `code` in "cmacro.ss"

  (define code (code-obj 'value))

  (define bstr
    (vm-eval
     `(let ([code ',code]
            [memcpy ',(lambda (to from len)
                        (memcpy to (cast from _uintptr _pointer) len))])
        (lock-object code)
        (let* ([code-p (($primitive $object-address) code ,code-pointer-adjust)]
               [length (foreign-ref 'uptr code-p (foreign-sizeof 'uptr))]
               [body-p (+ code-p (* ,code-prefix-words (foreign-sizeof 'uptr)))]
               [bstr (make-bytevector length)])
          (memcpy bstr body-p length)
          (unlock-object code)
          bstr))))

  (append
   ;; Show source location, if any:
   (let ([s (code-obj 'source-object)])
     (if s
         (let-values ([(path line col pos)
                       (vm-eval `(let ([s ',s])
                                   (values (let* ([sfd (source-object-sfd s)])
                                             (and sfd (source-file-descriptor-path sfd)))
                                           (source-object-line s)
                                           (source-object-column s)
                                           (source-object-bfp s))))])
           (let ([path (if (srcloc? path)
                           ;; the linklet layer wraps paths as srclocs
                           (srcloc-source path)
                           path)])
             (cond
               [(not path) null]
               [(and line col) (list (format "~a:~a:~a" path line col))]
               [pos (list (format "~a:~a" path pos))]
               [else (list path)])))
         null))
   ;; Show machine/assembly code:
   (cond
     [(and (current-can-disassemble)
           (force disassemble-bytes))
      (disassemble-bytes-to-assembly bstr #:relocations ((code-obj 'reloc+offset) 'value))]
     [else
      (list (list '#%machine-code bstr))])))

(define disassemble-bytes
  (delay
    (with-handlers ([exn:fail? (lambda (exn) #f)])
      (dynamic-require 'disassemble 'disassemble-bytes))))

(define (disassemble-bytes-to-assembly bstr #:relocations [relocations '()])
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    ((force disassemble-bytes) bstr #:relocations relocations))
  (define strs (regexp-split #rx"\n" (get-output-string o)))
  (list (cons '#%assembly-code strs)))
           
(define (arity-mask->args mask)
  (cond
    [(zero? (bitwise-and mask (sub1 mask)))
     ;; single bit set
     (for/list ([i (in-range (sub1 (integer-length mask)))])
       (string->symbol (format "a~a" i)))]
    [else
     ;; multiple bits set
     'args]))

;; ----------------------------------------

;; Look for 'CODE descriptions that have bytestrings and convert to disassembled.
;; This function mutates the description.
(define (disassemble-in-description v)
  (when (and (current-can-disassemble)
             (force disassemble-bytes))
    (define ht (make-hasheq))
    (let loop ([v v])
      (cond
        [(hash-ref ht v #f)
         (void)]
        [else
         (hash-set! ht v #t)
         (cond
           [(pair? v)
            (loop (car v))
            (loop (cdr v))]
           [(vector? v)
            (when (and ((vector-length v) . >= . 8)
                       (eq? 'CODE (vector-ref v 0))
                       (bytes? (vector-ref v 7)))
              (vector-set! v 7 (disassemble-bytes-to-assembly (vector-ref v 7))))
            (for ([e (in-vector v)])
              (loop e))])])))
  v)

;; ----------------------------------------
;; The schemify interpreter's "bytecode" is fairly readable as-is, so
;; just unpack compiled procedures at the leaves

(define (unwrap-chez-interpret-jitified bc)
  (define linklet-interpret-jitified? (vm-primitive 'linklet-interpret-jitified?))
  (define linklet-interpret-jitified-extract (vm-primitive 'linklet-interpret-jitified-extract))
  (let loop ([bc bc])
    (cond
      [(linklet-interpret-jitified? bc)
       (define proc (linklet-interpret-jitified-extract bc))
       (define proc-obj ((vm-primitive 'inspect/object) proc))
       (define code (proc-obj 'code))
       `(begin . ,(decompile-code code (make-hasheq)))]
      [(vector? bc)
       (for/vector #:length (vector-length bc) ([bc (in-vector bc)])
         (loop bc))]
      [(pair? bc)
       (cons (loop (car bc)) (loop (cdr bc)))]
      [else bc])))
