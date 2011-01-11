#lang racket/base
(require compiler/cm
         racket/match
         racket/fasl
         racket/serialize)

(define prev-uncaught-exception-handler (uncaught-exception-handler))
(uncaught-exception-handler (lambda (x)
  (when (exn:break? x) (exit 1))
  (prev-uncaught-exception-handler x)))

(let ([cmc (make-caching-managed-compile-zo)]
      [worker-id (deserialize (fasl->s-exp (read)))])
 (let loop ()
   (match (read)
     [(list 'DIE) void]
     [(list name dir file)
       (let ([dir (bytes->path dir)]
             [file (bytes->path file)])
        (let ([out-str-port (open-output-string)]
              [err-str-port (open-output-string)])
          (let ([cip (current-input-port)]
                [cop (current-output-port)]
                [cep (current-error-port)])
            (define (send/msg msg)
              (write msg cop)
              (flush-output cop))
            (define (send/resp type)
              (send/msg (list type (get-output-string out-str-port) (get-output-string err-str-port))))
            (define (pp x)
              (fprintf cep "COMPILING ~a ~a ~a ~a\n" worker-id name file x))
            (define (lock-client cmd fn)
              (match cmd
                ['lock 
                  (send/msg (list (list 'LOCK (path->bytes fn)) "" ""))
                  (match (read cip)
                    [(list 'locked) #t]
                    [(list 'compiled) #f])]
                ['unlock (send/msg (list (list 'UNLOCK (path->bytes fn)) "" ""))]))
            (with-handlers ([exn:fail? (lambda (x)
                             (send/resp (list 'ERROR (exn-message x))))])
              (parameterize ([parallel-lock-client lock-client]
                             [current-namespace (make-base-empty-namespace)]
                             [current-directory dir]
                             [current-load-relative-directory dir]
                             [current-input-port (open-input-string "")]
                             [current-output-port out-str-port]
                             [current-error-port err-str-port]
                             ;[manager-compile-notify-handler pp]
                            )

                (cmc (build-path dir file)))
              (send/resp 'DONE))))
        (flush-output)
        (loop))])))
