#lang racket/base
(require compiler/cm)
(require racket/match)

(define prev-uncaught-exception-handler (uncaught-exception-handler))
(uncaught-exception-handler (lambda (x)
  (when (exn:break? x) (exit 1))
  (prev-uncaught-exception-handler x)))

(let ([cmc (make-caching-managed-compile-zo)]
      [worker-id (read)])
 (let loop ()
   (match (read)
     [(list 'DIE) void]
     [(list name dir file)
       (let ([dir (bytes->path dir)]
             [file (bytes->path file)])
        (let ([out-str-port (open-output-string)]
              [err-str-port (open-output-string)])
          (define (send/resp type)
            (let ([msg (list type (get-output-string out-str-port) (get-output-string err-str-port))])
              (write msg)))
          (let ([cep (current-error-port)])
            (define (pp x)
              (fprintf cep "COMPILING ~a ~a ~a ~a\n" worker-id name file x))
          (with-handlers ([exn:fail? (lambda (x)
                           (send/resp (list 'ERROR (exn-message x))))])
            (parameterize (
                           [current-namespace (make-base-empty-namespace)]
                           [current-directory dir]
                           [current-load-relative-directory dir]
                           [current-output-port out-str-port]
                           [current-error-port err-str-port]
                           ;[manager-compile-notify-handler pp]
                          )

              (cmc (build-path dir file)))
            (send/resp 'DONE))))
        (flush-output)
        (loop))])))
