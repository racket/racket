#lang racket/base
(require racket/include
         (for-syntax racket/base)
         (only-in '#%linklet primitive-table)
         "../host/place-local.rkt")

(provide rktio
         rktio-error?
         rktio-errkind
         rktio-errno
         rktio-errstep
         racket-error?
         rktio-place-init!)
;; More `provide`s added by macros below

(define rktio-table
  (or (primitive-table '#%rktio)
      (error '#%rktio "rktio not supported by host")))

(define (lookup n)
  (hash-ref rktio-table n))

(define << arithmetic-shift)

(define-syntax-rule (define-constant n v)
  (begin
    (define n v)
    (provide n)))
  
(define-syntax-rule (define-type . _) (void))
(define-syntax-rule (define-struct-type . _) (void))

(define-syntax-rule (define-function _ _ name . _)
  (begin
    (define name (lookup 'name))
    (provide name)))

(define-syntax-rule (define-function/errno _ _ _ name . _)
  (define-function () #f name))
(define-syntax-rule (define-function/errno+step _ _ _ name . _)
  (define-function () #f name))

(include "../../rktio/rktio.rktl")

(define-function () #f rktio_filesize_ref)
(define-function () #f rktio_timestamp_ref)
(define-function () #f rktio_is_timestamp)
(define-function () #f rktio_recv_length_ref)
(define-function () #f rktio_recv_address_ref)
(define-function () #f rktio_identity_to_vector)
(define-function () #f rktio_convert_result_to_vector)
(define-function () #f rktio_to_bytes)
(define-function () #f rktio_to_bytes_list)
(define-function () #f rktio_to_shorts)
(define-function () #f rktio_NULL)
(define-function () #f rktio_do_install_os_signal_handler)
(define-function () #f rktio_get_ctl_c_handler)
(define-function () #f rktio_from_bytes_list)
(define-function () #f rktio_free_bytes_list)
(define-function () #f rktio_make_sha1_ctx)
(define-function () #f rktio_make_sha2_ctx)
(define-function () #f rktio_process_result_stdin_fd)
(define-function () #f rktio_process_result_stdout_fd)
(define-function () #f rktio_process_result_stderr_fd)
(define-function () #f rktio_process_result_process)
(define-function () #f rktio_status_running)
(define-function () #f rktio_status_result)
(define-function () #f rktio_pipe_results)

;; Error results are represented as vectors:
(define rktio-error? vector?)
(define (rktio-errkind v) (vector-ref v 0))
(define (rktio-errno v) (vector-ref v 1))
(define (rktio-errstep v) (vector-ref v 2))

(define (racket-error? v errno)
  (and (eqv? (rktio-errkind v) RKTIO_ERROR_KIND_RACKET)
       (eqv? (rktio-errno v) errno)))

(define-place-local rktio (rktio_init))

(define (rktio-place-init!)
  (set! rktio (rktio_init)))

;; Only in the main place:
(void (rktio_do_install_os_signal_handler rktio))
