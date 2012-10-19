#lang racket
(require ffi/unsafe/define
         ffi/unsafe)

#|
Open this program in DrRacket.
Click Run and then eval (collect-garbage) in the interaction area.
Repeat a few times, and eventually DrRacket will crash with a
segmentation fault.

No crash in 5.1, 5.1.1
Crashes in 5.2, 5.2.1, 5.3, 5.3.1.1

git bisect says introduced here: 2ada6d0e89a763f3b8523a87e580b1ffb25430eb
|#

;; NOTE: make sure file exists by running "touch $filename"
(define filename #"/tmp/my.db")

;; set NEXT-STMT? for another variation of the test, which does actually
;; seem to invoke the proper function, unlike sqlite3_close (???)
(define NEXT-STMT? #f)

(define-ffi-definer define-sqlite
  (ffi-lib "libsqlite3" '("0" #f)))

(define-cpointer-type _sqlite3_database)
(define-cpointer-type _sqlite3_statement)

(define SQLITE_OPEN_READWRITE #x00000002)

(define-sqlite sqlite3_open_v2
  (_fun (filename flags) ::
        (filename : _bytes)
        (db : (_ptr o _sqlite3_database))
        (flags : _int)
        (vfs : _pointer = #f)
        -> (result : _int)
        -> (values db result)))

(define-sqlite sqlite3_close
  (_fun _sqlite3_database
        -> _int))

(define-sqlite sqlite3_next_stmt
  (_fun _sqlite3_database _sqlite3_statement/null -> _sqlite3_statement/null))

(define c%
  (class object%
    (super-new)

    (define-values (db status)
      (sqlite3_open_v2 filename SQLITE_OPEN_READWRITE))

    (unless (zero? status)
      (eprintf "open got ~s\n" status))

    (define/public (finalize!)
      (when NEXT-STMT? (sqlite3_next_stmt db #f))
      (sqlite3_close db))
    ))

(define p (new c%))
(register-finalizer p (lambda (v) (send v finalize!)))
