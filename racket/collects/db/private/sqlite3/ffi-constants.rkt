#lang racket/base
(provide (all-defined-out))

; Constants
(define SQLITE_OK           0   ) ; Successful result */
(define SQLITE_ERROR        1   ) ; SQL error or missing database */
(define SQLITE_INTERNAL     2   ) ; An internal logic error in SQLite */
(define SQLITE_PERM         3   ) ; Access permission denied */
(define SQLITE_ABORT        4   ) ; Callback routine requested an abort */
(define SQLITE_BUSY         5   ) ; The database file is locked */
(define SQLITE_LOCKED       6   ) ; A table in the database is locked */
(define SQLITE_NOMEM        7   ) ; A malloc() failed */
(define SQLITE_READONLY     8   ) ; Attempt to write a readonly database */
(define SQLITE_INTERRUPT    9   ) ; Operation terminated by sqlite3_interrupt()*/
(define SQLITE_IOERR       10   ) ; Some kind of disk I/O error occurred */
(define SQLITE_CORRUPT     11   ) ; The database disk image is malformed */
(define SQLITE_NOTFOUND    12   ) ; (Internal Only) Table or record not found */
(define SQLITE_FULL        13   ) ; Insertion failed because database is full */
(define SQLITE_CANTOPEN    14   ) ; Unable to open the database file */
(define SQLITE_PROTOCOL    15   ) ; Database lock protocol error */
(define SQLITE_EMPTY       16   ) ; Database is empty */
(define SQLITE_SCHEMA      17   ) ; The database schema changed */
(define SQLITE_TOOBIG      18   ) ; Too much data for one row of a table */
(define SQLITE_CONSTRAINT  19   ) ; Abort due to constraint violation */
(define SQLITE_MISMATCH    20   ) ; Data type mismatch */
(define SQLITE_MISUSE      21   ) ; Library used incorrectly */
(define SQLITE_NOLFS       22   ) ; Uses OS features not supported on host */
(define SQLITE_AUTH        23   ) ; Authorization denied */
(define SQLITE_FORMAT      24   ) ; Auxiliary database format error */
(define SQLITE_RANGE       25   ) ; 2nd parameter to sqlite3_bind out of range */
(define SQLITE_NOTADB      26   ) ; File opened that is not a database file */
(define SQLITE_ROW         100  ) ; sqlite3_step() has another row ready */
(define SQLITE_DONE        101  ) ; sqlite3_step() has finished executing */

;; Extended error codes:
(define SQLITE_IOERR_BLOCKED (bitwise-ior SQLITE_IOERR (arithmetic-shift 11 8)))
(define SQLITE_IOERR_LOCK    (bitwise-ior SQLITE_IOERR (arithmetic-shift 15 8)))
(define SQLITE_READONLY_ROLLBACK (bitwise-ior SQLITE_READONLY (arithmetic-shift 3 8)))

(define SQLITE_INTEGER  1)
(define SQLITE_FLOAT    2)
(define SQLITE3_TEXT    3)
(define SQLITE_TEXT    3)
(define SQLITE_BLOB     4)
(define SQLITE_NULL     5)

(define SQLITE_STATIC     0)
(define SQLITE_TRANSIENT -1)

;; Open flags

(define SQLITE_OPEN_READONLY         #x00000001)
(define SQLITE_OPEN_READWRITE        #x00000002)
(define SQLITE_OPEN_CREATE           #x00000004)

(define SQLITE_OPEN_NOMUTEX          #x00008000)
(define SQLITE_OPEN_FULLMUTEX        #x00010000)
(define SQLITE_OPEN_SHAREDCACHE      #x00020000)
(define SQLITE_OPEN_PRIVATECACHE     #x00040000)
