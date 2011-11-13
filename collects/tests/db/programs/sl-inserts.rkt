#lang racket
(require db)

(define db (sqlite3-connect #:database 'memory))

(query-exec
 db
 "CREATE TABLE IF NOT EXISTS
        log_word_map(word TEXT, log_id INTEGER,
            PRIMARY KEY (word, log_id)
            ON CONFLICT FAIL)")

(query-exec db "BEGIN TRANSACTION")

(for ([x (in-range 20000)])
  (query-exec db
   "insert into log_word_map values (?, ?)"
   (number->string x) 99))

(query-exec db "COMMIT")
