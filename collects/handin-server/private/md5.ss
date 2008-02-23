(module md5 mzscheme
  (require (prefix mz: mzlib/md5))
  (define (md5 s)
    (bytes->string/latin-1 (mz:md5 (string->bytes/utf-8 s))))
  (provide md5))
