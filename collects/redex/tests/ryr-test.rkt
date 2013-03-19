#lang racket/base
(require net/url 
         racket/port 
         racket/file
         file/gunzip
         file/untar)

(define tmp-dir (make-temporary-file "ryr-test-~a" 'directory))
(current-directory tmp-dir)

(printf "downloading models.tar.gz to ~a\n" tmp-dir)
(call-with-output-file "models.tar.gz"
  (λ (out-port)
    (call/input-url
     (string->url "http://www.eecs.northwestern.edu/~robby/lightweight-metatheory/models.tar.gz")
     get-pure-port
     (λ (in-port)
       (copy-port in-port out-port)))))
(gunzip "models.tar.gz")
(untar "models.tar")

(define racket-files
  (sort (for/list ([file (in-directory "models")]
                   #:when (regexp-match #rx"rkt$" (path->string file))
                   #:unless (regexp-match #rx"/[.]_" (path->string file)))
          file)
        string<=?
        #:key path->string))

(for ([file (in-list racket-files)])
  (printf "running ~a\n" file)
  (flush-output)
  (let/ec k
    (parameterize ([error-escape-handler (λ args (k (void)))])
      (dynamic-require file #f)))
  (flush-output (current-error-port))
  (flush-output))

(delete-directory/files tmp-dir)
