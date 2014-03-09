#lang at-exp racket/base
(require racket/system
         racket/file
         racket/port
         racket/format
         racket/runtime-path
         file/tar)

(provide installer-sh)

(define-runtime-path installer-header "unix-installer/installer-header")

(define (system/show . l)
  (displayln (apply ~a #:separator " " l))
  (unless (apply system* (find-executable-path (car l)) (cdr l))
    (error "failed")))

(define (system/read . l)
  (displayln (apply ~a #:separator " " l))
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (apply system* (find-executable-path (car l)) (cdr l)))
  (read (open-input-bytes (get-output-bytes o))))

(define (count-lines i)
  (if (input-port? i)
      (for/sum ([l (in-lines i)]) 1)
      (call-with-input-file* i count-lines)))

(define (generate-installer-sh src-dir dest target-dir-name human-name release? readme)
  (system/show "chmod"
               "-R" "g+w" src-dir)
  (define tmp-tgz (make-temporary-file "~a.tgz"))
  (delete-file tmp-tgz)
  (printf "Tarring to ~s\n" tmp-tgz)
  (when readme
    (call-with-output-file*
     (build-path src-dir "README")
     #:exists 'truncate
     (lambda (o)
       (display readme o))))
  (parameterize ([current-directory src-dir])
    (apply tar-gzip tmp-tgz (directory-list)))
  (define tree-size (system/read "du" "-hs" src-dir))
  (define archive-cksum (system/read "cksum" tmp-tgz))
  (define script
    @~a{#!/bin/sh

         # This is a self-extracting shell script for @|human-name|.
         # To use it, just run it, or run "sh" with it as an argument.
        
         DISTNAME="@|human-name|"
         TARGET="@|target-dir-name|"
         BINSUM="@|archive-cksum|"
         ORIGSIZE="@|tree-size|"
         RELEASED="@(if release? "yes" "no")"})
  (define installer-lines (+ (count-lines (open-input-string script))
                             (count-lines installer-header)
                             2))
  (call-with-output-file* 
   dest
   #:exists 'truncate
   (lambda (o)
     (display script o)
     (newline o)
     (fprintf o "BINSTARTLINE=\"~a\"\n" installer-lines)
     (call-with-input-file*
      installer-header
      (lambda (i)
        (copy-port i o)))
     (call-with-input-file*
      tmp-tgz
      (lambda (i)
        (copy-port i o)))))
  (system/show "chmod" "+x" dest)
  (delete-file tmp-tgz))

(define (installer-sh human-name base-name dir-name release? dist-suffix readme)
  (define sh-path (format "bundle/~a-~a~a.sh" 
                          base-name 
                          (system-library-subpath #f) 
                          dist-suffix))
  (generate-installer-sh "bundle/racket" sh-path
                         dir-name human-name
                         release?
                         readme)
  sh-path)
