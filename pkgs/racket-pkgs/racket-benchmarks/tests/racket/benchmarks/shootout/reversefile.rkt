;;; -*- mode: scheme -*-
;;; $Id: reversefile-mzscheme.code,v 1.10 2006/06/21 15:05:29 bfulgham Exp $
;;; http://shootout.alioth.debian.org/
;;; Provided by Bengt Kleberg

#lang racket/base

(let ([inport (current-input-port)])
  (let rev ([lines null])
    (let ([line (read-bytes-line inport)])
      (if (eof-object? line)
          (for-each (lambda (l) (printf "~a\n" l))
                    lines)
          (rev (cons line lines))))))
