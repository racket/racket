#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "encoding.rkt"
         "utf-8.rkt")

(provide bytes-converter?
         bytes-open-converter
         bytes-close-converter
         bytes-convert
         bytes-convert-end)

(struct bytes-converter ([c #:mutable]
                         [custodian-reference #:mutable]))

;; The "-ish" variants allow unparied surrogates and the surrogates
;; encoded in the obvious extension of UTF-8. Those variants are
;; intended for converting to and from arbitrary 16-byte sequences,
;; which is useful for encoding Windows paths.
(define windows? (eq? 'windows (system-type)))
(define platform-utf-8 (if windows? 'utf-8-ish 'utf-8))
(define platform-utf-8-permissive (if windows? 'utf-8-ish-permissive 'utf-8-permissive))
(define platform-utf-16 (if windows? 'utf-16-ish 'utf-16-assume))

(define/who (bytes-open-converter from-str to-str)
  (check who string? from-str)
  (check who string? to-str)
  (cond
    [(and (string=? from-str "UTF-8") (string=? to-str "UTF-8"))
     (bytes-converter (utf-8-converter 'utf-8 'utf-8)
                      #f)]
    [(and (string=? from-str "UTF-8-permissive") (string=? to-str "UTF-8"))
     (bytes-converter (utf-8-converter 'utf-8-permissive 'utf-8)
                      #f)]
    [(and (string=? from-str "platform-UTF-8") (string=? to-str "platform-UTF-16"))
     (bytes-converter (utf-8-converter platform-utf-8 platform-utf-16)
                      #f)]
    [(and (string=? from-str "platform-UTF-8-permissive") (string=? to-str "platform-UTF-16"))
     (bytes-converter (utf-8-converter platform-utf-8-permissive platform-utf-16)
                      #f)]
    [(and (string=? from-str "platform-UTF-16") (string=? to-str "platform-UTF-8"))
     (bytes-converter (utf-8-converter platform-utf-16 platform-utf-8)
                      #f)]
    ;; "UTF-8-ish" is also known as "WTF-8".
    ;; "UTF-16-ish" is similar to UTF-16, but allows unpaired surrogates --- which is still
    ;; different from UCS-2, since paired surrogates are decoded as in UTF-16.
    [(and (string=? from-str "UTF-8-ish") (string=? to-str "UTF-16-ish"))
     (bytes-converter (utf-8-converter 'utf-8-ish 'utf-16-ish)
                      #f)]
    [(and (string=? from-str "UTF-8-ish-permissive") (string=? to-str "UTF-16-ish"))
     (bytes-converter (utf-8-converter 'utf-8-ish-permissive 'utf-16-ish)
                      #f)]
    [(and (string=? from-str "UTF-16-ish") (string=? to-str "UTF-8-ish"))
     (bytes-converter (utf-8-converter 'utf-16-ish 'utf-8-ish)
                      #f)]
    [(and (or (and (string=? from-str "UTF-8") (string=? to-str ""))
              (and (string=? from-str "") (string=? to-str "UTF-8")))
          (locale-encoding-is-utf-8?))
     (bytes-converter (utf-8-converter 'utf-8 'utf-8)
                      #f)]
    [else
     (define props (rktio_convert_properties rktio))
     (cond
       [(zero? (bitwise-and props RKTIO_CONVERTER_SUPPORTED))
        #f]
       [else
        (start-atomic)
        (check-current-custodian who)
        (define c (rktio_converter_open rktio
                                        (encoding->bytes who to-str)
                                        (encoding->bytes who from-str)))
        (cond
          [(rktio-error? c)
           (end-atomic)
           #;
           (raise-rktio-error who c "failed")
           #f]
          [else
           (define converter (bytes-converter c #f))
           (define cref (unsafe-custodian-register (current-custodian) converter close-converter #f #f))
           (set-bytes-converter-custodian-reference! converter cref)
           (end-atomic)
           converter])])]))

;; ----------------------------------------

;; in atomic mode
(define (close-converter converter)
  (define c (bytes-converter-c converter))
  (when c
    (cond
      [(utf-8-converter? c) (void)]
      [else
       (rktio_converter_close rktio c)
       (unsafe-custodian-unregister converter (bytes-converter-custodian-reference converter))])
    (set-bytes-converter-c! converter #f)))

(define/who (bytes-close-converter converter)
  (check who bytes-converter? converter)
  (atomically
   (close-converter converter)))

;; ----------------------------------------

(define/who (bytes-convert converter	 
                           src-bstr
                           [src-start-pos 0]
                           [src-end-pos (and (bytes? src-bstr) (bytes-length src-bstr))]
                           [dest-bstr #f]
                           [dest-start-pos 0]
                           [dest-end-pos (and (bytes? dest-bstr) (bytes-length dest-bstr))])
  (check who bytes-converter? converter)
  (check who bytes? src-bstr)
  (check who exact-nonnegative-integer? src-start-pos)
  (check who exact-nonnegative-integer? src-end-pos)
  (check who (lambda (d) (or (not d) (and (bytes? d) (not (immutable? d)))))
         #:contract "(or/c (and/c bytes? (not/c immutable?)) #f)" dest-bstr)
  (check who exact-nonnegative-integer? dest-start-pos)
  (check who #:or-false exact-nonnegative-integer? dest-end-pos)
  (check-range who src-start-pos src-end-pos (bytes-length src-bstr) src-bstr)
  (check-dest-range who dest-bstr dest-start-pos dest-end-pos)
  (do-convert who converter
              src-bstr src-start-pos src-end-pos
              dest-bstr dest-start-pos dest-end-pos
              (if (not dest-bstr)
                  ;; guess at needed length
                  (max 1 (- src-end-pos src-start-pos))
                  1)))

(define/who (bytes-convert-end converter
                               [dest-bstr #f]
                               [dest-start-pos 0]
                               [dest-end-pos (and (bytes? dest-bstr) (bytes-length dest-bstr))])
  (check who bytes-converter? converter)
  (check who #:or-false bytes? dest-bstr)
  (check who exact-nonnegative-integer? dest-start-pos)
  (check who #:or-false exact-nonnegative-integer? dest-end-pos)
  (check-dest-range who dest-bstr dest-start-pos dest-end-pos)
  (define-values (bstr used status)
    (do-convert who converter
                #f 0 0
                dest-bstr dest-start-pos dest-end-pos
                ;; guess at needed length
                6))
  (values bstr status))

(define (check-dest-range who dest-bstr dest-start-pos dest-end-pos)
  (cond
    [dest-bstr
     (define len (bytes-length dest-bstr))
     (check-range who dest-start-pos (or dest-end-pos len) len dest-bstr)]
    [dest-end-pos
     (unless (dest-start-pos . <= . dest-end-pos)
       (raise-arguments-error who "ending index is less than the starting index"
                              "staring index" dest-start-pos
                              "ending index" dest-end-pos))]))

;; ----------------------------------------

(define (do-convert who converter
                    src-bstr src-start-pos src-end-pos
                    dest-bstr dest-start-pos dest-end-pos
                    guess-dest-size)
  (start-atomic)
  (define c (bytes-converter-c converter))
  (unless c
    (end-atomic)
    (raise-argument-error who "converter is closed"
                          "converter" converter))
  (define use-dest-bstr (or dest-bstr
                            (make-bytes (if dest-end-pos
                                            (- dest-end-pos dest-start-pos)
                                            guess-dest-size))))
  ;; Loop for the case that `dest-bstr` and `dest-end-pos` are #f,
  ;; since we must grow output bytes as needed to consume all input
  (let loop ([use-dest-bstr use-dest-bstr]
             [src-start-pos src-start-pos]
             [use-dest-start-pos (if dest-bstr dest-start-pos 0)]
             [use-dest-end-pos (or (and dest-bstr dest-end-pos) (bytes-length use-dest-bstr))]
             [in-already-consumed 0]
             [out-already-produced 0])
    ;; Call the iconv-based converter or a utf-8-based converter:
    (define-values (in-consumed out-produced err)
      (convert-in c
                  src-bstr src-start-pos src-end-pos
                  use-dest-bstr use-dest-start-pos use-dest-end-pos))
    (cond
      [(and (eqv? err RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE)
            (not dest-bstr)
            (not dest-end-pos))
       ;; grow the output vector and try to decode more
       (define all-out-produced (+ out-produced out-already-produced))
       (define new-dest-bstr (make-bytes (* 2 (bytes-length use-dest-bstr))))
       (bytes-copy! new-dest-bstr 0 use-dest-bstr 0 all-out-produced)
       (loop new-dest-bstr
             (+ src-start-pos in-consumed)
             all-out-produced
             (bytes-length new-dest-bstr)
             (+ in-consumed in-already-consumed)
             all-out-produced)]
      [else
       ;; report results
       (define all-out-produced (+ out-produced out-already-produced))
       (end-atomic)
       (values (if dest-bstr
                   all-out-produced
                   (subbytes use-dest-bstr 0 all-out-produced))
               (+ in-already-consumed in-consumed)
               (cond
                 [(eqv? err RKTIO_ERROR_CONVERT_BAD_SEQUENCE) 'error]
                 [(eqv? err RKTIO_ERROR_CONVERT_PREMATURE_END) 'aborts]
                 [(eqv? err RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE) 'continues]
                 [else 'complete]))])))

;; in atomic mode
(define (convert-in c src src-start src-end dest dest-start dest-end)
  (cond
    [(utf-8-converter? c)
     (utf-8-convert-in c src src-start src-end dest dest-start dest-end)]
    [else
     (define r (rktio_convert_in rktio c  src src-start src-end dest dest-start dest-end))
     (define v (rktio_convert_result_to_vector r))
     (rktio_free r)
     (define in-consumed (vector-ref v 0))
     (define out-produced (vector-ref v 1))
     (define converted (vector-ref v 2))
     (define err (and (= converted RKTIO_CONVERT_ERROR)
                      (rktio_get_last_error rktio)))
     (values in-consumed out-produced err)]))
