(library (rumble)
  (export version
          banner

          null eof void void?

          begin0
          $value

          letrec*/names

          dynamic-wind
          call-with-current-continuation
          call-with-composable-continuation
          call-with-escape-continuation
          continuation?

          make-continuation-prompt-tag
          continuation-prompt-tag?
          default-continuation-prompt-tag
          unsafe-root-continuation-prompt-tag
          call-with-continuation-prompt
          call-with-continuation-barrier
          abort-current-continuation
          continuation-prompt-available?
          impersonate-prompt-tag
          chaperone-prompt-tag
          (rename [break-enabled-key rumble:break-enabled-key])
          set-break-enabled-transition-hook! ; not exported to Racket
          unsafe-abort-current-continuation/no-wind
          unsafe-call-with-composable-continuation/no-wind

          with-continuation-mark
          (rename [call-with-immediate-continuation-mark/inline
                   call-with-immediate-continuation-mark]
                  [call-with-immediate-continuation-mark
                   call-with-immediate-continuation-mark/proc])
          continuation-mark-set-first
          continuation-mark-set->list
          continuation-mark-set->list*
          continuation-mark-set->context
          current-continuation-marks
          (rename [continuation-marks rumble:continuation-marks]) ; wrapped at threads layer
          continuation-mark-set?
          make-continuation-mark-key
          continuation-mark-key?
          impersonate-continuation-mark-key
          chaperone-continuation-mark-key
          call-with-system-wind ; not exported to Racket

          make-engine
          engine-block
          engine-timeout
          engine-return
          current-engine-state  ; not exported to Racket
          set-ctl-c-handler! ; not exported to Racket
          get-ctl-c-handler  ; not exported to Racket
          set-scheduler-lock-callbacks! ; not exported to Racket
          set-scheduler-atomicity-callbacks! ; not exported to Racket
          set-engine-exit-handler! ; not exported to Racket

          make-thread-cell
          thread-cell?
          thread-cell-ref
          thread-cell-set!
          current-preserved-thread-cell-values
          thread-cell-values?

          parameterization-key
          make-parameter
          make-derived-parameter
          parameter?
          extend-parameterization
          parameterization?
          parameter-procedure=?
          reparameterize

          raise
          error-print-width
          error-value->string-handler
          error-print-context-length
          exception-handler-key
          uncaught-exception-handler
          error-display-handler
          error-escape-handler
          register-linklet-instantiate-continuation! ; not exported to Racket
          set-error-display-eprintf! ; not exported to Racket
          set-log-system-message! ; not exported to Racket

          current-inspector
          make-inspector
          make-sibling-inspector
          current-code-inspector

          struct:exn exn exn? exn-message exn-continuation-marks
          struct:exn:break exn:break exn:break? exn:break-continuation
          struct:exn:break:hang-up exn:break:hang-up exn:break:hang-up?
          struct:exn:break:terminate exn:break:terminate exn:break:terminate?
          struct:exn:fail exn:fail exn:fail?
          struct:exn:fail:contract exn:fail:contract exn:fail:contract?
          struct:exn:fail:contract:arity exn:fail:contract:arity exn:fail:contract:arity?
          struct:exn:fail:contract:divide-by-zero exn:fail:contract:divide-by-zero exn:fail:contract:divide-by-zero?
          struct:exn:fail:contract:non-fixnum-result exn:fail:contract:non-fixnum-result exn:fail:contract:non-fixnum-result?
          struct:exn:fail:contract:continuation exn:fail:contract:continuation exn:fail:contract:continuation?
          struct:exn:fail:contract:variable exn:fail:contract:variable exn:fail:contract:variable? exn:fail:contract:variable-id
          struct:exn:fail:read exn:fail:read exn:fail:read? exn:fail:read-srclocs
          struct:exn:fail:read:eof exn:fail:read:eof exn:fail:read:eof?
          struct:exn:fail:read:non-char exn:fail:read:non-char exn:fail:read:non-char?
          struct:exn:fail:filesystem exn:fail:filesystem exn:fail:filesystem?
          struct:exn:fail:filesystem:exists exn:fail:filesystem:exists exn:fail:filesystem:exists?
          struct:exn:fail:filesystem:version exn:fail:filesystem:version exn:fail:filesystem:version?
          struct:exn:fail:filesystem:errno exn:fail:filesystem:errno exn:fail:filesystem:errno? exn:fail:filesystem:errno-errno
          struct:exn:fail:network exn:fail:network exn:fail:network?
          struct:exn:fail:network:errno exn:fail:network:errno exn:fail:network:errno? exn:fail:network:errno-errno
          struct:exn:fail:out-of-memory exn:fail:out-of-memory exn:fail:out-of-memory?
          struct:exn:fail:unsupported exn:fail:unsupported exn:fail:unsupported?
          struct:exn:fail:user exn:fail:user exn:fail:user?

          struct:srcloc srcloc srcloc?
          srcloc-source srcloc-line srcloc-column srcloc-position srcloc-span
          prop:exn:srclocs exn:srclocs? exn:srclocs-accessor
          unsafe-make-srcloc

          struct:date date? date make-date
          date-second date-minute date-hour date-day date-month date-year
          date-week-day date-year-day date-dst? date-time-zone-offset

          struct:date* date*? date* make-date*
          date*-nanosecond date*-time-zone-name

          struct:arity-at-least arity-at-least arity-at-least?
          arity-at-least-value

          prop:procedure
          prop:incomplete-arity
          prop:method-arity-error
          prop:arity-string
          apply
          procedure?
          procedure-specialize
          |#%app|
          |#%call-with-values|
          extract-procedure ; not exported to Racket
          procedure-arity-includes?
          procedure-arity
          procedure-arity-mask
          procedure-result-arity
          procedure-extract-target
          procedure-closure-contents-eq?
          procedure-reduce-arity
          procedure-reduce-arity-mask
          procedure-rename
          procedure->method
          procedure-arity?
          prop:checked-procedure
          checked-procedure-check-and-extract
          primitive?
          primitive-closure?
          primitive-result-arity
          make-jit-procedure ; not exported to racket
          |#%name|           ; not exported to racket
          |#%method-arity|   ; not exported to racket

          equal?
          equal?/recur

          impersonator?
          chaperone?
          impersonator-of?
          chaperone-of?
          impersonator-val ; not exported to Racket
          impersonate-ref ; not exported to Racket
          impersonate-set! ; not exported to Racket
          impersonator-property?
          make-impersonator-property
          impersonator-property-accessor-procedure?
          impersonator-ephemeron
          prop:impersonator-of
          (rename [strip-impersonator unsafe-strip-impersonator] ; not exported to Racket
                  [prop:authentic-override prop:unsafe-authentic-override])  ; not exported to Racket

          impersonate-procedure
          chaperone-procedure
          impersonate-procedure*
          chaperone-procedure*
          procedure-impersonator*?
          impersonator-prop:application-mark
          unsafe-impersonate-procedure
          unsafe-chaperone-procedure

          raise-argument-error
          raise-arguments-error
          raise-result-error
          raise-mismatch-error
          raise-range-error
          raise-arity-error
          raise-arity-mask-error
          raise-result-arity-error
          raise-type-error
          raise-binding-result-arity-error ; not exported to Racket

          (rename [make-unquoted-printing-string unquoted-printing-string])
          unquoted-printing-string?
          unquoted-printing-string-value

          make-struct-type-property
          struct-type-property?
          struct-type-property-accessor-procedure?
          make-struct-type
          struct-type-install-properties! ; not exported to Racket
          structure-type-lookup-prefab-uid ; not exported to Racket
          make-struct-field-accessor
          make-struct-field-mutator
          struct-type-constructor-add-guards ; not exported to Racket
          register-struct-constructor! ; not exported to Racket
          register-struct-predicate! ; not exported to Racket
          register-struct-field-accessor! ; not exported to Racket
          register-struct-field-mutator! ; not exported to Racket
          struct-property-set!  ; not exported to Racket
          struct-constructor-procedure?
          struct-predicate-procedure?
          struct-accessor-procedure?
          struct-mutator-procedure?
          struct?
          struct-type?
          procedure-struct-type?
          struct-type-info
          struct-info
          struct-type-make-constructor
          struct-type-make-predicate
          struct->vector
          prefab-key?
          prefab-struct-key
          prefab-key->struct-type
          make-prefab-struct
          prop:authentic
          prop:equal+hash
          inspector?
          inspector-superior?
          impersonate-struct
          chaperone-struct
          chaperone-struct-unsafe-undefined
          prop:chaperone-unsafe-undefined
          chaperone-struct-type

          prop:object-name
          object-name

          eq-hash-code
          eqv-hash-code
          equal-hash-code
          equal-secondary-hash-code

          hash hasheqv hasheq
          make-hash make-hasheqv make-hasheq
          make-immutable-hash make-immutable-hasheqv make-immutable-hasheq
          make-weak-hash make-weak-hasheq make-weak-hasheqv
          hash-ref hash-set hash-set! hash-remove hash-remove!
          hash-for-each hash-map hash-copy hash-clear hash-clear!
          hash-iterate-first hash-iterate-next
          hash-iterate-key hash-iterate-value
          hash-iterate-key+value hash-iterate-pair
          unsafe-immutable-hash-iterate-first unsafe-immutable-hash-iterate-next
          unsafe-immutable-hash-iterate-key unsafe-immutable-hash-iterate-value
          unsafe-immutable-hash-iterate-key+value unsafe-immutable-hash-iterate-pair
          unsafe-mutable-hash-iterate-first unsafe-mutable-hash-iterate-next
          unsafe-mutable-hash-iterate-key unsafe-mutable-hash-iterate-value
          unsafe-mutable-hash-iterate-key+value unsafe-mutable-hash-iterate-pair
          unsafe-weak-hash-iterate-first unsafe-weak-hash-iterate-next
          unsafe-weak-hash-iterate-key unsafe-weak-hash-iterate-value
          unsafe-weak-hash-iterate-key+value unsafe-weak-hash-iterate-pair

          hash? hash-eq? hash-equal? hash-eqv? hash-weak? immutable-hash?
          hash-count
          hash-keys-subset?
          eq-hashtable->hash   ; not exported to racket
          hash->eq-hashtable   ; not exported to racket

          datum-intern-literal
          set-intern-regexp?!  ; not exported to racket

          impersonate-hash
          chaperone-hash

          true-object?

          bytes shared-bytes
          bytes?
          bytes-length
          make-bytes make-shared-bytes
          bytes-ref bytes-set!
          bytes->list list->bytes
          bytes->immutable-bytes
          bytes-copy! bytes-copy bytes-fill!
          bytes=? bytes<? bytes>?
          bytes-append
          subbytes

          make-string
          string-copy!
          substring

          char-blank?
          char-iso-control?
          char-punctuation?
          char-graphic?
          char-symbolic?
          interned-char?
          make-known-char-range-list
          char-general-category

          gensym
          symbol-interned?
          symbol-unreadable?
          string->uninterned-symbol
          string->unreadable-symbol
          symbol->string

          list?
          list-pair?
          (rename [|#%map| map]
                  [|#%for-each| for-each]
                  [|#%andmap| andmap]
                  [|#%ormap| ormap])

          vector?
          mutable-vector?
          make-vector
          (rename [inline:vector-length vector-length]
                  [inline:vector-ref vector-ref]
                  [inline:vector-set! vector-set!])
          vector-copy
          vector-copy!
          vector-immutable
          vector->values
          vector-fill!
          vector->immutable-vector
          vector->list
          vector*-length
          vector*-ref
          vector*-set!

          impersonate-vector
          impersonate-vector*
          chaperone-vector
          chaperone-vector*
          unsafe-impersonate-vector
          unsafe-chaperone-vector

          box?
          (rename [inline:unbox unbox]
                  [inline:set-box! set-box!])
          unbox* set-box*!
          make-weak-box weak-box? weak-box-value
          impersonate-box
          chaperone-box
          unbox/check-undefined    ; not exported to Racket
          set-box!/check-undefined ; not exported to Racket

          immutable?

          keyword?
          keyword->string
          string->keyword
          keyword<?

          symbol<?

          exact-integer?
          exact-nonnegative-integer?
          exact-positive-integer?
          inexact-real?
          byte?
          double-flonum?
          single-flonum?
          real->double-flonum
          real->single-flonum
          arithmetic-shift
          integer-sqrt
          integer-sqrt/remainder
          integer->integer-bytes
          integer-bytes->integer
          real->floating-point-bytes
          floating-point-bytes->real
          system-big-endian?
          string->number
          number->string
          quotient/remainder
          fx->fl
          fxrshift
          fxlshift
          fl->fx
          ->fl
          fl->exact-integer
          flreal-part
          flimag-part
          make-flrectangular
          gcd
          lcm

          random
          random-seed
          pseudo-random-generator?
          make-pseudo-random-generator
          current-pseudo-random-generator
          vector->pseudo-random-generator
          vector->pseudo-random-generator!
          pseudo-random-generator->vector
          pseudo-random-generator-vector?

          mpair?
          mcons
          (rename [inline:mcar mcar]
                  [inline:mcdr mcdr]
                  [inline:set-mcar! set-mcar!]
                  [inline:set-mcdr! set-mcdr!])

          flvector?
          (rename [new-flvector flvector])
          make-flvector
          flvector-length
          flvector-ref
          flvector-set!
          flvector-copy
          shared-flvector
          make-shared-flvector
          unsafe-flvector-length
          unsafe-flvector-set!
          unsafe-flvector-ref

          shared-fxvector
          make-shared-fxvector

          correlated?
          correlated-source
          correlated-line
          correlated-column
          correlated-position
          correlated-span
          correlated-e
          correlated->datum
          datum->correlated
          correlated-property
          correlated-property-symbol-keys

          make-reader-graph
          make-placeholder
          placeholder?
          placeholder-set!
          placeholder-get
          hash-placeholder?
          make-hash-placeholder
          make-hasheq-placeholder
          make-hasheqv-placeholder

          time-apply
          current-inexact-milliseconds
          current-milliseconds
          current-gc-milliseconds
          current-seconds
          seconds->date

          collect-garbage
          current-memory-use
          dump-memory-stats
          phantom-bytes?
          make-phantom-bytes
          set-phantom-bytes!
          set-garbage-collect-notify!             ; not exported to Racket
          set-reachable-size-increments-callback! ; not exported to Racket
          set-custodian-memory-use-proc!          ; not exported to Racket
          set-immediate-allocation-check-proc!    ; not exported to Racket
          unsafe-add-collect-callbacks
          unsafe-remove-collect-callbacks

          ;; not the same as Racket will executors:
          (rename
           [make-will-executor rumble:make-will-executor]
           [make-stubborn-will-executor rumble:make-stubborn-will-executor]
           [will-executor? rumble:will-executor?]
           [will-register rumble:will-register]
           [will-try-execute rumble:will-try-execute])
          poll-will-executors ; not exported to Racket

          make-ephemeron
          ephemeron?
          ephemeron-value

          system-type
          system-path-convention-type
          system-library-subpath-string ; not exported to Racket
          set-get-machine-info!         ; not exported to Racket
          set-cross-mode!               ; not exported to Racket

          unsafe-car
          unsafe-cdr
          unsafe-list-tail
          unsafe-list-ref
          unsafe-cons-list

          unsafe-char=?
          unsafe-char<?
          unsafe-char>?
          unsafe-char>=?
          unsafe-char<=?
          unsafe-char->integer

          unsafe-fx+
          unsafe-fx-
          unsafe-fx*
          unsafe-fxquotient
          unsafe-fxremainder
          unsafe-fxmodulo
          unsafe-fxabs
          unsafe-fxand
          unsafe-fxior
          unsafe-fxxor
          unsafe-fxnot
          unsafe-fxrshift
          unsafe-fxlshift

          unsafe-fx=
          unsafe-fx<
          unsafe-fx>
          unsafe-fx>=
          unsafe-fx<=
          unsafe-fxmin
          unsafe-fxmax

          unsafe-fl+
          unsafe-fl-
          unsafe-fl*
          unsafe-fl/
          unsafe-flabs

          unsafe-fl=
          unsafe-fl<
          unsafe-fl>
          unsafe-fl>=
          unsafe-fl<=
          unsafe-flmin
          unsafe-flmax

          unsafe-fl->fx
          unsafe-fx->fl

          unsafe-make-flrectangular
          unsafe-flreal-part
          unsafe-flimag-part

          unsafe-flround
          unsafe-flfloor
          unsafe-flceiling
          unsafe-fltruncate

          unsafe-flsin
          unsafe-flcos
          unsafe-fltan
          unsafe-flasin
          unsafe-flacos
          unsafe-flatan
          unsafe-fllog
          unsafe-flexp
          unsafe-flsqrt
          unsafe-flexpt

          unsafe-flrandom

          extfl* extfl+ extfl- ->extfl
          extfl->exact extfl->exact-integer
          extfl->floating-point-bytes extfl->fx
          extfl->inexact
          extfl/ extfl< extfl<= extfl= extfl> extfl>=
          extflabs extflacos extflasin extflatan extflceiling
          extflcos extflexp extflexpt floating-point-bytes->extfl
          extflfloor fx->extfl extfllog make-shared-extflvector
          make-extflvector extflmax extflmin extflonum-available?
          extflonum? real->extfl extflround shared-extflvector
          extflsin extflsqrt extfltan extfltruncate extflvector
          extflvector-length extflvector-ref extflvector-set! extflvector?

          unsafe-extfl* unsafe-extfl+ unsafe-extfl- unsafe-extfl/
          unsafe-extfl< unsafe-extfl<= unsafe-extfl= unsafe-extfl> unsafe-extfl>=
          unsafe-extflabs unsafe-extflmax unsafe-extflmin
          unsafe-extfl->fx unsafe-fx->extfl unsafe-extflsqrt
          unsafe-extflvector-length unsafe-extflvector-ref unsafe-extflvector-set!

          set-start-place!           ; not exported to Racket
          fork-place                 ; not exported to Racket
          start-place                ; not exported to Racket
          place-enabled?
          place-shared?
          unsafe-get-place-table
          unsafe-make-place-local unsafe-place-local-ref unsafe-place-local-set!
          place-local-register-ref   ; not exported to Racket
          place-local-register-set!  ; not exported to Racket
          place-local-register-init! ; not exported to Racket
          place-exit                 ; not exported to Racket
          current-place-roots        ; not exported to Racket

          _bool _bytes _short_bytes _double _double* _fixint _fixnum _float _fpointer _gcpointer
          _int16 _int32 _int64 _int8 _longdouble _pointer _scheme _stdbool _void
          _string/ucs-4 _string/utf-16 _symbol _ufixint _ufixnum _uint16 _uint32 _uint64 _uint8
          compiler-sizeof cpointer-gcable? cpointer-tag cpointer?
          ctype-alignof ctype-basetype ctype-c->scheme ctype-scheme->c ctype-sizeof ctype?
          end-stubborn-change extflvector->cpointer
          ffi-call ffi-call-maker ffi-callback ffi-callback-maker ffi-callback?
          ffi-lib-name ffi-lib? ffi-obj ffi-obj-lib
          ffi-obj-name  ffi-obj? flvector->cpointer free free-immobile-cell lookup-errno
          make-array-type make-cstruct-type make-ctype make-late-weak-box make-late-weak-hasheq
          make-sized-byte-string make-union-type malloc malloc-immobile-cell
          memcpy memmove memset offset-ptr? prop:cpointer ptr-add ptr-add! ptr-equal? ptr-offset ptr-ref
          ptr-set! saved-errno set-cpointer-tag! set-ptr-offset! vector->cpointer
          unsafe-register-process-global unsafe-add-global-finalizer
          (rename [ffi-lib* ffi-lib])
          set-ffi-get-lib-and-obj!        ; not exported to Racket
          poll-async-callbacks            ; not exported to Racket
          set-make-async-callback-poll-wakeup! ; not exported to Racket
          set-foreign-eval!               ; not exported to Racket

          unsafe-unbox
          unsafe-unbox*
          unsafe-set-box!
          unsafe-set-box*!
          unsafe-box*-cas!

          unsafe-mcar
          unsafe-mcdr
          unsafe-set-mcar!
          unsafe-set-mcdr!

          unsafe-vector-ref
          unsafe-vector-set!
          unsafe-vector*-ref
          unsafe-vector*-set!
          unsafe-vector*-cas!
          unsafe-vector-length
          unsafe-vector*-length

          unsafe-fxvector-length
          unsafe-fxvector-ref
          unsafe-fxvector-set!

          unsafe-bytes-length
          unsafe-bytes-ref
          unsafe-bytes-set!

          unsafe-undefined
          check-not-unsafe-undefined
          check-not-unsafe-undefined/assign

          unsafe-string-length
          unsafe-string-ref
          unsafe-string-set!

          unsafe-struct-ref
          unsafe-struct-set!
          unsafe-struct*-ref
          unsafe-struct*-set!
          unsafe-struct*-cas!
          unsafe-struct? ; not exported to racket

          unsafe-s16vector-ref
          unsafe-s16vector-set!
          unsafe-u16vector-ref
          unsafe-u16vector-set!
          unsafe-f64vector-ref
          unsafe-f64vector-set!
          unsafe-f80vector-set!
          unsafe-f80vector-ref

          ;; --- not exported to Racket: ---
          make-pthread-parameter
          fork-pthread
          pthread?
          get-thread-id
          get-initial-pthread
          make-condition
          condition-wait
          condition-signal
          condition-broadcast
          make-mutex
          mutex-acquire
          mutex-release
          threaded?
          set-future-callbacks!)
  (import (rename (chezpart)
                  [define define/no-lift])
	  (rename (only (chezscheme) sleep)
		  [sleep chez:sleep])
	  (only (chezscheme)
                thread?
                threaded?
                get-thread-id
                format
                fprintf
                current-error-port
                error
                map for-each andmap ormap)
          (only (chezscheme csv7)
                record-field-accessor
                record-field-mutator))

  (define/no-lift none (chez:gensym "none"))
  (define/no-lift none2 (chez:gensym "none2")) ; never put this in an emphemeron

  (include "rumble/define.ss")
  (include "rumble/virtual-register.ss")
  (include "rumble/layout.ss")
  (include "rumble/begin0.ss")
  (include "rumble/letrec.ss")
  (include "rumble/syntax-rule.ss")
  (include "rumble/value.ss")
  (include "rumble/lock.ss")
  (include "rumble/thread-local.ss")
  (include "rumble/version.ss")
  (include "rumble/check.ss")
  (include "rumble/constant.ss")
  (include "rumble/hash-code.ss")
  (include "rumble/symbol.ss")
  (include "rumble/struct.ss")
  (include "rumble/prefab.ss")
  (include "rumble/impersonator.ss")
  (include "rumble/equal.ss")
  (include "rumble/number.ss")
  (include "rumble/procedure.ss")
  (include "rumble/object-name.ss")
  (include "rumble/arity.ss")
  (include "rumble/intmap.ss")
  (include "rumble/hash.ss")
  (include "rumble/datum.ss")
  (include "rumble/thread-cell.ss")
  (include "rumble/pthread.ss")
  (include "rumble/control.ss")
  (include "rumble/interrupt.ss")
  (include "rumble/parameter.ss")
  (include "rumble/engine.ss")
  (include "rumble/source.ss")
  (include "rumble/error.ss")
  (include "rumble/error-rewrite.ss")
  (include "rumble/srcloc.ss")
  (include "rumble/boolean.ss")
  (include "rumble/bytes.ss")
  (include "rumble/string.ss")
  (include "rumble/char.ss")
  (include "rumble/list.ss")
  (include "rumble/vector.ss")
  (include "rumble/box.ss")
  (include "rumble/immutable.ss")
  (include "rumble/keyword.ss")
  (include "rumble/mpair.ss")
  (include "rumble/flvector.ss")
  (include "rumble/correlated.ss")
  (include "rumble/graph.ss")
  (include "rumble/time.ss")
  (include "rumble/random.ss")
  (include "rumble/memory.ss")
  (include "rumble/ephemeron.ss")
  (include "rumble/will-executor.ss")
  (include "rumble/system.ss")
  (include "rumble/unsafe.ss")
  (include "rumble/extfl.ss")
  (include "rumble/place.ss")
  (include "rumble/errno-data.ss")
  (include "rumble/foreign.ss")
  (include "rumble/future.ss")
  (include "rumble/inline.ss")

  (define-virtual-registers-init init-virtual-registers)
  (init-virtual-registers)

  (set-no-locate-source!)
  ;; Note: if there's a bug in `rumble` that causes exception handling to error,
  ;; the the following line will cause the error to loop with another error, etc.,
  ;; probably without printing anything:
  (set-base-exception-handler!)
  (init-place-locals!)
  (register-as-place-main!)
  (set-collect-handler!)
  (set-primitive-applicables!)
  (set-continuation-applicables!)
  (set-impersonator-applicables!)
  (set-mpair-hash!)
  (set-hash-hash!)
  (set-flvector-hash!)
  (set-extflonum-print!)
  (set-impersonator-hash!)
  (set-procedure-impersonator-hash!)
  (set-vector-impersonator-hash!)
  (set-box-impersonator-hash!)
  (set-cpointer-hash!))
