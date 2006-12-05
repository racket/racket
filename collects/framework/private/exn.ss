(module exn (lib "a-unit.ss")
  (require (lib "class.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (import)
  (export (rename framework:exn^
                  [struct:-exn struct:exn]
                  [make--exn make-exn]
                  [-exn? exn?]))

  (define-struct (-exn exn) ())
  (define-struct (unknown-preference exn) ()))
