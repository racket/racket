(module startup-select '#%kernel
  (if (eval-jit-enabled)
      (display "bytecode")
      (display "c"))
  (newline))

