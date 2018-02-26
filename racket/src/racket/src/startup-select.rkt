(module startup-select '#%kernel
  (if (eval-jit-enabled)
      (display "cstartup_bytecode")
      (display "cstartup_c"))
  (newline))

