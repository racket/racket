#ifndef _WIN64

.globl _scheme_mz_setjmp
_scheme_mz_setjmp:
  push %EBP	
  mov %ESP, %EBP
  mov 4(%EBP), %ECX  # return address
  mov 8(%EBP), %EAX  # jmp_buf ptr
  mov (%EBP), %EDX   # old EBP
  mov %EDX, (%EAX)
  mov %EBX, 4(%EAX)
  mov %EDI, 8(%EAX)
  mov %ESI, 12(%EAX)
  mov %ESP, 16(%EAX)
  mov %ECX, 20(%EAX)
  pop %EBP
  mov $0, %EAX
  ret

.globl _scheme_mz_longjmp
_scheme_mz_longjmp:
  push %EBP
  mov %ESP, %EBP
  mov 12(%EBP), %EAX  # return value
  mov 8(%EBP), %ECX   # jmp_buf
  mov 16(%ECX), %ESP  # restore stack pointer
  mov (%ECX), %EBP    # old EBP
  mov %EBP, (%ESP)
  mov %ESP, %EBP
  mov 4(%ECX), %EBX
  mov 8(%ECX), %EDI
  mov 12(%ECX), %ESI
  mov 20(%ECX), %ECX  # return address
  mov %ECX, 4(%EBP)
  pop %EBP
  ret

.section .drectve,"r"
.ascii " -export:scheme_mz_setjmp"
.section .drectve,"r"
.ascii " -export:scheme_mz_longjmp"
	
#endif
