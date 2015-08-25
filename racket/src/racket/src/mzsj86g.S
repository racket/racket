#ifdef _WIN64

.globl _scheme_mz_setjmp
_scheme_mz_setjmp:

  mov %RBX, (%RCX)
  mov %RBP, 0x08(%RCX)
  mov %RDI, 0x10(%RCX)
  mov %RSI, 0x18(%RCX)
  mov %RSP, 0x20(%RCX)
  mov %R12, 0x28(%RCX)
  mov %R13, 0x30(%RCX)
  mov %R14, 0x38(%RCX)
  mov %R15, 0x40(%RCX)
  stmxcsr 0x48(%RCX)
  movdqu %XMM6, 0x50(%RCX)
  movdqu %XMM7, 0x60(%RCX)
  movdqu %XMM8, 0x70(%RCX)
  movdqu %XMM9, 0x80(%RCX)
  movdqu %XMM10, 0x90(%RCX)
  movdqu %XMM11, 0x0A0(%RCX)
  movdqu %XMM12, 0x0B0(%RCX)
  movdqu %XMM13, 0x0C0(%RCX)
  movdqu %XMM14, 0x0D0(%RCX)
  movdqu %XMM15, 0x0E0(%RCX)
  mov (%RSP), %RAX
  mov %RAX, 0x0F0(%RCX)
  mov $0, %RAX
  ret

.globl _scheme_mz_longjmp
_scheme_mz_longjmp:
  mov (%RCX), %RBX
  mov 0x08(%RCX), %RBP
  mov 0x10(%RCX), %RDI
  mov 0x18(%RCX), %RSI
  mov 0x20(%RCX), %RSP
  mov 0x28(%RCX), %R12
  mov 0x30(%RCX), %R13
  mov 0x38(%RCX), %R14
  mov 0x40(%RCX), %R15
  ldmxcsr 0x48(%RCX)
  movdqu 0x50(%RCX), %XMM6
  movdqu 0x60(%RCX), %XMM7
  movdqu 0x70(%RCX), %XMM8
  movdqu 0x80(%RCX), %XMM9
  movdqu 0x90(%RCX), %XMM10
  movdqu 0x0A0(%RCX), %XMM11
  movdqu 0x0B0(%RCX), %XMM12
  movdqu 0x0C0(%RCX), %XMM13
  movdqu 0x0D0(%RCX), %XMM14
  movdqu 0x0E0(%RCX), %XMM15
  mov 0x0F0(%RCX), %RAX
  mov %RAX, (%RSP)
  mov %RDX, %RAX
  ret

#else

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
	
#endif
        
.section .drectve,"r"
.ascii " -export:scheme_mz_setjmp"
.section .drectve,"r"
.ascii " -export:scheme_mz_longjmp"
