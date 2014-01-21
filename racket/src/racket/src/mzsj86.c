/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
  Copyright (c) 1995 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.
*/

/* Re-implementation of i386 setjmp to avoid Windows-specific work,
   which messes up Racket's (GRacket's, really) threads. */

#include "schpriv.h"

#ifndef _WIN64

int __declspec(naked) scheme_mz_setjmp(mz_jmp_buf b)
{
  __asm {
    mov ECX, [ESP]
	mov EAX, [ESP+4]
	mov [EAX], EBP
	mov [EAX+4], EBX
	mov [EAX+8], EDI
	mov [EAX+12], ESI
	mov [EAX+16], ESP
	mov [EAX+20], ECX
	mov EAX, 0
	ret
  }
}

void __declspec(naked) scheme_mz_longjmp(mz_jmp_buf b, int v)
{
  __asm {
    mov EAX, [ESP+8]
	mov ECX, [ESP+4]
	mov ESP, [ECX+16]
	mov EBP, [ECX]
	mov EBX, [ECX+4]
	mov EDI, [ECX+8]
	mov ESI, [ECX+12]
	mov ECX, [ECX+20]
	mov [ESP], ECX
	ret
  }
}

#endif
