/* Link this file to gcc-compiled Win32 MzScheme extensions. */
/* It must be the last file in the link list. */

/* This is needed to terminate the list of inport stuff */
/* Copied from winsup/dcrt0.cc in the cygwin32 source distribution. */
	asm(".section .idata$3\n" ".long 0,0,0,0, 0,0,0,0");
