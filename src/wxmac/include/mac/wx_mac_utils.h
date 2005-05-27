///////////////////////////////////////////////////////////////////////////////
// File:	wx_mac_utils.h
// Purpose:	Various utilities (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_mac_utilsh
#define wx_mac_utilsh

char *wxP2C(const unsigned char *p);
char *wxP2C(const unsigned char *p, char *c);

unsigned char *wxC2P(const char *c);
unsigned char *wxC2P(const char *c, unsigned char *p);

char* macCopyString(char* s);
char* macCopyString0(char* s);
char* macCopyString1(char* s);

void macGetHatchPattern(int hatchStyle, Pattern *pattern);

#endif // wx_mac_utils
