/*								-*- C++ -*-
 *
 * Purpose: wxWindows application and main loop
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#ifndef AppMain_h
#define AppMain_h

#ifdef __GNUG__
#pragma interface
#endif

class wxFrame;

class wxApp : public wxObject {
public:
    // functions
    wxApp();

    virtual void    Dispatch(void);
            Bool    Initialized(void) { return initialized; };
    virtual int     MainLoop(void);
    virtual void    ExitMainLoop(void) { keep_going = False; }
    virtual wxFrame *OnInit(void) { return 0; };
    virtual int     OnExit(void) { return 0; };
    virtual Bool    Pending(void);

    char* GetAppName(void)	   { return wxAPP_NAME; }
    void  SetAppName(char *name)   { wxAPP_NAME = name; }
    char* GetClassName(void)	   { return wxAPP_CLASS; }
    void  SetClassName(char *name) { wxAPP_CLASS = name; }
    // data
    int    argc;
    char** argv;
private:
    friend int wxEntry(int argc, char **argv);

    Bool  initialized;
public:
    Bool  keep_going;
};

void wxInitNewToplevel(void);
extern void wxDoNextEvent();
extern int wxEventReady();
extern void wxDoEvents();

extern int wxEntry(int argc, char *argv[]);

extern int wxGetPreference(const char *name, int *res);
extern int wxGetBoolPreference(const char *name, int *res);

#endif // AppMain_h
