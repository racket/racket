/*
 * File:	wb_cmdlg.cc
 * Purpose:	Common dialogs: generic code
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1995, Julian Smart
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <stdlib.h>

static char *
wxDefaultFileSelector(Bool load, const char *what, char *extension, char *default_name)
{
  char prompt[50];
  char wild[60];

  sprintf(prompt, load ? "Select file" : "Save file", what);

  if (*extension == '.') {
    extension = COPYSTRING_TO_ALIGNED(extension, 1);
  }
  sprintf(wild, "*.%s", extension);

  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild);
}


// Generic file load dialog
char *
wxLoadFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(TRUE, what, extension, default_name);
}


// Generic file save dialog
char *
wxSaveFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(FALSE, what, extension, default_name);
}

/*
 * Message centring code
 *
 */

void wxSplitMessage(char *message, wxList *messageList, wxPanel *panel)
{
  char *copyMessage;
  size_t i = 0;
  size_t len;
  char *currentMessage;
  wxMessage *mess;

  copyMessage = copystring(message);
  i = 0;
  len = strlen(copyMessage);
  currentMessage = copyMessage;

  while (i < len) {
    while ((i < len) && (copyMessage[i] != '\n')) {
      i++;
    }
    if (i < len) copyMessage[i] = 0;
    mess = new wxMessage(panel, currentMessage);
    messageList->Append(mess);
    panel->NewLine();

    currentMessage = COPYSTRING_TO_ALIGNED(copyMessage, i + 1);
  }
}

void wxCentreMessage(wxList *messageList)
{
  wxNode *node;
  wxMessage *mess;

  // Do the message centering
  for(node = messageList->First(); node; node = node->Next()) {
    mess = (wxMessage *)node->Data();
    mess->Centre();
  }
}


/*
 * A general purpose dialog box with an OnClose that returns TRUE.
 *
 */

class wxMessageBoxDialog: public wxDialogBox
{
 public:
  int buttonPressed;

  wxMessageBoxDialog(wxWindow *parent, char *caption, Bool isModal, int x, int y,
		     int w, int h, long type);
  Bool OnClose(void);
};

wxMessageBoxDialog::wxMessageBoxDialog(wxWindow *parent, char *caption, Bool isModal, int x, int y,
				       int w, int h, long type):
  wxDialogBox(parent, caption, isModal, x, y, w, h, type)
{
  buttonPressed = wxCANCEL;
}

Bool wxMessageBoxDialog::OnClose(void)
{
  return TRUE;
}

void wxDialogOkButton(wxButton *but, wxEvent *)
{
  wxMessageBoxDialog *dialog;

  dialog = (wxMessageBoxDialog *)but->GetParent();

  dialog->buttonPressed = wxOK;
  dialog->Show(FALSE);
}

void wxDialogCancelButton(wxButton * but, wxEvent *)
{
  wxDialogBox *dialog;;

  dialog = (wxDialogBox *)but->GetParent();

  dialog->Show(FALSE);
}

void wxDialogYesButton(wxButton *but, wxEvent *)
{
  wxMessageBoxDialog *dialog;
  dialog = (wxMessageBoxDialog *)but->GetParent();
  dialog->buttonPressed = wxYES;
  dialog->Show(FALSE);
}

void wxDialogNoButton(wxButton *but, wxEvent *)
{
  wxMessageBoxDialog *dialog;
  dialog = (wxMessageBoxDialog *)but->GetParent();
  dialog->buttonPressed = wxNO;
  dialog->Show(FALSE);
}

// Pop up a message box: generic version used by X.
int wxbMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
  wxMessageBoxDialog *dialog;
  Bool centre;
  wxList *messageList;
  wxPanel *but_panel;
  wxButton *ok = NULL;
  wxButton *cancel = NULL;
  wxButton *yes = NULL;
  wxButton *no = NULL;

  
  wxBeginBusyCursor();

  dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, 0);

  centre = ((type & wxCENTRE) == wxCENTRE);

  messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
  but_panel = dialog ;

  if (type & wxYES_NO) {
    yes = new wxButton(but_panel, (wxFunction)&wxDialogYesButton, "Yes");
    no = new wxButton(but_panel, (wxFunction)&wxDialogNoButton, "No");
  }

  if (type & wxOK) {
    ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, "Ok");
  }

  if (type & wxCANCEL) {
    cancel = new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, "Cancel");
  }

  if (ok)
  {
    ok->SetDefault();
    ok->SetFocus();
  }
  else if (yes)
  {
    yes->SetDefault();
    yes->SetFocus();
  }

  dialog->Fit();

  // Do the message centering
  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  return dialog->buttonPressed;
}
