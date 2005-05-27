
#ifndef WX_CARBON
# include <Files.h>
# include <Quickdraw.h>
# include <Aliases.h>
# include <Resources.h>
# include <Windows.h>
# include <Fonts.h>
# include <Dialogs.h>
# include <TextEdit.h>
QDGlobals qd;
#endif

#define FOR_STARTER
#define MZ_REGISTER_STATIC(x) /* empty */
static char *scheme_mac_spec_to_path(FSSpec *spec);

#include "simpledrop.cpp"

#define FOR_MRED

#ifdef FOR_MRED
# define DEST_CREATOR 'mReD'
# define PROGNAME "MrEd"
#else
# define DEST_CREATOR 'MzSc'
# define PROGNAME "MzScheme"
#endif

int strlen(char *s);
int isspace(int c);

static char *eof_hack = "<Insert offset here>";

static char *protect_arg(char *s)
{
  char *p, *r, *q;
  int l;
  int need_quote = 0;
  int tick_count = 0;

  for (p = s, l = 0; *p; p++, l++) {
    if (*p == '\'')
      tick_count++;
    else if (isspace(*p) || *p == '"')
      need_quote = 1;
  }

  if (!need_quote && !tick_count)
    return s;

  r = NewPtr(l + 4 * tick_count + 3);
  p = r;
  *(p++) = '\'';
  for (q = s; *q; q++) {
    if (*q == '\'') {
      *(p++) = '\'';
      *(p++) = '"';
      *(p++) = '\'';
      *(p++) = '"';
      *(p++) = '\'';
    } else
      *(p++) = *q;
  }
  *(p++) = '\'';
  *p = 0;

  return r;
}

OSErr find_exec_spec(FSSpec *result, FSSpec *myself)
{
  OSErr err;
  CInfoPBRec pbrec;
  short find_position = 0;
  AliasHandle execAlias;
  Boolean aliasChanged;

  // first we try the stored alias.
  
  execAlias = (AliasHandle)GetResource('alis',128);
  
  if (execAlias != NULL) {
    
    err = ResolveAlias(NULL,execAlias,result,&aliasChanged);
  
    if (err == noErr) {
      
      if (aliasChanged) { // need to write it back to disk
        WriteResource((Handle)execAlias);
      }
       	
      return noErr;
    }
  }
    
  // if that doesn't work, drop back and look for the file in the current directory.

  result->vRefNum = myself->vRefNum;
  result->parID = myself->parID;
  
  while (1) {
    pbrec.hFileInfo.ioVRefNum = result->vRefNum;
    pbrec.hFileInfo.ioDirID = result->parID;
    pbrec.hFileInfo.ioFDirIndex = find_position + 1;
    pbrec.hFileInfo.ioNamePtr = result->name;
    if (PBGetCatInfo(&pbrec, 0) || !*result->name)
      break;
    
    find_position++;

    if (pbrec.hFileInfo.ioFlFndrInfo.fdType == 'APPL') {
      if (pbrec.hFileInfo.ioFlFndrInfo.fdCreator == DEST_CREATOR) { 
        // found it!
        Str255 newName = "\pExecutable Location";
        
	if (NewAlias(NULL,result,&execAlias) == noErr) {
	  AddResource((Handle)execAlias,'alis',128,newName);
	  WriteResource((Handle)execAlias);
	} 

	return noErr;
      }
		
    }
  }
  
  /* Didn't find it */
  ParamText("\pCould not find " PROGNAME ". ("
	    PROGNAME " is needed to run this program. Try running setup-plt again)", 
	    NULL, NULL, NULL);
  Alert(128, NULL);

  return 1;
}

int invoke_application(FSSpec *application, FSSpec *myself)
{
  Str255 ourName;
  LaunchParamBlockRec rec;
  int i, j, givenlen;
  char *a, *b, *buffer;
  long buffer_length;
  unsigned char *reason;
  short rsrcRef_ignored;
  Handle appParam_ignored;
  int argc;
  char **argv;
  ProcessSerialNumber psn;
  SInt16 file;
  int in_terminal; /* will be zero */
  ProcessSerialNumber curPSN;
  ProcessInfoRec info;
        
  psn.highLongOfPSN = 0;
  psn.lowLongOfPSN = kNoProcess;

  /* Is it already running? */
  while (!GetNextProcess(&psn)) {
    ProcessInfoRec prec;
    FSSpec got;
    Str32 name;
	  
    prec.processInfoLength = sizeof(prec);
    prec.processName = name;
    prec.processAppSpec = &got;
    GetProcessInformation(&psn, &prec);
    if ((got.vRefNum == application->vRefNum)
	&& (got.parID == application->parID)
	&& (got.name[0] == application->name[0])) {
      int i = application->name[0];
      while (i--) {
	if (got.name[i + 1] != application->name[i + 1])
	  break;	
      }
      if (i < 0) {
	/* It's already running: */
	ParamText("\p" PROGNAME " is already running. \r"
		  PROGNAME " is needed to run this program. "
		  "Please quit " PROGNAME " and try again. ",
		  NULL, NULL, NULL);
		  Alert(128, NULL);
	return 1;
      }
    }
  }

  /* Read our own data fork to get the command line: */
  GetCurrentProcess(&curPSN);
  info.processInfoLength = sizeof(ProcessInfoRec);
  info.processName = ourName;
  info.processAppSpec = NULL;
  GetProcessInformation(&curPSN, &info);

  if (HOpenDF(myself->vRefNum, myself->parID, ourName, fsRdPerm, &file)) {
    buffer = NULL;
    buffer_length = 0;
  } else {
    buffer_length = ((long *)eof_hack)[0];
    buffer = NewPtr(buffer_length + 1);
    SetFPos(file, fsFromStart, ((long *)eof_hack)[1]);
    FSRead(file, &buffer_length, buffer);
    buffer[buffer_length] = 0;
  }
		
  /* Get command-line arguments here */
  wxDrop_GetArgs(&argc, &argv, &in_terminal);

  /* Compute size of args from command-line */
  givenlen = 0;
  for (i = 1; i < argc; i++) {
    argv[i] = protect_arg(argv[i]);
    givenlen += strlen(argv[i]) + 1; /* add space */
  }

  {
    AEDesc myAddress;
    AEDesc docDesc, launchDesc;
    AEDescList theList;
    AliasHandle withThis;
    AppleEvent theEvent;
    int alen;
    OSErr myErr;

    a = (char *)NewPtr(buffer_length + givenlen);
    alen = buffer_length + givenlen;
    b = buffer;
    for (i = 0; i < buffer_length; i++)
      a[i] = b[i];
    for (j = 1; j < argc; j++) {
      a[i++] = ' ';
      while (*(argv[j]))
	a[i++] = *(argv[j]++);
    }

    myErr = AECreateDesc(typeProcessSerialNumber, (Ptr)&curPSN, sizeof(ProcessSerialNumber), &myAddress);

    AECreateAppleEvent('PLT ', 'cmdl', &myAddress, kAutoGenerateReturnID, kAnyTransactionID, &theEvent);

    AECreateDesc(typeChar, (Ptr)a, alen, &docDesc);

    /* create a list for the cmdline. */
    AECreateList(nil, 0, false, &theList);
    /* put it in the list */
    AEPutDesc(&theList, 0, &docDesc);
    AEPutParamDesc(&theEvent, keyDirectObject, &theList);
    /* coerce the event from being an event into being appParms */
    /*      If you just want to send an 'odoc' to an application that is */
    /* already running, you can stop here and do an AESend on theEvent */
    AECoerceDesc(&theEvent, typeAppParameters, &launchDesc);
    HLock((Handle)theEvent.dataHandle);
    /* and stuff it in the parameter block */
    /* This is a little weird, since we're actually moving the event out of the */
    /* AppParameters descriptor.  But it's necessary, the coercison to typeAppParameters */
    /* stuffs the whole appleevent into one AERecord (instead of a AEDesc) so  */
    /* the Finder gets the whole event as one handle.  It can then parse it itself */
    /* to do the sending */
    rec.launchAppParameters = (AppParametersPtr)*(launchDesc.dataHandle);
  }

  rec.launchBlockID = extendedBlock;
  rec.launchEPBLength = extendedBlockLen;
  rec.launchFileFlags = 0;
  rec.launchControlFlags = launchNoFileFlags | launchContinue | launchUseMinimum;
  rec.launchAppSpec = application;

  switch (LaunchApplication(&rec)) {
  case noErr:
    return 0;
  case memFullErr:
    reason = "\pThere is not enough memory available.";
    break;
  default:
    reason = "\pIs there enough memory available?";
    break;
  }
        
  /* Startup error */
  ParamText("\pError starting " PROGNAME ". \r"
	    PROGNAME " is needed to run this program. "
	    PROGNAME " was found but didn't start. ",
	    reason, NULL, NULL);
  Alert(128, NULL);
  return 1;
}


int main(int argc, char **argv)
{
  FSSpec application;
  OSErr err;
  FSSpec myself;
  
#ifndef WX_CARBON
  MaxApplZone();
	
  InitGraf(&qd.thePort);		/* initialize Mac stuff */
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  
  MoreMasters();
  MoreMasters();
#else
  MoreMasterPointers(1);
#endif

  if (HGetVol(myself.name, &myself.vRefNum, &myself.parID)) {
    return 0;
  }

  if (find_exec_spec(&application,&myself) == noErr) {
    return invoke_application(&application,&myself);
  }
}


void wxDrop_Runtime(char **, int)
{
}

void wxDrop_Quit(void)
{
}


/********** Reimplemented/copied here to save space in the binary: ********/

int strlen(char *s)
{
  int i;
  for (i = 0; s[i]; i++);
  return i;
}

int isspace(int c)
{
  return ((c == ' ') 
	  || (c == '\t')
	  || (c == '\n')
	  || (c == '\r'));
}

static char *scheme_mac_spec_to_path(FSSpec *spec)
{
#define QUICK_BUF_SIZE 256

  CInfoPBRec pbrec;
  char buf[256], qbuf[QUICK_BUF_SIZE], *s;
  int i, j, size = 0, alloced = QUICK_BUF_SIZE - 1;
  int vref = spec->vRefNum;
  long dirID = spec->parID;

  s = qbuf;
  for (i = spec->name[0]; i; i--)
    s[size++] = spec->name[i];

  while (1)  {
    pbrec.hFileInfo.ioNamePtr = (unsigned char *)buf;
    pbrec.hFileInfo.ioVRefNum = vref;
    pbrec.hFileInfo.ioDirID = dirID;
    pbrec.hFileInfo.ioFDirIndex = -1;
    if (PBGetCatInfo(&pbrec, 0))
      return NULL;
	
    if (size + buf[0] + 1 > alloced) {
      char *old;
	   
      alloced *= 2;
      old = (char *)NewPtr(alloced + 1);
      BlockMove(s, old, size);
    }

    s[size++] = ':';
    for (i = buf[0]; i; i--)
      s[size++] = buf[i];
	  
    dirID = pbrec.dirInfo.ioDrParID;
    if (dirID == 1)
      break;
  }
  
  if (alloced < QUICK_BUF_SIZE) {
    s = (char *)NewPtr(size + 1);
    for (j = 0, i = size; i--; j++)
      s[j] = qbuf[i];
  } else {
    int save;
  	
    for (i = 0, j = size - 1; i < j; i++, j--) {
      save = s[i];
      s[i] = s[j];
      s[j] = save;
    }
  }
  
  s[size] = 0;
  return s;
}

