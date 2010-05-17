
#ifdef WX_CARBON
# ifdef OS_X
#  include <Carbon/Carbon.h>
# else
#  include <Carbon.h>
# endif
#else
# include <Files.h>
# include <EPPC.h>
# include <AppleEvents.h>
# include <Events.h>
#endif

#ifndef FOR_STARTER
# include "scheme.h"
# include <ctype.h>
# include <string.h>
#else
# define scheme_malloc_atomic NewPtr
# define scheme_malloc NewPtr
# define memcpy(d, s, l) BlockMove(s, d, l)
# define malloc NewPtr

extern int strlen(char *);
extern int isspace(int);
static void strcpy(char *s, char *d)
{
  while (*d) *(s++) = *(d++);
  *s = 0;
}
#endif

#include "simpledrop.h"

#ifndef MZ_PRECISE_GC
# define GC_CAN_IGNORE /* empty */
#endif

int scheme_mac_ready, scheme_mac_argc = 0;
char **scheme_mac_argv;

#ifdef OS_X
void GetStarterInfo();
extern char *wxFSRefToPath(FSRef fsref);
#endif

static char *ThisAppName(void)
{	
#ifndef FOR_STARTER
# ifdef OS_X
  return "not used";
# else
  char *result, *dir;
  OSErr err;
  ProcessInfoRec info;
  ProcessSerialNumber curPSN;
  Str255 buffer;
  int dlen;
  
  err = GetCurrentProcess(&curPSN);
  
  info.processInfoLength = sizeof(ProcessInfoRec);
  info.processName = buffer;
  info.processAppSpec = NULL;
  
  err = GetProcessInformation(&curPSN, &info);
  
  dir = scheme_os_getcwd(NULL, 0, NULL, 1);
  dlen = strlen(dir);
  
  result = (char *)malloc(buffer[0] + dlen + 2);
  memcpy(result, dir, dlen);
  result[dlen] = ':';
  memcpy(result + dlen + 1, buffer + 1, buffer[0]);
  result[buffer[0] + dlen + 1] = 0;
  
  return result;
# endif
#else
  return "starter";
#endif
}

static void parse_commandline(char *s, char *src, int addon)
{
  GC_CAN_IGNORE char *token, *pos, ender;
  int count = 0, i;
  char *command[32];

  token = s;
  while (*token && (count < 32)) {
    while (isspace(*token)) {
      token++;
    }
    if (!*token) break;

    pos = token;
    command[count] = pos;
    while (*token && !isspace(*token)) {
      if (*token == '"') {
	ender = '"';
	token++;
      } else if (*token == '\'') {
	ender = '\'';
	token++;
      } else
	ender = 0;
      
      if (ender) {
	while (*token && (*token != ender)) {
	  *(pos++) = *(token++);
	}
	if (*token)
	  token++;
      } else
	*(pos++) = *(token++);
    }
    if (*token)
      token++;
    *pos = 0;

#ifndef FOR_STARTER
    if (src && !strcmp(command[count], "%%")) {
      /* Replace %% with file name */
      command[count] = src;
    } else if (src && (command[count][0] == '%') && (command[count][1]) == ':') {
      /* Replace % with file directory */
      char *ss, *r;
      int i;
      i = strlen(command[count]) + strlen(src);
      r = (char *)malloc(i + 1);
      ss = scheme_strdup(src);
      i = strlen(ss) - 1;
      while (i && ss[i] != ':') {
	i--;
      }
      ss[i + 1] = 0;
      strcpy(r, ss);
      strcat(r, command[count] + 2);
      command[count] = r;
    }
#endif

    count++;
  }
  
  scheme_mac_argc = 1 + count + (addon ? 1 : 0);
  scheme_mac_argv = (char **)malloc(scheme_mac_argc * sizeof(char *));
  scheme_mac_argv[0] = ThisAppName();
  for (i = 0; i < count; i++) {
    scheme_mac_argv[i + 1] = (char *)malloc(strlen(command[i]) + 1);
    strcpy(scheme_mac_argv[i + 1], command[i]);
  }
  if (addon)
    scheme_mac_argv[count + 1] = src;
}

extern void ParseLine(char *s, int *argc, char ***argv)
{
  parse_commandline(s, NULL, 0);
  *argc = scheme_mac_argc;
  *argv = scheme_mac_argv;
}

static void Startup(char **argv, int argc)
{
  int i;
  
  scheme_mac_ready = 1;
  
  if (!argc) {
    scheme_mac_argc = 1;
    scheme_mac_argv = (char **)malloc(sizeof(char *));
    scheme_mac_argv[0] = ThisAppName();
    return;
  }

  scheme_mac_argv = NULL;

#if !defined(FOR_STARTER) && !defined(OS_X)
  if (argc == 1) {
    /* See if this file has startup flags */
    char buf[2048];
    FILE *f = fopen(argv[0], "r");
    buf[0] = 0;
    if (f) {
      fgets(buf, 2048, f);
      fclose(f);
    }
    if (buf[0] == '#' && buf[1]  == '!') {
      GC_CAN_IGNORE char *s;
      int l;
      
      s = buf + 2;
      while (*s && !isspace(*s))
	s++;
      while (*s && isspace(*s))
	s++;
      
      l = strlen(s);
      while (l && isspace(s[l - 1])) {
	--l;
	s[l] = 0;
      }
      
      if (*s) {
	/* Yes, it does. */
	parse_commandline(s, argv[0], buf[2] == '!');
      }
    }  
  } 
  
  if (!scheme_mac_argv) {
    scheme_mac_argc = (argc ? argc + 2 : 1);
    scheme_mac_argv = (char **)malloc(scheme_mac_argc * sizeof(char *));
    for (i = 0; i < argc; i++)
      scheme_mac_argv[i + 2] = argv[i];
    if (argc)
      scheme_mac_argv[1] = "-F";
    
    scheme_mac_argv[0] = ThisAppName();
  }
#else
  scheme_mac_argc = argc + 1;
  scheme_mac_argv = (char **)malloc(scheme_mac_argc * sizeof(char *));
  for (i = 0; i < argc; i++) {
    scheme_mac_argv[i + 1] = argv[i];
  }
  scheme_mac_argv[0] = ThisAppName();
#endif
}

static int gone = 0;

#ifdef OS_X
extern char *scheme_mac_spec_to_path(FSSpec *spec);
#endif

static pascal short DoNothing(const AppleEvent *a, AppleEvent *b, long c)
{
  return 0;
}

static pascal OSErr OpenApplicationStuff(const AppleEvent *a, AppleEvent *b, long c)
{
  if (!gone) {
    gone = 1;
    Startup(NULL, 0);
  }
  
  return 0;
}

static pascal OSErr OpenFinderDoc(const AppleEvent *evt, AppleEvent *b, long c)
{
  AEDescList	docList;
  long		count, size;
  short		i, j;
  DescType	retType;
  AEKeyword	keywd;
  FSSpec	fss;
  FSRef	        fsref;
  char          **files, *fl;
  OSErr         err;

  AEGetParamDesc(evt, keyDirectObject, typeAEList, &docList);
  AECountItems(&docList, &count);
  if (gone)
      files = (char **)scheme_malloc(sizeof(char *) * count);
  else
      files = (char **)malloc(sizeof(char *) * count);
  j = 0;
  for (i = 0; i < count; i++) {
#ifdef OS_X
    err = AEGetNthPtr(&docList, i + 1, typeFSRef, &keywd, &retType, (Ptr)&fsref, sizeof(fsref), &size);
#else
    err = errAECoercionFail;
#endif
    if (err != noErr) {
      if (err == errAECoercionFail) {
	/* Try FSSpec: */
	FSSpec spec;
	err = AEGetNthPtr(&docList, i + 1, typeFSS, &keywd, &retType, (Ptr)&fss, sizeof(FSSpec), &size);
	if (err == noErr) {
	  fl = scheme_mac_spec_to_path(&spec);
	} else {
	  fl = NULL;
	}
      } else {
	fl = NULL;
      }
    } else {
#ifdef OS_X
      fl = wxFSRefToPath(fsref);
#else
      fl = NULL;
#endif
    }

    if (fl) {
      if (gone)
	files[i + j] = fl;
      else {
	/* have to malloc everything */
	char *fl2;
	fl2 = (char *)malloc(strlen(fl)+1);
	memcpy(fl2, fl, strlen(fl)+1);
	files[i + j] = fl2;
      }
      if (!files[i + j])
	--j;
    } else
      --j;
  }
  AEDisposeDesc(&docList);
  
  if (!gone) {
    gone = 1;
    Startup(files, count + j);
  } else {
    wxDrop_Runtime(files, count + j);
  }
  
  return 0;
}

static pascal OSErr CmdLineMessage(const AppleEvent *evt, AppleEvent *b, long c)
{
  AEDescList	cmdList;
  DescType	retType;
  AEKeyword	keywd;
  char          *cmdLine;
  long          size;

  AEGetParamDesc(evt, keyDirectObject, typeAEList, &cmdList);
  size = 1023;
  cmdLine = (char *)malloc(size + 1);
  AEGetNthPtr(&cmdList, 1, typeChar, &keywd, &retType, (Ptr)cmdLine, size, &size);

  cmdLine[size] = 0;
  scheme_mac_ready = 1;
  parse_commandline(cmdLine, NULL, 0);
  
  return 0;
}

static pascal OSErr SetUpQuitMessage(const AppleEvent *a, AppleEvent *b, long c)
{
  wxDrop_Quit();
  return 0;
}

static void Install(void)
{
  short err=0;
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, NewAEEventHandlerUPP(OpenApplicationStuff), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, NewAEEventHandlerUPP(OpenFinderDoc), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments, NewAEEventHandlerUPP(DoNothing), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, NewAEEventHandlerUPP(SetUpQuitMessage), 0, 0);
  err = AEInstallEventHandler('PLT ', 'cmdl', NewAEEventHandlerUPP(CmdLineMessage), 0, 0);
}

void wxDrop_GetArgs(int *argc, char ***argv, int *in_terminal)
{
  *in_terminal = 1;

  MZ_REGISTER_STATIC(scheme_mac_argv);

  Install();
  while (!scheme_mac_ready) {
    EventRecord event;
    
    WaitNextEvent(highLevelEventMask, &event, 0x7FFFFFFF, 0L);
    if (event.what == kHighLevelEvent) {
      AEProcessAppleEvent(&event);
    }
  }

#ifdef OS_X
  {
    int from_finder;

    from_finder = (((*argc) > 1) && (strncmp((*argv)[1],"-psn_",5) == 0));
    if (from_finder) {
      /* Finder started app, or someone wants us to think so; set
	 *in_terminal to 0 and combine AE-based command-line with given
	 command line */
      int i, new_argc;
      char **new_argv;
      *in_terminal = 0;
      new_argc = (scheme_mac_argc - 1) + ((*argc) - 2) + 1;
      new_argv = (char **)malloc(new_argc * sizeof(char *));
      new_argv[0] = (*argv)[0];
      for (i = 2; i < (*argc); i++) {
	new_argv[i - 1] = (*argv)[i];
      }
      for (; i < new_argc + 1; i++) {
	new_argv[i - 1] = scheme_mac_argv[i - (*argc) + 1];
      }
      scheme_mac_argc = new_argc;
      scheme_mac_argv = new_argv;
    } else {
      /* command-line start; no AE arguments */
      scheme_mac_argc = *argc;
      scheme_mac_argv = *argv;
    }

    GetStarterInfo();

    /* Open the GRacket framework resources: */
    {
      CFBundleRef fwBundle;

      fwBundle = CFBundleGetBundleWithIdentifier(CFSTR("org.racket-lang.GRacket"));
      if (fwBundle) {
	SInt16 refNum;
	SInt16 lRefNum;
	CFBundleOpenBundleResourceFiles(fwBundle, &refNum, &lRefNum);
      }
    }

  }
#endif  

  *argc = scheme_mac_argc;
  *argv = scheme_mac_argv;
}

/**********************************************************************/

#ifdef OS_X

#define BUFSIZE 1000
#define RSRCNAME "starter-info"
#define EXECNAME "GRacket"

static CFPropertyListRef getPropertyList()
{
  CFDataRef       xmlData;
  CFStringRef error;
  CFPropertyListRef propertyList;
  CFBundleRef appBundle;
  CFURLRef myRef;

  // locate the starter's bundle:
  appBundle = CFBundleGetMainBundle();
  
  // get a URL for the named resource
  myRef = CFBundleCopyResourceURL(appBundle, CFSTR(RSRCNAME), 
				  NULL, NULL);
  if (myRef == NULL) {
    return NULL;
  }
  
  // Load the XML data using its URL.
  CFURLCreateDataAndPropertiesFromResource(kCFAllocatorDefault, myRef, 
					   &xmlData, NULL, NULL, NULL);

  // convert to a Property List
  propertyList = CFPropertyListCreateFromXMLData(kCFAllocatorDefault, xmlData, 
						 kCFPropertyListImmutable, 
						 &error);
  if (error != NULL) {
    return NULL;
  }

  return propertyList;
}

char *ConvertCFStringRef(CFStringRef str)
{
  static char buf[BUFSIZE];
  char *result;
  Boolean success;

  success = CFStringGetCString(str,buf,BUFSIZE,kCFStringEncodingUTF8);
  if (!success) {
    return "???";
  }
  result = (char *)malloc(strlen(buf) + 1);
  strcpy(result,buf);
  return result;
}  

char *ExeRelativeToAbsolute(char *p)
{
  CFBundleRef appBundle;
  CFURLRef url;
  CFStringRef str;
  char *s, *a;
  long len, len2;

  appBundle = CFBundleGetMainBundle();
  url = CFBundleCopyExecutableURL(appBundle);
  str = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle);
  
  s = ConvertCFStringRef(str);

  len = strlen(s);
  while (len && (s[len - 1] != '/')) {
    --len;
  }

  len2 = strlen(p);

  a = (char *)malloc(len + len2 + 1);
  memcpy(a, s, len);
  memcpy(a + len, p, len2 + 1);

  return a;
}

void GetStarterInfo()
{
  int i;
  CFPropertyListRef propertyList;

  propertyList = getPropertyList();

  if (propertyList) {    
    CFStringRef execName;
    CFArrayRef storedArgsArray;
    CFIndex count;
    char **storedArgs, *tmps, *orig_argv0 = NULL;
    int name_offset;
    
    if (CFDictionaryContainsKey((CFDictionaryRef)propertyList,
				(const void *)(CFSTR("executable name")))) {
      execName = (CFStringRef)CFDictionaryGetValue((CFDictionaryRef)propertyList,
						   (CFSTR("executable name")));
      tmps = ConvertCFStringRef(execName);
      if (tmps[0] != '/') {
	/* Path is relative to this executable */
	tmps = ExeRelativeToAbsolute(tmps);
      }
      orig_argv0 = scheme_mac_argv[0];
      scheme_mac_argv[0] = tmps;
    }

    if (CFDictionaryContainsKey((CFDictionaryRef)propertyList,
				(const void *)CFSTR("stored arguments"))) {
      storedArgsArray = (CFArrayRef)CFDictionaryGetValue((CFDictionaryRef)propertyList,
							 (CFSTR("stored arguments")));
    } else {
      return;
    }
    
    count = CFArrayGetCount(storedArgsArray);
    
    name_offset = (orig_argv0 ? 2 : 0);

    storedArgs = (char **)malloc(sizeof(char *) * (scheme_mac_argc + count + name_offset));
    
    storedArgs[0] = scheme_mac_argv[0];
    if (orig_argv0) {
      /* Preserve the "run" name for a launcher: */
      storedArgs[1] = "-N";
      storedArgs[2] = orig_argv0;
    }

    for (i = 0; i < count; i++) {
      CFStringRef arg;
      char *tmps;
      arg = (CFStringRef)CFArrayGetValueAtIndex(storedArgsArray,i);
      tmps = ConvertCFStringRef(arg);
      storedArgs[i + 1 + name_offset] = tmps;
    }
    for (i = 1; i < scheme_mac_argc; i++) {
      storedArgs[count + i + name_offset] = scheme_mac_argv[i];
    }

    scheme_mac_argv = storedArgs;
    scheme_mac_argc += count + name_offset;
  }
}

#endif
