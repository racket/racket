/* Call to start: */
extern void wxDrop_GetArgs(int *, char ***, int *in_terminal);

/* You supply: */
void wxDrop_Runtime(char **, int);
void wxDrop_Quit(void);

/* Utility: */
extern void ParseLine(char *, int *, char ***);

extern int scheme_mac_ready;
extern int scheme_mac_argc;
extern char **scheme_mac_argv;
