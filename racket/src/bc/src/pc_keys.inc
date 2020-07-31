
/* Standard U.S. keyboard (sorry, world). */
/* Started with OSKit code, but I wanted more keys, including Ctl-C */

#define SHIFT -1
#define CTL -2
#define ALT -3

static int shift_state, ctl_state, alt_state;

static char keymap[128][2] = {
	{0,     0},             /* 0 */
	{27,	27},		/* 1 - ESC */
	{'1',	'!'},		/* 2 */
	{'2',	'@'},
	{'3',	'#'},
	{'4',	'$'},
	{'5',	'%'},
	{'6',	'^'},
	{'7',	'&'},
	{'8',	'*'},
	{'9',	'('},
	{'0',	')'},
	{'-',	'_'},
	{'=',	'+'},
	{8,	8},		/* 14 - Backspace */
	{'\t',	'\t'},		/* 15 */
	{'q',	'Q'},
	{'w',	'W'},
	{'e',	'E'},
	{'r',	'R'},
	{'t',	'T'},
	{'y',	'Y'},
	{'u',	'U'},
	{'i',	'I'},
	{'o',	'O'},
	{'p',	'P'},
	{'[',	'{'},
	{']',	'}'},		/* 27 */
	{'\r',	'\r'},		/* 28 - Enter */
	{CTL,	CTL},		/* 29 - Ctrl */
	{'a',	'A'},		/* 30 */
	{'s',	'S'},
	{'d',	'D'},
	{'f',	'F'},
	{'g',	'G'},
	{'h',	'H'},
	{'j',	'J'},
	{'k',	'K'},
	{'l',	'L'},
	{';',	':'},
	{'\'',	'"'},		/* 40 */
	{'`',	'~'},		/* 41 */
	{SHIFT,	SHIFT},		/* 42 - Left Shift */
	{'\\',	'|'},		/* 43 */
	{'z',	'Z'},		/* 44 */
	{'x',	'X'},
	{'c',	'C'},
	{'v',	'V'},
	{'b',	'B'},
	{'n',	'N'},
	{'m',	'M'},
	{',',	'<'},
	{'.',	'>'},
	{'/',	'?'},		/* 53 */
	{SHIFT,	SHIFT},		/* 54 - Right Shift */
	{0,	0},		/* 55 - Print Screen */
	{ALT,	ALT},		/* 56 - Alt */
	{' ',	' '},		/* 57 - Space bar */
	{0,	0},		/* 58 - Caps Lock */
	{0,	0},		/* 59 - F1 */
	{0,	0},		/* 60 - F2 */
	{0,	0},		/* 61 - F3 */
	{0,	0},		/* 62 - F4 */
	{0,	0},		/* 63 - F5 */
	{0,	0},		/* 64 - F6 */
	{0,	0},		/* 65 - F7 */
	{0,	0},		/* 66 - F8 */
	{0,	0},		/* 67 - F9 */
	{0,	0},		/* 68 - F10 */
	{0,	0},		/* 69 - Num Lock */
	{0,	0},		/* 70 - Scroll Lock */
	{'7',	'7'},		/* 71 - Numeric keypad 7 */
	{'8',	'8'},		/* 72 - Numeric keypad 8 */
	{'9',	'9'},		/* 73 - Numeric keypad 9 */
	{'-',	'-'},		/* 74 - Numeric keypad '-' */
	{'4',	'4'},		/* 75 - Numeric keypad 4 */
	{'5',	'5'},		/* 76 - Numeric keypad 5 */
	{'6',	'6'},		/* 77 - Numeric keypad 6 */
	{'+',	'+'},		/* 78 - Numeric keypad '+' */
	{'1',	'1'},		/* 79 - Numeric keypad 1 */
	{'2',	'2'},		/* 80 - Numeric keypad 2 */
	{'3',	'3'},		/* 81 - Numeric keypad 3 */
	{'0',	'0'},		/* 82 - Numeric keypad 0 */
	{'.',	'.'},		/* 83 - Numeric keypad '.' */
};

int convert_scan_code(int k)
{
  if (k & 0x80) {
    /* Key release. Look for modifier releases. */
    k &= 0x7f;
    k = keymap[k][0];
    if (k == SHIFT)
      shift_state = 0;
    else if (k == CTL)
      ctl_state = 0;
    else if (k == ALT)
      alt_state = 0;
    return -1;
  } else {
    /* Translate the character through the keymap. */
    k = keymap[k][shift_state];
    if (k == SHIFT)
      shift_state = 1;
    else if (k == CTL)
      ctl_state = 1;
    else if (k == ALT)
      alt_state = 1;
    else {
      if (k == '.') {
	/* Check for Ctl-Alt-Del */
	if (ctl_state && alt_state)
	  exit(0);
      }

      if (ctl_state) {
	if (('A' <= k) && (k <= 'Z'))
	  k += ('a' - 'A');
	
	if (('a' <= k) && (k <= 'z'))
	  k -= ('a' - 1);
	else
	  k = -1;
      }
      
      return k;
    }

    return -1;
  }
}

#undef SHIFT
#undef CTL
#undef ALT
