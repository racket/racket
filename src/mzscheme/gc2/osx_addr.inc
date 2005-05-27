
/* This code comes from the conservative collector, ../gc/os_dep.c */

/* Decodes the machine instruction which was responsible for the sending of the
   SIGBUS signal. Sadly this is the only way to find the faulting address because
   the signal handler doesn't get it directly from the kernel (although it is
   available on the Mach level, but droppped by the BSD personality before it
   calls our signal handler...)
   This code should be able to deal correctly with all PPCs starting from the
   601 up to and including the G4s (including Velocity Engine). */
#define EXTRACT_OP1(iw)     (((iw) & 0xFC000000) >> 26)
#define EXTRACT_OP2(iw)     (((iw) & 0x000007FE) >> 1)
#define EXTRACT_REGA(iw)    (((iw) & 0x001F0000) >> 16)
#define EXTRACT_REGB(iw)    (((iw) & 0x03E00000) >> 21)
#define EXTRACT_REGC(iw)    (((iw) & 0x0000F800) >> 11)
#define EXTRACT_DISP(iw)    ((short *) &(iw))[1]

static char *get_fault_addr(unsigned int instr, unsigned int * regs)
{
   int            disp = 0, tmp;
   unsigned int   baseA = 0, baseB = 0;
   unsigned int   addr, alignmask = 0xFFFFFFFF;

   switch(EXTRACT_OP1(instr)) {
      case 38:   /* stb */
      case 39:   /* stbu */
      case 54:   /* stfd */
      case 55:   /* stfdu */
      case 52:   /* stfs */
      case 53:   /* stfsu */
      case 44:   /* sth */
      case 45:   /* sthu */
      case 47:   /* stmw */
      case 36:   /* stw */
      case 37:   /* stwu */
            tmp = EXTRACT_REGA(instr);
            if(tmp > 0)
               baseA = regs[tmp];
            disp = EXTRACT_DISP(instr);
            break;
      case 31:
            switch(EXTRACT_OP2(instr)) {
               case 86:    /* dcbf */
               case 54:    /* dcbst */
               case 1014:  /* dcbz */
               case 247:   /* stbux */
               case 215:   /* stbx */
               case 759:   /* stfdux */
               case 727:   /* stfdx */
               case 983:   /* stfiwx */
               case 695:   /* stfsux */
               case 663:   /* stfsx */
               case 918:   /* sthbrx */
               case 439:   /* sthux */
               case 407:   /* sthx */
               case 661:   /* stswx */
               case 662:   /* stwbrx */
               case 150:   /* stwcx. */
               case 183:   /* stwux */
               case 151:   /* stwx */
               case 135:   /* stvebx */
               case 167:   /* stvehx */
               case 199:   /* stvewx */
               case 231:   /* stvx */
               case 487:   /* stvxl */
                     tmp = EXTRACT_REGA(instr);
                     if(tmp > 0)
                        baseA = regs[tmp];
                        baseB = regs[EXTRACT_REGC(instr)];
                        /* determine Altivec alignment mask */
                        switch(EXTRACT_OP2(instr)) {
                           case 167:   /* stvehx */
                                 alignmask = 0xFFFFFFFE;
                                 break;
                           case 199:   /* stvewx */
                                 alignmask = 0xFFFFFFFC;
                                 break;
                           case 231:   /* stvx */
                                 alignmask = 0xFFFFFFF0;
                                 break;
                           case 487:  /* stvxl */
                                 alignmask = 0xFFFFFFF0;
                                 break;
                        }
                        break;
               case 725:   /* stswi */
                     tmp = EXTRACT_REGA(instr);
                     if(tmp > 0)
                        baseA = regs[tmp];
                        break;
               default:   /* ignore instruction */
                     return NULL;
                    break;
            }
            break;
      default:   /* ignore instruction */
            return NULL;
            break;
   }
	
   addr = (baseA + baseB) + disp;
   addr &= alignmask;
   return (char *)addr;
}
