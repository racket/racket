#define SIG_siS_s 5
typedef Scheme_Object* (*prim_siS_s)(Scheme_Object*, int, Scheme_Object**);
Scheme_Object* scheme_rtcall_siS_s(prim_siS_s f, Scheme_Object* g115, int g116, Scheme_Object** g117);
#define SIG_iSs_s 6
typedef Scheme_Object* (*prim_iSs_s)(int, Scheme_Object**, Scheme_Object*);
Scheme_Object* scheme_rtcall_iSs_s(prim_iSs_s f, int g118, Scheme_Object** g119, Scheme_Object* g120);
#define SIG_s_s 7
typedef Scheme_Object* (*prim_s_s)(Scheme_Object*);
Scheme_Object* scheme_rtcall_s_s(prim_s_s f, Scheme_Object* g121);
#define SIG_n_s 8
typedef Scheme_Object* (*prim_n_s)(Scheme_Native_Closure_Data*);
Scheme_Object* scheme_rtcall_n_s(prim_n_s f, Scheme_Native_Closure_Data* g122);
#define SIG__s 9
typedef Scheme_Object* (*prim__s)();
Scheme_Object* scheme_rtcall__s(prim__s f );
#define SIG_ss_s 10
typedef Scheme_Object* (*prim_ss_s)(Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_ss_s(prim_ss_s f, Scheme_Object* g123, Scheme_Object* g124);
#define SIG_ss_m 11
typedef MZ_MARK_STACK_TYPE (*prim_ss_m)(Scheme_Object*, Scheme_Object*);
MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(prim_ss_m f, Scheme_Object* g125, Scheme_Object* g126);
#define SIG_Sl_s 12
typedef Scheme_Object* (*prim_Sl_s)(Scheme_Object**, long);
Scheme_Object* scheme_rtcall_Sl_s(prim_Sl_s f, Scheme_Object** g127, long g128);
#define SIG_l_s 13
typedef Scheme_Object* (*prim_l_s)(long);
Scheme_Object* scheme_rtcall_l_s(prim_l_s f, long g129);
#define SIG_bsi_v 14
typedef void (*prim_bsi_v)(Scheme_Bucket*, Scheme_Object*, int);
void scheme_rtcall_bsi_v(prim_bsi_v f, Scheme_Bucket* g130, Scheme_Object* g131, int g132);
#define SIG_iiS_v 15
typedef void (*prim_iiS_v)(int, int, Scheme_Object**);
void scheme_rtcall_iiS_v(prim_iiS_v f, int g133, int g134, Scheme_Object** g135);
#define SIG_ss_v 16
typedef void (*prim_ss_v)(Scheme_Object*, Scheme_Object*);
void scheme_rtcall_ss_v(prim_ss_v f, Scheme_Object* g136, Scheme_Object* g137);
#define SIG_b_v 17
typedef void (*prim_b_v)(Scheme_Bucket*);
void scheme_rtcall_b_v(prim_b_v f, Scheme_Bucket* g138);
#define SIG_sl_s 18
typedef Scheme_Object* (*prim_sl_s)(Scheme_Object*, long);
Scheme_Object* scheme_rtcall_sl_s(prim_sl_s f, Scheme_Object* g139, long g140);
#define SIG_iS_s 19
typedef Scheme_Object* (*prim_iS_s)(int, Scheme_Object**);
Scheme_Object* scheme_rtcall_iS_s(prim_iS_s f, int g141, Scheme_Object** g142);
#define SIG_S_s 20
typedef Scheme_Object* (*prim_S_s)(Scheme_Object**);
Scheme_Object* scheme_rtcall_S_s(prim_S_s f, Scheme_Object** g143);
#define SIG_s_v 21
typedef void (*prim_s_v)(Scheme_Object*);
void scheme_rtcall_s_v(prim_s_v f, Scheme_Object* g144);
#define SIG_iSi_s 22
typedef Scheme_Object* (*prim_iSi_s)(int, Scheme_Object**, int);
Scheme_Object* scheme_rtcall_iSi_s(prim_iSi_s f, int g145, Scheme_Object** g146, int g147);
#define SIG_siS_v 23
typedef void (*prim_siS_v)(Scheme_Object*, int, Scheme_Object**);
void scheme_rtcall_siS_v(prim_siS_v f, Scheme_Object* g148, int g149, Scheme_Object** g150);
