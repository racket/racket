#if defined(OSKIT) && !defined(OSKIT_TEST) && !KNIT

#  include <oskit/clientos.h>
#  include <oskit/dev/osenv.h>
# include <oskit/fs/memfs.h> 
# include <oskit/dev/clock.h> 
# include <oskit/c/sys/time.h> 
# include <oskit/x86/pc/dev.h>
# ifdef USE_OSKIT_CONSOLE
#  include <oskit/x86/pc/direct_cons.h> 
# else
#  include <oskit/dev/freebsd.h> 
# endif

static oskit_osenv_t *osenv;

void start_clock()
{
# define LOCAL_TO_GMT(t) /* (t)->tv_sec += secondswest */
  oskit_timespec_t time;
  /* use fdev's default clock device */
  oskit_clock_t *clock = oskit_clock_init();
  
  oskit_rtc_get(&time);   /* read rtc */
  LOCAL_TO_GMT(&time);            /* adjust for local time */
  oskit_clock_settime(clock, &time); /* set time */
  
  oskit_register(&oskit_clock_iid, (void *) clock);
}

void start_memfs()
{
  oskit_error_t err;
  oskit_filesystem_t *fs;
  oskit_dir_t *root;
  oskit_fsnamespace_t *fsnamespace;

# define CHECK(what, f) \
  if ((err = f)) { printf("in-memory filesystem init error at " what ": %x\n", err); return; }

  CHECK("memfs", oskit_memfs_init(osenv, &fs));
  CHECK("getroot", oskit_filesystem_getroot(fs, &root));
  CHECK("fsnamespace", oskit_create_fsnamespace(root, root, &fsnamespace)); 
  CHECK("setfsnamespace",oskit_clientos_setfsnamespace(fsnamespace));

#undef CHECK
}

oskit_error_t fs_gettime(struct oskit_timespec *tsp)
{
  struct timeval now;
  gettimeofday(&now, NULL);

  tsp->tv_sec = now.tv_sec;
  tsp->tv_nsec = now.tv_usec * 1000;

  return 0;
}

int gethostname(char *s, int len)
{
  strncpy(s, "mzscheme-machine", len);
  return 0;
}

static void reset_console_input(void)
{
  /* So the "press any key to reboot" works: */
  direct_cons_set_flags(0);
}

/*    *********** OSKIT filesystem/ethernet START *****************    */
# ifdef OSK_FILESYSTEMS_AND_ETHERNET

#  include <oskit/c/fs.h>
#  include <oskit/dev/dev.h>
#  include <oskit/fs/filesystem.h>
#  include <oskit/fs/dir.h>
#  include <oskit/diskpart/diskpart.h>
#  include <oskit/fs/linux.h>
#  include <oskit/dev/linux.h>
#  include <oskit/principal.h>
#  include <oskit/net/freebsd.h>

static oskit_principal_t *cur_principal;
static oskit_filesystem_t *main_fs;

static void unmount(void)
{
  if (main_fs) {
    printf(">> Flushing and unmounting filesystem\n");
    fflush(NULL);
    oskit_clientos_setfsnamespace(NULL);
    oskit_filesystem_sync(main_fs, 1);
    oskit_filesystem_release(main_fs);
    main_fs = NULL;
  }
}

int start_linux_fs(char *diskname, char *partname, int net)
{
  oskit_error_t err;
  oskit_identity_t id;
  oskit_blkio_t *disk;
  oskit_blkio_t *part;
  oskit_filesystem_t *fs;
  oskit_dir_t *root;
  oskit_fsnamespace_t *fsnamespace;
# define MAX_PARTS 30
  diskpart_t part_array[MAX_PARTS];
  int num_parts;

# define CHECK(what, f) \
  if ((err = f)) { printf("filesystem init error at " what ": %x\n", err); return 0; }

  printf(">> Initializing devices\n");
  oskit_dev_init(osenv);
  oskit_linux_init_osenv(osenv);
  oskit_linux_init_ide();
  if (net)
    oskit_linux_init_net();
  oskit_linux_init_scsi();
  printf(">> Probing devices\n");
  oskit_dev_probe();
  printf(">> Filesystem initialization\n");
  CHECK("fsinit", fs_linux_init());

  id.uid = 0;
  id.gid = 0;
  id.ngroups = 0;
  id.groups = 0;
  printf(">> Making principal\n");
  CHECK("makeprincipal", oskit_principal_create(&id, &cur_principal));

  printf(">> Opening disk %s\n", diskname);
  CHECK("diskopen", oskit_linux_block_open(diskname, OSKIT_DEV_OPEN_ALL, &disk));

  printf(">> Reading partitions\n");
  num_parts = diskpart_blkio_get_partition(disk, part_array, MAX_PARTS);
  printf(">> Found %d partitions, looking for %s\n", num_parts, partname);
  if (diskpart_blkio_lookup_bsd_string(part_array, partname, disk, &part) == 0) {
    printf("can't find partition %s\n", partname);
    return 0;
  }

  oskit_blkio_release(disk); /* (Partition has a ref.) */

  printf(">> Mounting filesystem\n");
  CHECK("mount", fs_linux_mount(part, 0, &fs));

  printf(">> Getting root\n");
  CHECK("getroot", oskit_filesystem_getroot(fs, &root));
  CHECK("fsnamespace", oskit_create_fsnamespace(root, root, &fsnamespace)); 
  CHECK("setfsnamespace",oskit_clientos_setfsnamespace(fsnamespace));
  
  /* fs has root: */
  oskit_dir_release(root);
  /* clientos has namespace: */
  oskit_fsnamespace_release(fsnamespace);

  main_fs = fs;
  atexit(unmount);

  return 1;
# undef CHECK
}

static int start_freebsd_enet(char *addr, char *mask, char *gate, int fs)
{
  oskit_socket_factory_t *factory;
  struct oskit_freebsd_net_ether_if *eif;
  oskit_error_t err;
# define CHECK(what, f) \
  if ((err = f)) { printf("ethernet init error at " what ": %x\n", err); return 0; }

  if (!fs) {
    /* Otherwise, fs initialization does this: */
    printf(">> Initializing devices\n");
    oskit_dev_init(osenv);
    oskit_linux_init_osenv(osenv);
    oskit_linux_init_net();
    printf(">> Probing devices\n");
    oskit_dev_probe();
  }

  printf(">> Initializing ethernet\n");
  CHECK("socket creator", oskit_freebsd_net_init(osenv, &factory));
  oskit_register(&oskit_socket_factory_iid, (void *)factory);

  printf(">> Finding ethernet device\n");
  CHECK("open", oskit_freebsd_net_open_first_ether_if(&eif));

  printf(">> Configuring ethernet: %s %s %s\n", addr, mask, gate);
  CHECK("ifconfig", oskit_freebsd_net_ifconfig(eif, "eth0", addr, mask));
  CHECK("gateway", oskit_freebsd_net_add_default_route(gate));

  return 1;
# undef CHECK
}

oskit_error_t oskit_get_call_context(const struct oskit_guid *iid, void **out_if)
{
  if (memcmp(iid, &oskit_iunknown_iid, sizeof(*iid)) == 0 ||
      memcmp(iid, &oskit_principal_iid, sizeof(*iid)) == 0) {
    *out_if = cur_principal;
    oskit_principal_addref(cur_principal);
    return 0;
  }
  
  *out_if = 0;
  return OSKIT_E_NOINTERFACE;
}

# endif
/*    *********** OSKIT filesystem/ethernet END *****************    */

#ifndef OSKIT_TEST

static void oskit_prepare(int *_argc, char ***_argv)
{
  int argc = *_argc;
  char **argv = *_argv;

  oskit_clientos_init();
  osenv = oskit_osenv_create_default();
  oskit_register(&oskit_osenv_iid, (void *)osenv);
  start_clock();
  oskit_init_libc();

# ifdef OSK_FILESYSTEMS_AND_ETHERNET
  {
    /* To hardwire a disk or network configuration, set
       `fs' or `net' to 1 and set the initial values of the
       corresponding configuration variables: */
    int fs = 0;
    char *disk = "hda";
    char *partition = "f";
    int net = 0;
    char *addr = "155.99.212.55";
    char *mask = "255.255.255.0";
    char *gate = "155.99.212.126";

    while (1) {
      if (argc > 1) {
	if (!strcmp(argv[1], "--fs")) {
	  if (argc >= 4) {
	    fs = 1;
	    disk = argv[2];
	    partition = argv[3];
	    memmove(argv + 1, argv + 4, (argc - 4) * sizeof(char **));
	    argc -= 3;
	  } else {
	    argc = 1;
	    printf("--fs requires two arguments: <disk> <partition>\n");
	    break;
	  }
	} else if (!strcmp(argv[1], "--net")) {
	  if (argc >= 5) {
	    net = 1;
	    addr = argv[2];
	    mask = argv[3];
	    gate = argv[4];
	    memmove(argv + 1, argv + 5, (argc - 5) * sizeof(char **));
	    argc -= 4;
	  } else {
	    argc = 1;
	    printf("--net requires three arguments: <address> <netmask> <gateway>\n");
	    break;
	  }
	} else
	  break;
      } else
	break;
    }

    if (fs) {
      if (!start_linux_fs(disk, partition, net)) {
	printf("Disk filesystem init failed; using in-memory filesystem.\n");
	start_memfs();
      }
    } else {
      printf("No disk or partition specified; using in-memory filesystem.\n");
      start_memfs();
    }
  
    if (!net || !start_freebsd_enet(addr, mask, gate, fs))
      printf("No ethernet; TCP connections will fail.\n"); 
  }
# else
  {
    start_memfs();
  }
# endif

# ifdef USE_OSKIT_CONSOLE
  /* We talk to console directly */
  direct_cons_set_flags(DC_NONBLOCK | DC_RAW);
  atexit(reset_console_input);
# else
  /* C library handles console; needs liboskit_freebsd_dev.a. */
  /* (Initialization here conflicts with OSK_FILESYSTEMS_AND_ETHERNET). */
  oskit_dev_init();
  oskit_freebsd_init_sc();
  oskit_dev_probe();
  oskit_console_init();
# endif

  *_argc = argc;
  *_argv = argv;
}

#endif

#endif /* OSKIT */
