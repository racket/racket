#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>

struct rktio_envvars_t {
  intptr_t count, size;
  char **names;
  char **vals;
};

#ifdef OS_X
# define SETENV_DROPS_LEADING_EQUAL_SIGN
#endif

#if defined(OS_X) && !TARGET_OS_IPHONE
# include <crt_externs.h>
# define GET_ENVIRON_ARRAY *_NSGetEnviron()
#endif

#if defined(RKTIO_SYSTEM_UNIX) && !defined(GET_ENVIRON_ARRAY)
extern char **environ;
# define GET_ENVIRON_ARRAY environ
#endif

#ifdef RKTIO_SYSTEM_UNIX
char **rktio_get_environ_array(void)
{
  return GET_ENVIRON_ARRAY;
}
#endif

int rktio_is_ok_envvar_name(rktio_t *rktio, const char *s)
{
  intptr_t i = strlen(s);
#ifdef RKTIO_SYSTEM_WINDOWS
  if (!s[0]) return 0;
#endif  
  while (i--) {
    if (s[i] == '=')
      return 0;
  }
  return 1;
}

int rktio_are_envvar_names_case_insensitive(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  return 1;
#else
  return 0;
#endif
}

char *rktio_getenv(rktio_t *rktio, const char *name)
{
#ifdef RKTIO_SYSTEM_UNIX
  char *s;
  s = getenv(name);
  if (s)
    return MSC_IZE(strdup)(s);
  else {
    set_racket_error(RKTIO_ERROR_NO_SUCH_ENVVAR);
    return NULL;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  intptr_t value_size;
  wchar_t *wp;
  wp = WIDE_PATH_temp(name);
  if (!wp) return NULL;
  value_size = GetEnvironmentVariableW(wp, NULL, 0);
  if (value_size) {
    wchar_t *value_w;
    char *value;
    intptr_t got;
    value_w = malloc(sizeof(wchar_t) * value_size);
    got = GetEnvironmentVariableW(WIDE_PATH_temp(name), value_w, value_size);
    if (got < value_size)
      value_w[got] = 0;
    value = NARROW_PATH_copy(value_w);
    free(value_w);
    return value;
  }
  set_racket_error(RKTIO_ERROR_NO_SUCH_ENVVAR);
  return NULL;
#endif
}

int rktio_setenv(rktio_t *rktio, const char *name, const char *val)
{
#ifdef RKTIO_SYSTEM_UNIX
  if (val) {
    int r;
#ifdef SETENV_DROPS_LEADING_EQUAL_SIGN
    char *tmp = NULL;
    
    if (val[0] == '=') {
      intptr_t len = strlen(val);
      tmp = malloc(len + 2);
      memcpy(tmp + 1, val, len + 1);
      tmp[0] = '=';
      val = tmp;
    }
#endif

    r = setenv(name, val, 1);

#ifdef SETENV_DROPS_LEADING_EQUAL_SIGN
    if (tmp)
      free(tmp);
#endif
    
    if (r)
      get_posix_error();

    return (r ? 0 : 1);
  } else {
    /* on some platforms, unsetenv() returns void */
    unsetenv(name);
    return 1;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  int rc;
  wchar_t *val_w, *name_w;

  name_w = WIDE_PATH_temp(name);
  if (!name_w) return 0;
  
  if (val) {
    val_w = WIDE_PATH_copy(val);
    if (!val_w) return 0;
  } else
    val_w = NULL;

  rc = SetEnvironmentVariableW(name_w, val_w);

  if (val_w)
    free(val_w);
  
  if (rc)
    return 1;
  
  get_windows_error();
  return 0;
#endif
}

rktio_envvars_t *rktio_envvars(rktio_t *rktio)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    char *p;
    wchar_t *e;
    intptr_t i, start, j, count;
    rktio_envvars_t *envvars;

    e = GetEnvironmentStringsW();
    if (!e) {
      get_windows_error();
      return NULL;
    }

    count = 0;
    i = 0;
    while (e[i]) {
      count++;
      while (e[i]) i++;
      i++;
    }
    
    envvars = malloc(sizeof(rktio_envvars_t));
    envvars->size = count;
    envvars->count = count;
    envvars->names = malloc(count * sizeof(char *));
    envvars->vals = malloc(count * sizeof(char *));

    count = 0;
    i = 0;
    while (e[i]) {
      start = i;
      while (e[i]) { i++; }
      p = NARROW_PATH_copy(e + start);
      for (j = 0; p[j] && p[j] != '='; j++) {
      }
      p[j] = 0;
      envvars->names[count] = MSC_IZE(strdup)(p);
      envvars->vals[count] = MSC_IZE(strdup)(p+j+1);
      free(p);
      i++;
      count++;
    }

    FreeEnvironmentStringsW(e);

    return envvars;
  }
#else
  {
    intptr_t i, j;
    char **ea, *p;
    rktio_envvars_t *envvars;

    ea = GET_ENVIRON_ARRAY;

    for (i = 0; ea[i]; i++) {
    }
    
    envvars = malloc(sizeof(rktio_envvars_t));
    envvars->size = i;
    envvars->count = i;
    envvars->names = malloc(i * sizeof(char *));
    envvars->vals = malloc(i * sizeof(char *));

    for (i = 0; ea[i]; i++) {
      p = ea[i];
      for (j = 0; p[j] && p[j] != '='; j++) {
      }
      envvars->names[i] = rktio_strndup(p, j);
      envvars->vals[i] = MSC_IZE(strdup)(p+j+1);
    }

    return envvars;
  }
#endif
}

rktio_envvars_t *rktio_empty_envvars(rktio_t *rktio)
{
  rktio_envvars_t *envvars;
  
  envvars = malloc(sizeof(rktio_envvars_t));
  envvars->size = 2;
  envvars->count = 0;
  envvars->names = malloc(envvars->size * sizeof(char *));
  envvars->vals = malloc(envvars->size * sizeof(char *));

  return envvars;
}

rktio_envvars_t *rktio_envvars_copy(rktio_t *rktio, rktio_envvars_t *envvars)
{
  rktio_envvars_t *new_envvars;
  intptr_t i;
  
  new_envvars = malloc(sizeof(rktio_envvars_t));
  new_envvars->size = envvars->count;
  new_envvars->count = envvars->count;
  new_envvars->names = malloc(envvars->count * sizeof(char *));
  new_envvars->vals = malloc(envvars->count * sizeof(char *));

  for (i = 0; i < envvars->count; i++) {
    new_envvars->names[i] = MSC_IZE(strdup)(envvars->names[i]);
    new_envvars->vals[i] = MSC_IZE(strdup)(envvars->vals[i]);
  }

  return new_envvars;
}

void rktio_envvars_free(rktio_t *rktio, rktio_envvars_t *envvars)
{
  int i;

  for (i = 0; i < envvars->count; i++) {
    free(envvars->names[i]);
    free(envvars->vals[i]);
  }
  
  free(envvars->names);
  free(envvars->vals);
  free(envvars);
}

static void envvars_resize(rktio_envvars_t *envvars, intptr_t new_size)
{
  char **new_names;
  char **new_vals;
  
  new_names = malloc(sizeof(char *) * new_size);
  new_vals = malloc(sizeof(char *) * new_size);
  
  memcpy(new_names, envvars->names, sizeof(char*) * envvars->count);
  memcpy(new_vals, envvars->vals, sizeof(char*) * envvars->count);

  free(envvars->names);
  free(envvars->vals);
  
  envvars->size = new_size;
  envvars->names = new_names;
  envvars->vals = new_vals;
}

intptr_t rktio_envvars_count(rktio_t *rktio, rktio_envvars_t *envvars)
{
  return envvars->count;
}

char *rktio_envvars_name_ref(rktio_t *rktio, rktio_envvars_t *envvars, intptr_t i)
{
  return MSC_IZE(strdup)(envvars->names[i]);
}

char *rktio_envvars_value_ref(rktio_t *rktio, rktio_envvars_t *envvars, intptr_t i)
{
  return MSC_IZE(strdup)(envvars->vals[i]);
}

char *rktio_envvars_get(rktio_t *rktio, rktio_envvars_t *envvars, const char *name)
{
  intptr_t i;

  for (i = 0; i < envvars->count; i++) {
    if (!strcmp(envvars->names[i], name))
      return MSC_IZE(strdup)(envvars->vals[i]);
  }

  return NULL;
}

void rktio_envvars_set(rktio_t *rktio, rktio_envvars_t *envvars, const char *name, const char *value)
{
  intptr_t i, j;

  for (i = 0; i < envvars->count; i++) {
    if (!strcmp(envvars->names[i], name)) {
      if (value) {
        free(envvars->vals[i]);
        envvars->vals[i] = MSC_IZE(strdup)(value);
      } else {
        free(envvars->names[i]);
        free(envvars->vals[i]);
        for (j = i + 1; j < envvars->count; j++) {
          envvars->names[j-1] = envvars->names[j];
          envvars->vals[j-1] = envvars->vals[j];
        }
        if ((envvars->size > 4)
            && (envvars->count <= (envvars->size >> 2))) {
          envvars_resize(envvars, envvars->size >> 1);
        }
        return;
      }
    }
  }

  if (!value)
    return;

  if (envvars->count == envvars->size)
    envvars_resize(envvars, envvars->size * 2);

  envvars->names[envvars->count] = MSC_IZE(strdup)(name);
  envvars->vals[envvars->count] = MSC_IZE(strdup)(value);
  envvars->count++;
}

void *rktio_envvars_to_block(rktio_t *rktio, rktio_envvars_t *envvars)
{
#ifdef RKTIO_SYSTEM_UNIX
  char **r, *s;
  intptr_t i;
  intptr_t len = 0, slen, c;

  for (i = 0; i < envvars->count; i++) {
    len += strlen(envvars->names[i]);
    len += strlen(envvars->vals[i]);
    len += 2;
  }

  r = (char **)malloc((envvars->count+1) * sizeof(char*) + len);
  s = (char *)(r + (envvars->count+1));
  c = 0;
  for (i = 0; i < envvars->count; i++) {
    r[c++] = s;
    slen = strlen(envvars->names[i]);
    memcpy(s, envvars->names[i], slen);
    s[slen] = '=';
    s = s + (slen + 1);
    slen = strlen(envvars->vals[i]);
    memcpy(s, envvars->vals[i], slen);
    s[slen] = 0;
    s = s + (slen + 1);
  }
  r[c] = NULL;

  return r;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  intptr_t i;
  intptr_t len = 0, slen;
  wchar_t *r, *s;

  for (i = 0; i < envvars->count; i++) {
    s = WIDE_PATH_temp(envvars->names[i]);
    if (s) {
      len += wcslen(s);
      s = WIDE_PATH_temp(envvars->vals[i]);
      if (s)
        len += wcslen(s);
      len += 2;
    }
  }

  r = (wchar_t *)malloc((len + 1) * sizeof(wchar_t));

  len = 0;

  for (i = 0; i < envvars->count; i++) {
    s = WIDE_PATH_temp(envvars->names[i]);
    if (s) {
      slen = wcslen(s);
      memcpy(r + len, s, slen * sizeof(wchar_t));
      len += slen;
      r[len++] = '=';
      s = WIDE_PATH_temp(envvars->vals[i]);
      if (!s) s = L"";
      slen = wcslen(s);
      memcpy(r + len, s, slen * sizeof(wchar_t));
      len += slen;
      r[len++] = 0;
    }
  }
  r[len] = 0;

  return r;
#endif
}
