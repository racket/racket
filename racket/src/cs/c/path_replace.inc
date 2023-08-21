static char *path_replace_filename(const char *s, const char *new_file)
{
  int len1 = strlen(s), len2 = strlen(new_file);
  char *r;

  while ((len1 > 0) && (s[len1-1] != '/') && (s[len1-1] != '\\'))
    len1--;

  r = malloc(len1+len2+1);
  memcpy(r, (void *)s, len1);
  memcpy(r+len1, (void *)new_file, len2+1);

  return r;
}
