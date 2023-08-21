if test "${enable_postlink}" != "" ; then
  POST_LINKER="${enable_postlink}"
else
  POST_LINKER="${default_post_linker}"
fi

if test "${POST_LINKER}" != "" ; then
  echo "post-linker command: ${POST_LINKER}"
fi
