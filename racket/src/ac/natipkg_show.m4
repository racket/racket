show_explicitly_enabled "${enable_natipkg}" "Adding \"natipkg\" as shared-object convention and library subpath"
show_explicitly_enabled "${enable_nonatipkg}" "Adding \"nonatipkg\" as shared-object convention and library subpath"
if test "${enable_sofind}" != "" ; then
   echo "=== Setting shared-object convention and library-subpath suffix to ${enable_sofind}"
fi
