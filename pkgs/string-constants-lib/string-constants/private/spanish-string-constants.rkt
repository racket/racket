(module spanish-string-constants "string-constant-lang.rkt"

 ;;; when translating this constant, substitue name of actual language for `English'
 (is-this-your-native-language
  "¿Es español tu idioma materno?")

 (interact-with-drscheme-in-language "Interactúa con DrRacket en Español")

 (are-you-sure-you-want-to-switch-languages
  "Esto cambiará el idioma de la interfaz, lo que requiere que reinicies DrRacket. ¿Estas seguro(a)?")
 

 (accept-and-quit "Aceptar y salir")
 (accept-and-exit "Aceptar y salir")

 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
 (ok "OK")
 (cancel "Cancelar")
 (abort "Abortar")
 (untitled "Sin título")
 (untitled-n "Sin título ~a")
 (warning "Advertencia")
 (error "Error")
 (close "Cerrar") ;; as in, close an open window
 (stop "Detener")
 (&stop "&Detener")
 (are-you-sure-delete? "¿Seguro(a) quieres borrar ~a?") ;; ~a is a filename or directory name
 (ignore "Ignorar")
 (revert "Revertir")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "No volver a preguntar (usar la selección actual siempre)")
 ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "No volver a preguntar")
 
 ;; important urls
 (web-materials "Sitios de Web Relacionados")
 (tool-web-sites "Sitios de Web de Herramientas")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")
 
 ;;; bug report form
 (cancel-bug-report? "¿Cancelar el reporte de problemas?")
 (are-you-sure-cancel-bug-report?
  "¿Estas seguro que quieres cancelar el envío de éste reporte de problemas?")
 (bug-report-form "Forma para reportar problemas")
 (bug-report-field-name "Nombre")
 (bug-report-field-email "Correo Electrónico")
 (bug-report-field-summary "Resumen")
 (bug-report-field-severity "Gravedad")
 (bug-report-field-class "Clase")
 (bug-report-field-description "Descripción")
 (bug-report-field-reproduce1 "Pasos para")
 (bug-report-field-reproduce2 "Reproducir")
 (bug-report-field-environment "Ambiente")
 (bug-report-field-docs-installed "Documentos instalados")
 (bug-report-field-collections "Colecciones")
 (bug-report-field-human-language "Idioma")
 (bug-report-field-version "Versión")
 (bug-report-synthesized-information "Información sintetizada")  ;; dialog title
 (bug-report-show-synthesized-info "Muestra información resumida")
 (bug-report-submit "Enviar")
 (bug-report-submit-menu-item "Enviar reporte de problemas...") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "Error al enviar el reporte de problemas")
 (error-sending-bug-report-expln "Un error ocurrió mientras enviaba éste reporte de problemas.  Si tu conexión a Internet está funcionando bien, por favor visita:\n\n    http://bugs.racket-lang.org/\n\ny envía el reporte de problemas por medio de la forma de web en esea página de WEB. Sentimos mucho las molestias que esto te ocasiona.\n\nEl mensaje de error es:\n~a")
 (illegal-bug-report "Reporte de problemas ilegal")
 (pls-fill-in-field "Por favor requisita el campo \"~a\"")
 (malformed-email-address "Dirección de correo electrónico inválida")
 (pls-fill-in-either-description-or-reproduce "Por favor requisita el campo de Descripción o el de Pasos para Reproducir.")

 ;;; check syntax
 (check-syntax "Revisa la sintáxis")
 (cs-italic "Itálica")
 (cs-bold "Negrita")
 (cs-underline "Subrayado")
 (cs-change-color "Cambiar color")
 (cs-tack/untack-arrow "Anclar/Liberar flecha")
 (cs-jump-to-next-bound-occurrence "Saltar a la Siguiente Ocurrencia Ligada")
 (cs-jump-to-binding "Saltar a Ocurrencia Ligada")
 (cs-jump-to-definition "Saltar a la Definición")
 (cs-error-message "Mensaje de error")
 (cs-open-file "Abrir ~a")
 (cs-rename-var "Renombrar ~a")
 (cs-rename-id "Renombrar identificador")
 (cs-rename-var-to "Renombrar ~a a:")
 (cs-name-duplication-error "El nuevo nombre que has seleccionado, ~s, colisiona con otro nombre en este ambiente.")
 (cs-rename-anyway "Renombrar de cualquier modo")
 (cs-status-init "Checar sintaxis: Iniciando el ambiente para el código de usuario")
 (cs-status-coloring-program "Checar sintaxis: coloreando la expresión")
 (cs-status-eval-compile-time "Checar sintaxis: tiempo de evaluación y compilación")
 (cs-status-expanding-expression "Checar sintaxis: expandiendo la expresión")
 (cs-mouse-over-import "Asociación ~s importada desde ~s")
 
 (cs-lexical-variable "variable léxica")
 (cs-imported-variable "variable importada")
 
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Recolectar")
 (read-only "Sólo lectura")
 (auto-extend-selection "Selección Auto-Extendida")
 (overwrite "Sobreescribir")
 (running "ejecutando")
 (not-running "suspendido") 
 
 ;;; misc
 (welcome-to-something "Bienvenido a ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Bienvenido a DrRacket, versión ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "Bienvenido a DrRacket")

 (goto-line "Ir a la línea")
 (goto-line-invalid-number
  "~a no es un número de línea válido. Debe ser un entero entre 1 y ~a")
 (goto-position "Ir a la posición")
 (no-full-name-since-not-saved
  "El archivo no tienen un nombre completo porque no ha sido salvado aún.")
 (cannot-open-because-dne "~a no puede ser abierto, porque no existe.")

 (needs-execute-language-changed
  "ADVERTENCIA: El lenguaje ha cambiado. Haz click en Ejecutar.")
 (needs-execute-teachpack-changed
  "ADVERTENCIA: Los paquetes de enseñanza han cambiado. Haz click en Ejecutar.")
 (needs-execute-defns-edited
  "ADVERTENCIA: La ventana de definiciones ha cambiado. Haz click en Ejecutar.")


 (file-is-not-saved "El archivo \"~a\" no ha sido salvado.")
 (save "Salvar")
 (close-anyway "Cerrar y descartar cambios")
 (clear-anyway "Limpiar")

 ;; menu item title
 (log-definitions-and-interactions "Guardar bitácora de Definiciones e Interacciones...")
 (stop-logging "Terminar bitácora")
 (please-choose-a-log-directory "Por favor seleccione un directorio para almacenar bitácora")
 (logging-to "Guardando bitácora en: ")
 (erase-log-directory-contents "¿Borrar el contenido del directorio de bitácoras: ~a?")
 (error-erasing-log-directory "Error al borrar el contenido del directorio de bitácoras.\n\n~a\n")

  ;; modes
 (mode-submenu-label "Modos")
 (scheme-mode "Modo Scheme")
 (racket-mode "Modo Racket")
 (text-mode "Modo Texto")

 (scheme-mode-color-symbol "Símbolo")
 (scheme-mode-color-keyword "Llave")
 (scheme-mode-color-comment "Comentario")
 (scheme-mode-color-string "Cadena")
 (scheme-mode-color-constant "Constante")
 (scheme-mode-color-parenthesis "Paréntesis")
 (scheme-mode-color-error "Error")
 (scheme-mode-color-other "Otro")
  ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Escoge un color para: ~a")
 (preferences-colors "Colores") ;; used in the preferences dialog

 (url: "URL:")
 (open-url... "Abre URL...")
 (open-url "Abre URL")
 (browse... "Navega...")
 (bad-url "URL Inválido")
 (bad-url:this "URL Inválido: ~a")
 
 ;; Help Desk
 (help "Ayuda")
 (help-desk "Módulo de Ayuda")
 (plt:hd:search "Buscar")
 (plt:hd:feeling-lucky "Me siento afortunado")
 (plt:hd:home "Hogar del Módulo de Ayuda")
 ; next 3 are popup menu choices at bottom of help desk window
 (plt:hd:search-for-keyword "por Palabra clave")
 (plt:hd:search-for-keyword-or-index "por palabra clave o entrada en el índice")
 (plt:hd:search-for-keyword-or-index-or-text "por palabra clave, entrada en el índice o texto")
 (plt:hd:exact-match "patrón exacto") ;; exact match??
 (plt:hd:containing-match "subcadena") ;; containing match ??
 (plt:hd:regexp-match "expresión regular") ;; match regexp
 (plt:hd:find-docs-for "Encuentra documentación sobre:")
 (plt:hd:search-stopped-too-many-matches "(Búsqueda detenida - demasiados patrones casan.)")
 (plt:hd:nothing-found-for "Nada casa con ~a")
 (plt:hd:and "y")
 (plt:hd:refresh "refrescar")
 (plt:hd:refresh-all-manuals "refrescar todos los manuales")
 (plt:hd:manual-installed-date "(~a instalado)")
 ;; Help Desk configuration
 ;; refreshing manuals
 ;; should not mention `SVN' (plt:hd:refresh-done "Refresco de los manuales via SVN terminado")
 (plt:hd:refresh-clearing-indices "Eliminando índices guardados")
 (plt:hd:refresh-deleting... "Borrando la versión vieja de ~a...")
 (plt:hd:refresh-downloading... "Bajando ~a...")
 (plt:hd:refresh-installing... "Instalando nueva versión de ~a...")
 (plt:hd:refresh-clearing-indices "Eliminando indices almacenados")
 (plt:hd:refreshing-manuals "Bajando (nuevamente) los Manuales")
 (plt:hd:refreshing-manuals-finished "Terminado.")
 (plt:hd:about-help-desk "Acerca del Módulo de Ayuda")
 (plt:hd:help-desk-about-string
  "El Módulo de Ayuda es una fuente complete de información acerca del software del grupo PLT, incluyendo DrRacket, MzScheme y MrEd.\n\nVersión ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Ayuda para la ayuda")
 (plt:hd:help-on-help-details "Para ayuda sobre el uso del Módulo de Ayuda, sigue la liga `Cómo usar el Módulo de Ayuda' desde el página principal del Módulo de Ayuda.  (Para llegar a la página principal si no estás ahí ya, presiona el botón marcado `Hogar' en la parte superior de la ventana del Módulo de Ayuda.")
 (plt:hd:ask-about-separate-browser
  "Ha seleccionado una liga que apunta a contenido en el Web.  ¿Le gustaría visitarlo en el navegador del Módulo de Ayuda o le gustaría usar un navegador separado para ver dicho contenido?")
 (plt:hd:homebrew-browser "Navegador del Módulo de Ayuda") ;; choice for the above string (in a button)
 (plt:hd:separate-browser "Navegador separado") ;; other choice for the above string (also in a button)
 (plt:hd:external-link-in-help "URLs externos en Ayuda")
 (plt:hd:use-homebrew-browser "Usar el Navegador del Módulo de Ayuda para URL externos")
 (plt:hd:new-help-desk "Nuevo Módulo de Ayuda")

 ;; in the Help Desk language dialog, title on the right.
 (plt:hd:manual-search-ordering "Orden de búsqueda en manuales")
 
 (reload "Volver a cargar") ;; Reload


 ;; help desk http proxy
 (http-proxy "Proxy de HTTP")
 (proxy-direct-connection "Coneción Directa")
 (proxy-use-proxy "Utilizar proxy:")
 (proxy-host "Host")
 (proxy-port "Port")
 (proxy-bad-host "El Host Proxy está mal especificado")

    ;;browser
 (rewind-in-browser-history "Revertir")
 (forward-in-browser-history "Adelante")
 (home "Hogar")
 (browser "Navegador")
 (external-browser-choice-title "Navegador externo") ; title for radio-button set
 (browser-command-line-label "Línea de comandos:") ; label for radio button that is followed by text boxes
 (choose-browser "Escoge un navegador")
 (no-browser "Preguntar más tarde")
 (browser-cmdline-expl-line-1 "(línea de comando formada concatenando pre-text, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "y post-text, sin espacios extra entre ellos.)") ; ... line 2. (Anyone need more lines?)
 (install? "¿Instalar?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Ha seleccionado un paquete instalable.")
 (do-you-want-to-install-it? "¿Desea intalarlo?")
 (paren-file-size "(El archivo mide ~a bytes)")
 (download-and-install "Descargar && Instalar")
 (download "Descargar")
 (save-downloaded-file "Salvar el archivo descargado como")
 (save-downloaded-file/size "Salvar el archivo descargado (~a bytes) como")
 (downloading "Descargar")
 (downloading-file... "Descargando archivo...")
 (package-was-installed "El paquete fué instalado.")
 (download-was-saved "Se ha salvado el archivo descargado.")

 (install-plt-file-menu-item... "Instalar archivo .plt ...")
 (install-plt-file-dialog-title "Instalar archivo .plt")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Archivo")
 (install-plt-filename "Nombre de Archivo:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "¿Deseas instalar ~a, o quieres abrirlo para edición?")
 (install-plt-file/yes "Instalar")
 (install-plt-file/no "Editar")
 
 (plt-installer-progress-window-title "Progreso del instalador")
 (plt-installer-abort-installation "Abortar instalación") ;; button label
 (plt-installer-aborted "Abortado.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "Acerca de DrRacket")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "¿Salvar este archivo como texto plano?")
 (save-in-drs-format "¿Salvar este archivo en el formato, no texto, particular de DrRacket?")
 (yes "Si")
 (no "No")
 
 ;;; preferences
 (preferences "Preferencias")
 (error-saving-preferences "Error al salvar preferencias: ~a")
 (error-reading-preferences "Error al leer preferencias")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Advertencias")
 (editor-prefs-panel-label "Editando")
 (general-prefs-panel-label "General") 
 (highlight-parens "Resaltar entre parejas de paréntesis")
 (fixup-parens "Corrige paréntesis")
 (flash-paren-match "Señala el paréntesis que casa")
 (auto-save-files "Auto-salva archivos")
 (backup-files "Archivos de respaldo")
 (map-delete-to-backspace "Cambia suprimir por backspace")
 (verify-exit "Confirmar salida")
 (ask-before-changing-format "Preguntar antes de cambiar el formato de salida")
 (wrap-words-in-editor-buffers "Ajustar al border palabras en el editor")
 (show-status-line "Mostrar línea de estado")
 (count-columns-from-one "Cuenta números de columna a partir de uno") 
 (display-line-numbers "Muestra números de línea en el contenedor; sin desplazamiento de caracteres")
 (enable-keybindings-in-menus "Habilita enlaces de teclas en los menús")
 (option-as-meta "Tratar cualquier tecla como meta") ;; macos/macos x only
 (reuse-existing-frames "Reutilizar marcos existentes cuando se abre un nuevo archivo")
 (default-fonts "Fuentes por omisión")
 (paren-match-color "Color de resaltado de paréntesis") ; in prefs dialog
 (online-coloring-active "Colorea sintaxis interactivamente")
 (open-files-in-tabs "Abrir archivos en distintas pestañas (no distintas ventanas)")
 (show-interactions-on-execute "Abrir ventana de interacciones cuando se ejecuta un programa")
 (limit-interactions-size "Limita el tamaño de las interacciones")
 (background-color "Color de fondo")
 (default-text-color "Texto por omisión") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Por favor selecciona un color de fondo")

  ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "abcdefghijklmnñopqrstuvxyz¡!¿?«».")  ;; FIXME: shoulde this have special characters?

 (change-font-button-label "Cambiar")
 (fonts "Fuentes")

					; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Por favor selecciona una nueva fuente \"~a\"")

 (font-size-slider-label "Tamaño")
 (restart-to-see-font-changes "Re-inicia para ver los cambios de las fuentes")

 (font-prefs-panel-title "Fuente")
 (font-name "Nombre de fuente")
 (font-size "Tamaño de fuente")
 (set-font "Ver Fuente...")
 (font-smoothing-label  "Suavisar Fuente")
 (font-smoothing-none "Nada")
 (font-smoothing-some "Algo")
 (font-smoothing-all "Todo")
 (font-smoothing-default "Utiliza el valor por omisión del sistema")
 (select-font-name "Selecciona un nombre de Fuente")
 (example-text "Texto de ejemplo:")
 (only-warn-once "Sólo advierte una vez cuando las ejecución e interacciones no están sincronizadas")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Esperando el archivo candado de las preferencias...")
 (pref-lock-not-gone
  "El archivo candado de las preferencias:\n\n   ~a\n\nevita que éstas sean salvadas. Asegúrate que ningún otro software de Racket esté corriendo y borra este archivo.")
 (still-locked-exit-anyway? "Las preferencias no fueron salvadas exitosamente. ¿Salir de cualquier forma?")

 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Sangrar")  ;; To indent is "Sangrado de márgenes"
 (indenting-prefs-extra-regexp "Expresiones regulares extra")
 
 ; filled with define, lambda, or begin
 (enter-new-keyword "Teclea una nueva palabra clave parecida a ~a:")
 (x-keyword "Palabra clave ~a")
 (x-like-keywords "Palabra clave parecida a ~a")

 (expected-a-symbol "esperaba un símbolo, encontré: ~a")
 (already-used-keyword "la palabra clave \"~a\" ya tenía un sangrado especial asignado")
 (add-keyword "Añade")
 (remove-keyword "Borra")
 
 ;;; find/replace
 (find-and-replace "Buscar y reemplazar")
 (find "Buscar")
 (replace "Reemplazar")
 (dock "Atracar")
 (undock "Des-atracar")
 (replace&find-again "Reemplazar && Vuelve a buscar") ;;; need double & to get a single &
 (forward "Hacia adelante")
 (backward "Hacia atrás")
 (hide "Esconder")

 ;; multi-file-search
 (mfs-multi-file-search-menu-item "Buscar en archivos...")
 (mfs-string-match/graphics "Búsqueda de Cadenas (maneja archivos con gráficas)")
 (mfs-regexp-match/no-graphics "Expresión Regular (sólo archivos de texto plano)")
 (mfs-searching... "Buscando...")
 (mfs-configure-search "Configuración de Búsqueda")
 (mfs-files-section "Archivos")
 (mfs-search-section "Buscar")
 (mfs-dir "Dir")
 (mfs-recur-over-subdirectories "Recurrir sobre subdirectorios")
 (mfs-regexp-filename-filter "Filtro de nombres de archivo con expresiones regulares")
 (mfs-search-string "Buscar cadena")
 (mfs-drscheme-multi-file-search "Búsqueda Multi Archivo  - DrRacket")
 (mfs-not-a-dir "\"~a\" no es un directorio")
 (mfs-open-file "Abrir Archivo")
 (mfs-stop-search "Detener Búsqueda")
 (mfs-case-sensitive-label "Sensible al Tamaño")
 (mfs-no-matches-found "Nada casó con la búsqueda.")
 (mfs-search-interrupted "Búsqueda abortada.")
 
 ;;;reverting a file
 (are-you-sure-revert "¿Estas seguro que deseas revertir este archivo? Este cambio no puede deshacerse.")
 (are-you-sure-revert-title "¿Revertir?")

 ;; saving a file
 ;; ~a is filled with the filename
 (error-saving "Error al Salvar")
 (error-saving-file/name "Hubo un error mientras salvaba ~a.")
 (error-loading "Error al cargar")
 (error-loading-file/name "Hubo un error mientras cargaba ~a.")
 (unknown-filename "<< desconocido >>")

 ;;; finder dialog
 (must-specify-a-filename "Debes especificar un nombre de archivo")
 (file-does-not-exist "El archivo \"~a\" no existe.")
 (ask-because-file-exists "El archivo \"~a\" ya existía. ¿Deseas reemplazarlo?")
 (dne-or-cycle "El archivo \"~a\" contiene un directorio inexistente o un ciclo.")
 (get-file "Obtener archivo")
 (put-file "Poner archivo")
 (full-pathname "Ruta completa")
 (show-dot-files "Muestra archivos y directorio que comiencen con un punto.")
 (up-directory-button-label "Arriba")
 (add-button-label "Añadir") ;;; for multi-file selection
 (add-all-button-label "Añadir todos") ;;; for multi-file selection
 (remove-button-label "Eliminar") ;;; for multi-file selection
 (file-wrong-form "Ese archivo no tiene el formato correcto.")
 (select-files "Selecciona archivos")
 (select-file "Selecciona un archivo")
 (dir-dne "El directorio no existe.")
 (file-dne "El archivo no existe.")
 (empty-filename "El nombre del archivo debe contener al menos algunas letras.")
 (that-is-dir-name "Eso es un nombre de directorio.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Archivo")
 (edit-menu "Edición")
 (help-menu "Ayuda")
 (windows-menu "Ventana")

 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Archivo")

 (new-info  "Abre un nuevo archivo")
 (new-menu-item "&Nuevo")
 (new-...-menu-item "&Nuevo...")
 
 (open-info "Abre un nuevo archivo del disco")
 (open-menu-item "&Abrir...")
 (open-recent-info "Una lista de archivos abiertos recientemente")
 (open-recent-menu-item "Abrir reciente")

 (revert-info "Revertir este archivo a la copia en disco")
 (revert-menu-item "&Revertir")

 (save-info "Salva este archivo a disco")
 (save-menu-item "&Guardar")

 (save-as-info "Te pide un nombre para salvar este archivo a disco")
 (save-as-menu-item "Guardar &como ...")

 (print-info "Envía este archivo a una impresora")
 (print-menu-item "&Imprimir...")

 (close-info "Cierra este archivo")
 (close-menu-item "&Cerrar")

 (quit-info "Cierra todas las ventanas")
 (quit-menu-item-windows "&Salir")
 (quit-menu-item-others "&Salir") ;; FIXME: salir is exit, so quit is ???
 
 (edit-menu-label "&Edición")
 
 (undo-info "Deshace la acción más reciente")
 (undo-menu-item "&Deshacer")

 (redo-info "Deshace el más reciente deshacer")
 (redo-menu-item "&Rehacer")

 (cut-info "Mueve los elementos seleccionados al porta-papeles para pegarlos más tarde")
 (cut-menu-item "Cor&tar")

 (copy-info "Copia los elementos seleccionados al porta-papeles para pegarlos más tarde")
 (copy-menu-item "&Copiar")

 (paste-info "Pega los elementos copiados o cortados más recientemente en lugar de los objetos seleccionados")
 (paste-menu-item "&Pegar")

 (clear-info "Borra los elementos seleccionados sin afectar el porta-papeles o el pegado")
 (clear-menu-item-windows "Borr&ar")

 (select-all-info "Selecciona el documento completo")
 (select-all-menu-item "&Selecciona todo")
 
 (find-info "Busca una cadena")
 (find-menu-item "&Buscar...")

 (find-again-info "Busca la misma cadena que antes")
 (find-again-menu-item "Volver a buscar")
 
 (replace-and-find-again-info "Reemplaza el texto actual y busca por la misma cadena que antes")
 (replace-and-find-again-menu-item "Reemplaza && buscar otra vez")

 (preferences-info "Configura tus preferencias")
 (preferences-menu-item "Personalizar...")

 (keybindings-info "Muestra los enlaces de tecla activos")
 (keybindings-menu-item "Enlaces de teclas")
 (keybindings-show-active "Mostrar enlaces de tecla activos")
 (keybindings-frame-title "Enlaces de teclas")
 (keybindings-sort-by-name "Ordena por Nombre")
 (keybindings-sort-by-key "Ordena por Llave")
 (keybindings-add-user-defined-keybindings "Añade enlaces de tecla definidos por el usuario...")
 (keybindings-menu-remove "Eliminar ~a")
 (keybindings-choose-user-defined-file "Por favor seleccione un archivo con enlaces de teclas.")

 (user-defined-keybinding-error "Error al ejecutar el enlace de teclas ~a\n\n~a")
 (user-defined-keybinding-malformed-file "El archivo ~a no contiene un módulo escrito usando el lenguaje framework/keybinding-lang.")  

 (insert-text-box-item "Inserta caja de texto")
 (insert-image-item "Inserta imagen...")
 (insert-comment-box-menu-item-label "Insertar Caja de comentario")
 (insert-lambda "Inserta &Lambda")
 
 (wrap-text-item "Ajustar texto al borde")

 (windows-menu-label "&Ventana")
 (bring-frame-to-front "Traer ventana al frente")
 (bring-frame-to-front... "Traer ventana al frente...")
 (most-recent-window "Ventana más reciente")
 
 (view-menu-label "&Muestra")
 (show-overview "Mostrar Panorama")
 (hide-overview "Esconder Panorama")
 (show-module-browser "Mostrar Navegador de Módulos")
 (hide-module-browser "Esconder Navegador de Módulos")

 (help-menu-label "&Ayuda")
 (about-info "Créditos y detalles de esta apliación")
 (about-menu-item "Acerca ...")

  ;;; help-desk-specific menus
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "¿Deseas una nueva ventana o limpiar la ventana actual?")
 (clear-current "Limpiar actual")
 (new-window "Nueva ventana")

 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit "Salir")
 (quit "Abandonar") ;; FIXME: Quit
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-exit "¿Estas seguro(a) que deseas salir?")
 (are-you-sure-quit "¿Estas seguro(a) que deseas abandonar?") ;; FIXME: Quit
 
 ;;; autosaving
 (error-autosaving "Error al auto-salvar \"~a\".")
 (autosaving-turned-off "Auto-salvar está desactivado hasta\n que el archivo sea salvado.")
 (recover-autosave-files-frame-title "Recuperar archivos auto-salvados")
 (autosave-details "Detalles")
 (autosave-recover "Recuperar")
 (autosave-unknown-filename "<<desconocido>>")

 ;; these are labels in a dialog that drscheme displays
 ;; if you have leftover autosave files. to see the dialog,
 ;; start up drscheme and modify (but don't save) a file
 ;; (also, do this with an unsaved file). Wait for the autosave
 ;; files to appear (typically 5 minutes). Kill DrRacket
 ;; and restart it. You'll see the dialog
 (autosave-autosave-label: "Archivo auto-salvado:")
 (autosave-original-label: "Archivo original:")
 (autosave-autosave-label "Archivo auto-salvado")
 (autosave-original-label "Archivo original")
 (autosave-compare-files "Compara archivos auto-salvados")

 (autosave-show-autosave "Auto-salvar archivo") ;; title of a window showing the autosave file

 (autosave-explanation "DrRacket encontró archivos auto-salvados que pueden contener tu trabajo no salvado.")

 (autosave-recovered! "¡Recuperado!") ;; status of an autosave file
 (autosave-deleted "Borrado")       ;; status of an autosave file

 (autosave-error-deleting "Error al borrar ~a\n\n~a") ;; first is a filename, second is an error message from mz.
 (autosave-delete-button "Borrar")
 (autosave-delete-title "Borrar")  ;; title of a dialog asking for deletion confirmation
 (autosave-done "Listo")
 
 ;; appears in the file dialog
 (autosave-restore-to-where? "Escoge un lugar para guardar el archivo auto-salvado.")
 
 ;;; file modified warning
 (file-has-been-modified
  "El archivo ha sido modificado desde la última vez que fue salvado. ¿Sobreescribo las modificaciones?")
 (overwrite-file-button-label "Sobreescribir")
 
 (definitions-modified 
  "El texto de las definiciones ha sido modificado en el sistema de archivos; por favor salve o regrese el texto de las definiciones.")
 (drscheme-internal-error "Error interno de DrRacket")
 
 ;;; tools
 (invalid-tool-spec "La especificación de la herramienta, especificada en el archivo info.rkt de la colección ~a, es inválida.  Esperaba una cadena o una lista no vacía de cadenas y recibí: ~e")
 (error-invoking-tool-title "Error al invocar la herramienta ~s;~s")
 (tool-tool-names-same-length
  "esperaba que `tool-names' y `tools', en el archivo info.rkt de ~s, fueran listas de la misma longitud, pero obtuve ~e y ~e")
 (tool-tool-icons-same-length
  "esperaba que `tool-icons' y  `tools', en el archivo info.rkt de ~s, fueran listas de la misma longitud, pero obtuve ~e y ~e")
 (tool-tool-urls-same-length
  "esperaba que `tool-urls' y `tools' fueran listas del mismo tamaño en info.rkt de ~s, pero obtuve ~e y ~e")
 (error-getting-info-tool "error al cargar el archivo info.rkt de ~s")
 (tool-error-phase1 "Error en la fase 1 de la herramienta ~s; ~s")
 (tool-error-phase2 "Error en la fase 2 de la herramienta ~s; ~s")

 ;;; define popup menu
 (end-of-buffer-define "<< fin de contenedor (buffer) >>") ;; FIXME: buffer --> almacenador intermediario ?
 (sort-by-name "Ordena por nombre")
 (sort-by-position "Ordena por posición en el archivo")
 (no-definitions-found "<< no se encontraron definiciones >>")
 (jump-to-defn "Saltar a la definición de ~a")

 (recent-items-sort-by-age "Ordena por Edad")
 (recent-items-sort-by-name "Ordena por Nombre")
 
  ;;; show menu
 (hide-definitions-menu-item-label "Esconder &Definiciones")
 (show-definitions-menu-item-label "Mostrar &Definiciones")
 (definitions-menu-item-help-string "Mostrar/Esconder la ventana de definiciones")
 (show-interactions-menu-item-label "Mostrar &Interacciones")
 (hide-interactions-menu-item-label "Esconder &Interacciones")
 (interactions-menu-item-help-string "Mostrar/Esconder la ventana de Interacciones")
 (show-toolbar "Mostrar &Barra de herramientas")
 (hide-toolbar "Esconder &Barra de Herramientas")

 ;;; file menu
 (save-definitions-as "Salvar Definiciones como...")
 (save-definitions "Salvar Definiciones")
 (print-definitions "Imprimir Definiciones...")
 (about-drscheme "Acerca DrRacket")
 (save-other "Salvar otros")
 (save-definitions-as-text "Salvar Definiciones como Texto...")
 (save-interactions "Salvar Interacciones")
 (save-interactions-as "Salvar Interacciones como...")
 (save-interactions-as-text "Salvar Interacciones como Texto...")
 (print-interactions "Imprimir Interacciones...")
 (new-tab "Nueva pestaña")
 (close-tab "Cerrar pestaña") ;; must not have any &s in it.

 ;;; edit-menu
 (split-menu-item-label "&Dividir")
 (collapse-menu-item-label "C&olapsar")
 
 ;;; language menu
 (language-menu-name "&Lenguaje")
 
 ;;; scheme-menu
 (scheme-menu-name "Ra&cket")
 (execute-menu-item-label "Ejecutar")
 (execute-menu-item-help-string "Reinicia el programa en la ventana de definiciones")
 (break-menu-item-label "Interrumpir")
 (break-menu-item-help-string "Interrumpe la evaluación actual")
 (kill-menu-item-label "Terminar")
 (kill-menu-item-help-string "Terminar la evaluación actual")
 (clear-error-highlight-menu-item-label "Eliminar resaltado de error")
 (clear-error-highlight-item-help-string "Elimina el resaltado rosa de errores")
 (reindent-menu-item-label "&Re-sangrar")
 (reindent-all-menu-item-label "Re-sangrar &todo")
 (semicolon-comment-out-menu-item-label "&Comentar con punto y coma")
 (box-comment-out-menu-item-label "&Comentar con una Caja")
 (uncomment-menu-item-label "&Des-comentar")

 (convert-to-semicolon-comment "Convertir a comentario con punto y coma")
 
 ;;; executables
 (create-executable-menu-item-label "Crear ejecutable...")
 (create-executable-title "Crear Ejecutable")
 (must-save-before-executable "Debes salvar tu programa antes de hacer un ejecutable.")
 (save-a-mred-launcher "Salvar un lanzador de GRacket")
 (save-a-mzscheme-launcher "Salvar un lanzador de Racket")
 (save-a-mred-stand-alone-executable "Salvar un ejecutable autocontenido de GRacket")
 (save-a-mzscheme-stand-alone-executable "Salvar un ejecutable autocontenido de Racket")

 (definitions-not-saved "La ventana de definiciones no ha sido salvada. El ejecutable usará la última versión salvada de la ventana de definiciones. ¿Desea continuar?")
 (launcher "Lanzador")
 (stand-alone "Auto-contenido")
 (executable-type "Tipo")
 (executable-base "Base")
 (filename "Nombre de archivo: ")
 (create "Crear")
 ;; "choose-an-executable" changed to "specify-a"
 ;(please-choose-an-executable-filename "Por favor selecciona un nombre de archivo para salvar el ejecutable.")
 ;; Replaced by generic ~a-must-end-with-~a
 ;(windows-executables-must-end-with-exe
 ; "El nombre de archivo \n\n  ~a\n\nes ilegal.  En Windows, los ejecutables deben tener terminación .exe.")
 ;(macosx-executables-must-end-with-app
 ; "El nombre de archivo\n\n  ~a\n\nes ilegal.  En MacOS X, los ejecutables deben tener terminación .app.")
 (warning-directory-will-be-replaced
  "ADVERTENCIA: el directorio:\n\n  ~a\n\nserá reemplazado.  ¿Continuar?")
 
 (create-servlet "Crear Servlet...")
 
 ;; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Crear Servlet no funciona con el lenguaje ~a.")

 ;;; buttons
 (execute-button-label "Ejecutar") 
 (save-button-label "Salvar")
 (break-button-label "Interrumpir")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Busca en el Módulo de Ayuda \"~a\"")
 (exact-lucky-search-help-desk-for "Búsqueda precisa y con suerte en el Módulo de Ayuda \"~a\"") ;; FIXME:  Exact lucky search in Help Desk for
 
 ;; collapse and expand popup menus
 (collapse-sexp "Colapsar expresión-s")
 (expand-sexp "Expandir expresión-s")

 ;;; fraction dialog
 (enter-fraction "Introducir Fracción")
 (whole-part "Parte entera")
 (numerator "Numerador")
 (denominator "Denominador")
 (invalid-number "Número inválido: debe ser un número real exacto, no entero.")
 (insert-fraction-menu-item-label "Insertar Fracción...")
 
 ;; number snip popup menu
 (show-decimal-expansion "Ver expansión decimal")
 (show-mixed-fraction-view "Ver como una fracción mixta")   
 (show-improper-fraction-view "Ver como fracción impropia")
 (show-more-decimal-places "Muestra más posiciones decimales")
 
 ;;; TeachPack messages
 (select-a-teachpack "Selecciona un Paquete de Enseñanza")
 (clear-teachpack "Limpia el Paquete de Enseñanza ~a")
 (teachpack-error-label "DrRacket - Paquete de Enseñanza error")
 (teachpack-didnt-load "El archivo del Paquete de Enseñanza ~a no se cargó apropiadamente.")
 (add-teachpack-menu-item-label "Añadir un Paquete de Enseñanza...")
 (clear-all-teachpacks-menu-item-label "Limpia Todos los Paquetes de Enseñanza")
 (drscheme-teachpack-message-title "DrRacket Paquete de Enseñanza")
 (already-added-teachpack "El paquete de enseñanza ~a ya estaba cargado")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Por favor selecciona un lenguaje.  La mayoría de los estudiantes de cursos introductorios deberían usar el lenguaje por omisión.")
 (language-dialog-title "Configurar Lenguaje")
 (case-sensitive-label "Sensible a mayúsculas") ;; FIXME: Case sensitive
 (output-style-label "Estilo de salida")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (print-printing-style "print")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insertar caracteres de nueva línea en valores impresos")
 (input-syntax "Sintáxis de Entrada")
 (dynamic-properties "Propiedades dinámicas")
 (output-syntax "Sintáxis de Salida")
 (no-debugging-or-profiling "No depurando o delineando")
 (debugging "Depurando")
 (debugging-and-profiling "Depurando y delineando")
 (test-coverage "Paquete de prueba de cobertura sintáctica") ;; FIXME: Syntactic test suite coverage
 (show-details-button-label "Mostrar Detalles")
 (hide-details-button-label "Esconder Detalles")
 (choose-language-menu-item-label "Seleccionar Lenguaje...")
 (revert-to-language-defaults "Revertir a los Valores por Omisión del Lenguaje")
 (fraction-style "Estilo de Fracciones")
 (use-mixed-fractions "Fracciones mixtas")
 (use-repeating-decimals "Decimales repetidos")
 (decimal-notation-for-rationals "Usar notación decimal para racionales")
 (please-select-a-language "Por favor selecciona un lenguaje")

 
 ;;; languages
 (beginning-student "Estudiante Principiante")
 (beginning-one-line-summary "define, cond, structs, constantes, y primitivas") 
 (beginning-student/abbrev "Estudiante Principiante con Abreviaturas de Listas")
 (beginning/abbrev-one-line-summary "Principiante con estilo de impresión de listas en el REPL") 
 (intermediate-student "Estudiante Intermedio")
 (intermediate-one-line-summary "Estudiante Principiante más alcance léxico")
 (intermediate-student/lambda "Estudiante Intermedio con lambda")
 (intermediate/lambda-one-line-summary "Estudiante Intermedio más funciones de alto-nivel")
 (advanced-student "Estudiante Avanzado")
 (advanced-one-line-summary "Estudiante Intermedio más lambda y mutación") 
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (professional-languages "Languajes Profesionales")
 (teaching-languages "Languajes de Enseñanza")
 (experimental-languages "Languajes Experimentales") 
 (no-language-chosen "No ha seleccionado un lenguaje")
 (pretty-big-scheme "Muy Grande")
 (pretty-big-scheme-one-line-summary "Añade syntaxis y funciones de los lenguajes HtDP")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS, sin ornamentos")
 (expander "Expandir")
 (expander-one-line-summary "Expande expresiones en lugar de evaluarlas")
 
 
 
 (initial-language-category "Lenguaje inicial")
 (unknown-debug-frame "[desconocido]")
 
 ;(module-language-one-line-summary "Ejecutar crea un REPL en el contexto del módulo, incluyendo el lenguaje declarado para el módulo")

  ;;; from the `not a language language' used initially in drscheme.
 (must-choose-language "DrRacket no puede procesar programas hasta que selecciones un lenguaje de programación.")
 
 ; next two appear before and after the name of a text book (which will be in italics)
 (using-a-textbook-before "¿Utilizas ")
 (using-a-textbook-after "?")
 
                                        ; next two are before and after a language
 (start-with-before "Empezar con ")

 (seasoned-plt-schemer? "¿Usurio experimentado de PLT Scheme?")
 (looking-for-standard-scheme? "¿Buscas un Scheme estándar?")

                                        ; the three string constants are concatenated together and the middle
                                        ; one is hyperlinked to the dialog that suggests various languages
 (get-guidance-before "Selecciona la opción  “Seleccionar lenguaje...”  en el menú “Languaje, o ")
 (get-guidance-during "permitenos orientarte")
 (get-guidance-after ".")

 
 ;;; debug language
 (backtrace-window-title "Backtrace - DrRacket")
 (files-interactions "Interacciones de ~a") ;; filled with a filename
 (current-interactions "interacciones")
 (current-definitions "definiciones")
 (mzscheme-w/debug "Texto (MzScheme, incluye R5RS)")
 (mzscheme-one-line-summary "Implementación de Scheme del PLT") 
 (mred-w/debug "Gráfico (MrEd, incluye MzScheme)")
 (mred-one-line-summary "Añade soporte para IGU a MzScheme")

 ;; profiling
 (profiling-low-color "Tenue")
 (profiling-high-color "Fuerte")
 (profiling-choose-low-color "Por favor selecciona un color tenue")
 (profiling-choose-high-color "Por favor selecciona un color fuerte")
 (profiling "Delineando")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Delineando Rango de Color")
 (profiling-scale "Delineando Escala de Color")
 (profiling-sqrt "Raíz cuadrada")
 (profiling-linear "Lineal")
 (profiling-square "Cuadrado")
 (profiling-number "Número de llamadas")
 (profiling-time "Tiempo acumulado")
 (profiling-update "Actualiza Delineado")
 (profiling-col-percent-time "% Tiempo")
 (profiling-col-function "Función")
 (profiling-col-time-in-msec "Msec")
 (profiling-col-calls "Llamadas")
 (profiling-show-profile "Muestra Delineado")
 (profiling-hide-profile "Esconder Profile")
 (profiling-unknown-src "<< desconocido >>")
 (profiling-no-information-available "No hay información de delineado disponible. Por favor, asegúrate que la opción de delineado está activada para el lenguaje y de haber ejecutado su programa.")
 (profiling-clear? "Cambiar la ventana de definiciones invalida la información de delineado. ¿Continuar?")
 
 ;; test coverage
 (test-coverage-clear? "El cambiar la ventana de definiciones invalida la información de pruebas de cobertura. ¿Continuar?")
 (test-coverage-clear-and-do-not-ask-again "Sí y no me preguntes más")
 (test-coverage-ask? "¿Preguntar acerca de las pruebas de  cobertura?")

 ;; tracing
 (tracing-enable-tracing "Habilitar trazado")
 (tracing-show-tracing-window "Mostrar trazado")
 (tracing-hide-tracing-window "Esconder trazado")
 (tracing-tracing-nothing-to-show "No hay resultados de trazado disponibles.  Asegúrese que su lenguaje soporte trazado y que esté habilitado.")

 
 ;;; repl stuff
 (evaluation-terminated "Evaluación Terminada")
 (evaluation-terminated-explanation
  "El hilo de control de la evaluación ya no se está ejecutando, por lo que no se puede efectuar ninguna evaluación hasta la siguiente ejecución.")
 (last-stack-frame "Mostrar el último marco (frame) en el stack")
 (last-stack-frames "Mostrar los últimos ~a marcos (frames)")
 (next-stack-frames "Mostrar los siguientes ~a marcos (frames) en el stack")
 
 ;;; welcoming message in repl
 (language "Lenguaje")
 (custom "custom")
 (teachpack "Paquete de Enseñanza")
 (welcome-to "Bienvenido a")
 (version "versión")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "¿Quieres terminar la evaluación?")
 (just-break "Interrupción Simple")
 (kill "Terminar")
 (kill? "¿Terminar?")
 
 ;;; version checker
 (version:update-menu-item "Buscando Actualizaciones...")
 (version:update-check "Revisar Actualización")

 ;; special menu
 (special-menu "Especial")

 ;; large semi colon letters
 (insert-large-letters... "Insertar Letras Grandes...")
 (large-semicolon-letters "Letas Dos Puntos Grandes")
 (text-to-insert "Texto a insertar")

 (module-browser-filename-format "Nombre de archivo completo: ~a (~a líneas)")
 (module-browser-root-filename "Raíz del archivo: ~a")
 (module-browser-font-size-gauge-label "Tamaño de fuente")
 (module-browser-progress-label "Módulo de resumen de progreso")
 (module-browser-adding-file "Añadiendo archivo: ~a...")
 (module-browser-laying-out-graph-label "Depositando gráfica")
 (module-browser-open-file-format "Abriendo ~a")
 (module-browser "Navegador de Módulos")
 (module-browser... "Navegador de Módulos...")
 (module-browser-error-expanding "Error al expandir el programa:\n\n~a")
 (module-browser-show-lib-paths "Muestra archivos cargados por rutas (lib ..)")
 (module-browser-progress "Navegador de Módulos: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Navegador de Módulos: compilando las definiciones")
 (module-browser-show-lib-paths/short "Seguir los requires de bibliotecas (lib)") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "Seguir los requires de bibliotecas (planet)") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Refrescar") ;; button label in show module browser pane in drscheme window.
; (module-browser-only-in-plt-and-module-langs
;  "El navegador de módulos está disponible para programas en los lenguajes PLT y en el lenguaje con módulos (y únicamente para programas que incluyan módulos).")
 (module-browser-name-length "Longitud de nombre")
 (module-browser-name-short "Corto")
 (module-browser-name-medium "Medio")
 (module-browser-name-long "Largo")
 (module-browser-open-all "Abrir todos los archivos mostrados")

 ;; Birthdays section
 (happy-birthday-matthias "¡Feliz cumpleaños Matthias!")
 (happy-birthday-matthew "¡Feliz cumpleaños Matthew!")
 (happy-birthday-shriram "¡Feliz cumpleaños Shriram!")
 
 (mrflow-using-default-language-title "Lenguaje por omisión usado")
 (mrflow-using-default-language "El lenguaje usado actualmente no tiene un tipo tabla definido para sus primitivas.  Usaré R5RS Scheme en su lugar.")
 (mrflow-button-title "Analizar")
;(mrflow-language-primitives-error "Nombre de archivo incorrecto para la tabla de tipos primitivos del lenguaje: ~a")
;(mrflow-language-primitives-error-title "Error de Primitivas del Lenguaje")
 (mrflow-popup-menu-hide-errors "Esconder Errores")
 (mrflow-popup-menu-hide-type "Esconder Tipo")
 (mrflow-popup-menu-show-errors "Mostrar Errores")
 (mrflow-popup-menu-show-type "Mostrar Tipo")
;(mrflow-read-exception "Leer excepción: ~a")
;(mrflow-read-exception-title "Leer Excepción")
;(mrflow-syntax-exception "Excepción de sintaxis: ~a")
;(mrflow-syntax-exception-title "Excepción de Sintaxis")
;(mrflow-unknown-exception-title "Excepción Desconocida")
;(mrflow-unknown-exception "Excepción Desconocida: ~a")
 
 (snips-and-arrows-popup-menu-tack-all-arrows "Ligar todas las Flechas")
 (snips-and-arrows-popup-menu-untack-all-arrows "Desligar todas las Flechas")
 (snips-and-arrows-user-action-disallowed-title "Cambios del usuario no permitidos actualmente")
 (snips-and-arrows-user-action-disallowed "Cambios del usuario no son permitidos en editores que contienen partes insertadas por herramientas.  Esconde todas las partes antes de modificar el contenido del editor.")
 (snips-and-arrows-hide-all-snips-in-editor "Esconder todas las partes en el editor")

 (xml-tool-insert-scheme-box "Insertar Caja de Racket")
 (xml-tool-insert-scheme-splice-box "Insertar Caja de Unión de Racket")
 (xml-tool-insert-xml-box "Insertar Caja de XML")
 (xml-tool-xml-box "Caja de XML")
 (xml-tool-scheme-box "Caja de Racket")
 (xml-tool-scheme-splice-box "Caja de Unión de Racket")
 (xml-tool-switch-to-scheme "Cambiar a caja de  Racket")
 (xml-tool-switch-to-scheme-splice "Cambiar a caja de unión de Racket")
 (xml-tool-eliminate-whitespace-in-empty-tags "Eliminar blancos en etiquetas vacías")
 (xml-tool-leave-whitespace-alone "Dejar blancos por la paz")

 (show-recent-items-window-label "Archivos abiertos recientemente")
 (show-recent-items-window-menu-item "Muestra Archivos Abiertos recientemente en otra ventana")
 (number-of-open-recent-items "Número de elementos recientes")
 (switch-anyway "Cambia de archivo de cualquier forma")

 (stepper-program-has-changed "ADVERTENCIA: El programa ha cambiado.")
 (stepper-program-window-closed "ADVERTENCIA: La ventana del programa ha desaparecido.")

 (stepper-name "Stepper")
 (stepper-language-level-message
  "El nivel del lenguaje es \"~a\".  Actualmente el Stepper funciona para los niveles \"~a\" al \"~a\".")
 (stepper-button-label "Paso")
 (stepper-previous-application "Aplicación")
 (stepper-previous "Paso")
 (stepper-next "Paso")
 (stepper-next-application "Aplicación")
 (stepper-jump-to-beginning "Hogar")
 
 (dialog-back "Atrás")
 
 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "El programa en la ventana de definiciones sigue corriendo.  ¿Cerrar de cualquier forma?")
 (program-has-open-windows "El programa en la ventana de definiciones abrió otras ventanas.  ¿Cerrar esta ventana de cualquier forma?")

 ;; ml-command-line-arguments is for the command line arguments
 ;; label in the module language details in the language dialog.
 (ml-command-line-arguments "Argumentos en la línea de comandos como un vector de cadenas, en leer sintaxis")
 
 ;; ml-cp names are all for the module language collection path
 ;; configuration. See the details portion of the language dialog
 ;; for the module language (at the bottom).
 (ml-cp-default-collection-path "<<rutas de colección por omisión>>")

 ;; in std get-directory 
 (ml-cp-choose-a-collection-path "Por favor selecciona una ruta de colecciones")

 ;; err msg when adding default twice
 (ml-cp-default-already-present "La ruta de colecciones por omisión ya está presente")
 
 ;; title of this section of the dialog (possibly the word
 ;; `Collection' should not be translated)
 (ml-cp-collection-paths "Rutas de colecciones")

 ;; button labels
 (ml-cp-add "Añadir")
 (ml-cp-add-default "Añadir por omisión")
 (ml-cp-remove "Eliminar")
 (ml-cp-raise "Elevar")
 (ml-cp-lower "Bajar")

 ;; Profj
 (profj-java "Java")
 (profj-java-mode "Modo Java")
 (profj-java-mode-color-keyword "llave")
 (profj-java-mode-color-string "cadena")
 (profj-java-mode-color-literal "literal")
 (profj-java-mode-color-comment "comentario")
 (profj-java-mode-color-error "error")
 (profj-java-mode-color-identifier "identificador")
 (profj-java-mode-color-default "por omisión")

 (profj-insert-java-comment-box "Insertar Caja para Comentarios Java")
 (profj-insert-java-interactions-box "Insertar Caja de Interacciones Java")

 
 ;; The Test Suite Tool
 ;; Errors
 (test-case-empty-error "Caso de prueba vacío")
 (test-case-too-many-expressions-error "Demasiadas expresiones en el caso de prueba.")
 ;; DrRacket window menu items
 (test-case-insert "Insertar Caso de Prueba")
 (test-case-disable-all "Deshabilitar todos los Casos de Prueba")
 (test-case-enable-all "Habilitar todos los Casos de Prueba")
 ;; NOTE: The following three string constants are labels of the test-case fields. The width
 ;;       of the field is determined by the length of the longest of the following three words.
 ;;       if the words are too long the test case will take up too much horizontal room and
 ;;       not look very good.
 ;; This string is the label of the expression that is being tested in a test case.
 (test-case-to-test "Por probar")
 ;; This string is the label of the expression that is the expected value of the to-test expression.
 (test-case-expected "Esperado")
 ;; This string is the label of the actual result of the to test expression.
 (test-case-actual "Obtenido")
 (test-case-predicate "Predicado")
 (test-case-should-raise "Debe provocar") ;; should raise?
 ;; The label of a field of the test-case that describes the expected error message of a test case
 (test-case-error-message "Mensaje de Error")

 (test-case-menu-title "Caso de prueba")
 (test-case-switch-to-error-box "Cambiar a caja de prueba de error")
 (test-case-switch-to-nonerror-box "Cambiar a caja de prueba sin-error")
 (test-case-collapse "Colapsar el caso de prueba")
 (test-case-show-actual "Mostrar valor actual")
 (test-case-enable "Activar caso de prueba")
 (test-case-show-predicate "Mostrar predicado")
 (test-case-show-error-message "Mostrar mensaje de error")
 (test-case-convert-to-text "Convertir a texto")

  ;; Profj Boxes
 (profjBoxes-empty-error "Interacción vacía")
 (profjBoxes-too-many-expressions-error "Demasiadas expresiones en una caja")
 (profjBoxes-interactions-label "Interacciones")
 (profjBoxes-bad-java-id-error "Java ID mal formado")
 (profjBoxes-examples-label "Ejemplos")
 (profjBoxes-add-new-example-button "Añadir nuevo ejemplo")
 (profjBoxes-type "Tipo")
 ;; The Java identifier of an example of data
 (profjBoxes-name "Nombre")
 (profjBoxes-value "Valor")
 (profjBoxes-insert-java-examples "Insertar Ejemplos de Java")
 (profjBoxes-insert-java-interactions "Insertar Interacciones Java")

 ;; Slideshow
 (slideshow-hide-picts "Mostrar Cajas Anidadas")
 (slideshow-show-picts "Mostrar Imágenes")
 (slideshow-cannot-show-picts "No puedo mostrar imágenes; ejecuta el programa para capturar primero sus tamaños")
 (slideshow-insert-pict-box "Insertar Caja de Imagen") 

 
 ;; GUI Tool
 (gui-tool-heading "Herramienta IGU")
 (gui-tool-before-clicking-message "Antes de seleccionar un ícono de herramienta, utiliza \"Insertar IGU\" del menú \"Especial\" para insertar un elemento raíz IGU, o bien selecciona un IGU previamente insertado.")
 (gui-tool-show-gui-toolbar "Mostrar la barra de herramientas IGU")
 (gui-tool-hide-gui-toolbar "Esconder la barra de herramientas IGU")
 (gui-tool-insert-gui "Insertar IGU")
 
 )
