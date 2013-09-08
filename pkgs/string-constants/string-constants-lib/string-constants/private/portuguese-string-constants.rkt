#|

When modifying the string constants files,
please adhere to these guidelines:

- All the entries in english-string-constants.rkt have the same format
  (name string).  If the purpose of an entry you are adding to the
  file is not clear from just the name and string, put a comment next
  to the entry explaining what the string is going to be used for and
  in what context.
  That's especially true for strings that contain things like "~a".
  Systematically describe what the "~a" is going to be replaced with.
  When we have to translate strings like "deleting ~a", we translators
  need to know what "~a" is going to be (in particular, in some
  languages like French, we need to know whether the "~a" is going to
  be a masculine or feminine word, or whether it's going to be
  singular or plural, etc).

- When adding a bunch of new entries, put together in a section the
  entries that logically belong together.  Give a title to your
  section, so we have an idea of what the strings are for.  Don't mix
  in the same section strings that have nothing to do with each other,
  that's only going to confuse us.  Do not start a new section if
  there's already one that deals with the same thing.  Dumping all the
  new entries in random order at the end of the file is not a good way
  to have your new entries translated in a timely manner...

- Before adding strings for your new pet tool, check whether you can't
  re-use strings that already exist.  There's no need for yet another
  "Ok" string...

- If you modify an existing string in english-string-constants.rkt, go
  through all the *-string-constants.rkt files for the other languages,
  comment out the old version of the modified string in each of these
  files, and put a short comment there telling us the English string
  has changed and needs to be re-translated.  Do not erase the old
  version, it might help us translate the new one.  Do not move it
  either.  Just comment it out and add the short comment.  After the
  next git update DrRacket will automatically tell us translators that
  a new string needs to be translated, we will find your comment in
  the file, and know what to do.
	Some evil evil people might think that, since DrRacket automatically
  informs us of new strings to be translated, an easier thing to do
  when modifying an existing string would be to simply rename it at
  the same time.  This works, except that if you do that, we
  translators will get two warnings from DrRacket:
		language english had but french does not:
		(new-name "New String")
		language french had but english does not:
		(old-name "Old String")
  then we translators will be left to wonder whether the two things
  are related or not, and whether we can safely base our translation
  of "New String" on the translation of "Old String" (since the two
  strings are likely to be close in meaning).  Worse, we might not
  even realize the two strings are related and translate "New String"
  from scratch, just to realize later that it's only a variation of
  "Old String".  I can tell you that nothing pisses off a translator
  more than having to translate pretty much the same string twice
  just because *you* were too lazy to inform us that it was just a
  slight modification to an existing string rather than an entirely
  new one.  Conclusion: do not change the name of a string unless you
  have some really good reason to do so.

- Please think hard before choosing a string and make sure it means
  what you want it to mean.  That way you won't have to change it
  later, and we won't have to retranslate it.

- Please think hard before choosing the name for a string.  Use
  meaningful names.  "error" or "ok" are not meaningful names.  Prefix
  all related names with a common prefix (the name of your tool or
  module).  String names are not the right place to save on typing.

- If, for some reason, you really have to change the name of a string
  (like, because the original name you gave it really sucked...),
  change the name of the string in *ALL* the *-string-constants.rkt
  files.  That's a modification you can do without the help of a
  translator, so do it yourself.  It's not the job of the translators
  to clean up your naming mess for you.  Besides, you are the one who
  knows what you changed, so leaving the translators having to guess
  what you did is Not Nice(tm).

- If, for some reason, you need to remove a string (i.e. you changed
  your code and don't need the string anymore), remove the string in
  *ALL* the *-string-constants.rkt files.  Again, you don't need the
  help of a translator to do that.  If you're not sure whether you
  might need the string in the future or not, just comment it out in
  *ALL* the files.

|#

(module portuguese-string-constants "string-constant-lang.rkt"
 ;;; when translating this constant, substitute name of actual language for `English'
 (is-this-your-native-language "Português é a tua língua materna?")

 (are-you-sure-you-want-to-switch-languages
  "Isto vai modificar a língua da interface gráfica, o que requere que reinicie o DrRacket. Tem a certeza que deseja prosseguir?")

 (interact-with-drscheme-in-language "Usar o DrRacket em Português")

 ;; these two should probably be the same in all languages except English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Aceitar e Sair")
 (accept-and-exit "Aceitar e Sair")
 
 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
 (ok "OK")
 (cancel "Cancelar")
 (abort "Abortar")
 (untitled "SemNome")
 (untitled-n "SemNome ~a")
 (warning "Aviso")
 (error "Erro")
 (close "Fechar") ;; as in, close an open window. must match close-menu-item
                  ;; in the sense that, when the &s have been stripped from
                  ;; close-menu-item, it must be the same string as this.
 (stop "Parar")   
 (&stop "&Parar") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Tem a certeza que deseja remover ~a?") ;; ~a is a filename or directory name
 (ignore "Ignorar")
 (revert "Recuperar")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "Não voltar a perguntar (utilizar sempre a escolha actual)")
 ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "Não voltar a perguntar")

 ;;; important urls
 (web-materials "Sítios Web Relacionados") ;; menu item title
 (tool-web-sites "Sítios Web de Ferramentas")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

 ;;; bug report form
 (cancel-bug-report? "Cancelar relatório de erro?")
 (are-you-sure-cancel-bug-report?
  "Tem a certeza que deseja cancelar o envio deste relatório de erro?")
 (bug-report-form "Formulário de relatório de erro")
 (bug-report-field-name "Nome")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Sumário")
 (bug-report-field-severity "Gravidade")
 (bug-report-field-class "Categoria")
 (bug-report-field-description "Descrição")
 (bug-report-field-reproduce1 "Passos a")
 (bug-report-field-reproduce2 "Reproduzir")
 (bug-report-field-environment "Ambiente")
 (bug-report-field-docs-installed "Documentação Instalada")
 (bug-report-field-collections "Colecções")
 (bug-report-field-human-language "Língua")
 (bug-report-field-memory-use "Utilização de Memória")
 (bug-report-field-version "Versão")
 (bug-report-synthesized-information "Informação Gerada")  ;; dialog title
 (bug-report-show-synthesized-info "Mostrar Informação Gerada")
 (bug-report-submit "Enviar")
 (bug-report-submit-menu-item "Enviar relatório de erro...") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "Erro a Enviar relatório de erro")
 (error-sending-bug-report-expln "Um erro ocorreu enquanto o relatório de erro estava a ser enviado. Se a sua ligação à internet está a funcionar correctamente, por favor visite:\n\n http://bugs.racket-lang.org/\n\ne envie o erro através do nosso formulário online. Pedimos desculpa pelo incómodo.\n\nA mensagem de erro é:\n~a")
 (illegal-bug-report "Relatório de Erro Ilegal")
 (pls-fill-in-field "Por favor preencha o campo \"~a\"")
 (malformed-email-address "Endereço de email mal formado")
 (pls-fill-in-either-description-or-reproduce "Por favor preencha o campo de Descrição ou o campo com os Passos para Reproduzir o erro.")

 ;;; check syntax
 (check-syntax "Verificar Sintaxe")
 (cs-italic "Itálico")
 (cs-bold "Negrito")
 (cs-underline "Sublinhar")
 (cs-change-color "Modificar cor")
 (cs-tack/untack-arrow "Marcar/Desmarcar Seta(s)")
 (cs-jump-to-next-bound-occurrence "Saltar para a Próxima Ocorrência")
 (cs-jump-to-binding "Ir para a Próxima Atribuição")
 (cs-jump-to-definition "Ir para a Definição")
 (cs-error-message "Mensagem de Erro")
 (cs-open-file "Abrir ~a")
 (cs-rename-var "Renomear ~a")
 (cs-rename-id "Renomear Identificador")
 (cs-rename-var-to "Renomear ~a para:")
 (cs-name-duplication-error "O novo nome que escolheu, ~s, entra em conflito com um nome já existente no ambiente corrente.")
 (cs-rename-anyway "Renomear na Mesma")
 (cs-status-init "Verificar Sintaxe: A Inicializar ambiente para o código do utilizador")
 (cs-status-coloring-program "Verificar Sintaxe: a colorir expressão")
 (cs-status-eval-compile-time "Veriricar Sintaxe: tempo de compilação do eval")
 (cs-status-expanding-expression "Verificar Sintaxe: a expandir a expressão")
 (cs-mouse-over-import "a ligar ~s importada de ~s")

 (cs-lexical-variable "variável léxica")
 (cs-imported-variable "variável importada")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Apenas Leitura")
 (auto-extend-selection "Auto-Extensível")
 (overwrite "Reescrever")
 (running "a executar")
 (not-running "parado")
 
 ;;; misc
 (welcome-to-something "Benvindo a ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Benvindo ao DrRacket, versão ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "Benvindo ao DrRacket")

 (goto-line "Ir para a Linha")
 (goto-line-invalid-number
  "~a não é um número de linha válido. Deve ser um inteiro entre 1 e ~a")
 (goto-position "Ir para a Posição")
 (no-full-name-since-not-saved
  "O ficheiro não tem um nome completo porque ainda não foi gravado.")
 (cannot-open-because-dne "Impossível abrir ~a porque não existe.")
 
  (needs-execute-language-changed
   "ATENÇÃO: A língua mudou. Clique Correr.")
  (needs-execute-teachpack-changed
   "ATENÇÃO: Os pacotes de ensino mudaram. Clique Correr.")
  (needs-execute-defns-edited
   "ATENÇÃO: A janela de definições mudou. Clique Correr.")
  
 (file-is-not-saved "O ficheiro \"~a\" não está gravado.")
 (save "Gravar")
 (close-anyway "Fechar na Mesma")
 (clear-anyway "Limpar na Mesma")

 ;; menu item title
 (log-definitions-and-interactions "Arquivar Definições e Interacções...")
 (stop-logging "Parar Arquivação")
 (please-choose-a-log-directory "Por favor escolha uma directoria de arquivação")
 (logging-to "A arquivar para: ")
 (erase-log-directory-contents "Apagar todo o conteúdo da directoria de arquivação: ~a?")
 (error-erasing-log-directory "Erro a apagar o conteúdo da directoria de arquivação.\n\n~a\n")

 ;; modes
 (mode-submenu-label "Modos")
 (scheme-mode "Modo Scheme")
 (racket-mode "Modo Racket")
 (text-mode "Modo Texto")

 (scheme-mode-color-symbol "Símbolo")
 (scheme-mode-color-keyword "Palavra-Chave")
 (scheme-mode-color-comment "Comentário")
 (scheme-mode-color-string "String")
 (scheme-mode-color-constant "Constante")
 (scheme-mode-color-parenthesis "Parêntesis")
 (scheme-mode-color-error "Erro")
 (scheme-mode-color-other "Outro")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Escolher uma cor para ~a")
 (preferences-colors "Cores") ;; used in the preferences dialog
 
 (url: "URL:")
 (open-url... "Abrir URL...")
 (open-url "Abrir URL")
 (browse... "Procurar...")
 (bad-url "URL Errado")
 (bad-url:this "URL Errado: ~a")
 
 ;; Help Desk
 (help "Ajuda")
 (help-desk "Directorio de Ajuda")
 (plt:hd:search "Procurar")
 (plt:hd:feeling-lucky "Sinto-me Sortudo")
 (plt:hd:home "Início do Directorio de Ajuda") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Procurar por palavra-chave")
 (plt:hd:search-for-keyword-or-index "Procurar por palavra-chave ou índice")
 (plt:hd:search-for-keyword-or-index-or-text "Procurar por palavra-chave, índice ou texto")
 (plt:hd:exact-match "Procura exacta")
 (plt:hd:containing-match "A conter a procura")
 (plt:hd:regexp-match "Procura de expressão regular (regexp)")
 (plt:hd:find-docs-for "Procurar documentos por:")
 (plt:hd:search-stopped-too-many-matches "[Procura Abortada: demasiados resultados]")
 (plt:hd:nothing-found-for "Nada encontrado para ~a")
 (plt:hd:and "e")
 (plt:hd:refresh "Refrescar")
 (plt:hd:refresh-all-manuals "Refrescar todos os manuais")
 (plt:hd:manual-installed-date "(instalado a ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "Reactualização de manuais")
 (plt:hd:refresh-downloading... "A tirar ~a...")
 (plt:hd:refresh-deleting... "A remover a versão antiga de ~a...")
 (plt:hd:refresh-installing... "A instalar nova versão de ~a...")
 (plt:hd:refresh-clearing-indices "A apagar os índices em cache")
 (plt:hd:refreshing-manuals-finished "Concluído.")
 (plt:hd:about-help-desk "Sobre o Directorio de Ajuda")
 (plt:hd:help-desk-about-string
  "O Directorio de Ajuda é um recurso completo de informação sobre o software da PLT, incluindo o DrRacket, MzScheme e o MrEd.\n\nVersão ~a\n Copyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Ajuda sobre a Ajuda")
 (plt:hd:help-on-help-details "Para ajuda sobre como utilizar o Directorio de Ajuda, seguir a ligação `Como usar o Directorio de Ajuda' no inicio do Directorio de Ajuda. (Para ir para o inicio se ainda não estiver lá, clique no botão `Início' no topo da janela do Directorio de Ajuda.)")
  (reload "Actualizar") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Seleccionou uma ligação para um conteúdo na web. Quer visualizá-lo no navegador do Directorio de Ajuda, ou prefere usar um navegador independente?")
  (plt:hd:homebrew-browser "Navegador do Directorio de Ajuda") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Navegador Independente") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "URLs Externos na Ajuda")
  (plt:hd:use-homebrew-browser "Usar o navegador do Directorio da Ajuda para URLs Externos")
  (plt:hd:new-help-desk "Novo Directorio de Ajuda")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Ordem de Procura dos Manuais")

  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "Usar o tamanho de fonte do DrRacket")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "Isto é apenas um texto exemplo para a opção do tamanho da fonte. Abra o directório de ajuda (do menu Ajuda) para seguir estas ligações.")
  
 ;; Help desk htty proxy
 (http-proxy "HTTP Proxy")
 (proxy-direct-connection "Conexão Directa")
 (proxy-use-proxy "Usar Proxy:")
 (proxy-host "Host")
 (proxy-port "Porto")
 (proxy-bad-host "Host de Proxy Incorrecto")

 ;; browser
 (rewind-in-browser-history "Para Trás")
 (forward-in-browser-history "Avançar")
 (home "Início")
 (browser "Navegador")
 (external-browser-choice-title "Navegador Externo") ; title for radio-button set
 (browser-command-line-label "Linha de Comandos:") ; label for radio button that is followed by text boxes
 (choose-browser "Escolher um Navegador")
 (no-browser "Perguntar mais tarde")
 (browser-cmdline-expl-line-1 "(Linha de comando formada pela concatenação do texto inicial, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "e o texto final, sem espaços extra entre eles.)") ; ... line 2. (Anyone need more lines?)
 (install? "Instalar?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Seleccionou um pacote instalável.")
 (do-you-want-to-install-it? "Deseja instalá-lo?")
 (paren-file-size "(Este ficheiro tem ~a bytes)")
 (download-and-install "Tirar && Instalar") ;; button label
 (download "Tirar") ;; button label
 (save-downloaded-file/size "Gravar ficheiro tirado (~a bytes) como") ;; label for get-file dialog
 (save-downloaded-file "Gravar ficheiro tirado como")  ;; label for get-file dialog
 (downloading "A tirar") ;; dialog title
 (downloading-file... "A tirar...")
 (package-was-installed "O pacote foi instalado.")
 (download-was-saved "O ficheiro tirado foi gravado.")

 (install-plt-file-menu-item... "Instalar ficheiro .plt...")
 (install-plt-file-dialog-title "Instalar ficheiro .plt")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Ficheiro")
 (install-plt-filename "Nome do Ficheiro:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Instalar ~a ou abrir ficheiro para edição?")
 (install-plt-file/yes "Instalar")
 (install-plt-file/no "Editar")

 (plt-installer-progress-window-title "Progresso da Instalação") ;; frame title
 (plt-installer-abort-installation "Abortar Instalação") ;; button label
 (plt-installer-aborted "Abortado.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "Sobre o DrRacket")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Gravar este ficheiro como texto?")
 (save-in-drs-format "Gravar este ficheiro no formato específico de DrRacket?")
 (yes "Sim")
 (no "Não")
 
 ;;; preferences
 (preferences "Preferências")
 (error-saving-preferences "Erro a gravar preferências: ~a")
 (error-saving-preferences-title "Erro a gravar preferências")
 (error-reading-preferences "Erro a ler preferências")
 (prefs-file-locked "The preferences file is locked (because the file ~a exists), so your preference change could not be saved. Cancel preference change?")
 (try-again "Tentar de novo") ;; button label
 (prefs-file-still-locked "O ficheiro de preferências continua trancado (porque o ficheiro ~a existe), logo as suas modificações não serão guardadas.")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Avisos")
 (editor-prefs-panel-label "Editar")
 (general-prefs-panel-label "Geral")
 (highlight-parens "Iluminar entre parêntesis correctos")
 (fixup-open-brackets "Ajustar automaticamente parêntesis rectos de abertura")
 (fixup-close-parens "Ajustar automaticamente parêntesis de fecho")
 (flash-paren-match "Iluminar correcção de parêntesis")
 (auto-save-files "Auto-gravar ficheiros")
 (backup-files "Gravar ficheiros como backup")
 (map-delete-to-backspace "Mapar tecla de delete para backspace")
 (verify-exit "Verificar Saída")
 (ask-before-changing-format "Perguntar antes de modificar formato de gravação")
 (wrap-words-in-editor-buffers "Wrap words in editor buffers")
 (show-status-line "Mostrar linha de status")
 (count-columns-from-one "Contar número de colunas a partir de um")
 (display-line-numbers "Mostrar número da linha no buffer; sem offset de caracteres")
 (show-line-and-column-numbers "Mostrar número de linha e coluna") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Mostrar caractéres de offset") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Activar ligações de teclas nos menus")
 (option-as-meta "Tratar tecla option como meta") ;; macos/macos x only
 (reuse-existing-frames "Reutilizar janelas já existentes para abrir novos ficheiros")
 (default-fonts "Fontes por Defeito")
 (paren-match-color "Cor de iluminação dos parêntesis"); in prefs dialog
 (online-coloring-active "Coloração de sintaxe interactiva")
 (open-files-in-tabs "Abrir ficheiros em tabs separados (não em janelas separadas)")
 (show-interactions-on-execute "Abrir janela de interacções automaticamente quando correr um programa")
 (limit-interactions-size "Limitar tamanho das interacções")
 (background-color "Cor de fundo")
 (default-text-color "Texto por defeito") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Por favor escolha uma cor de fundo")

 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") 

 (change-font-button-label "Modificar")
 (fonts "Fontes")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Por favor, escolha uma nova \"~a\" fonte")

 (font-size-slider-label "Tamanho")
 (restart-to-see-font-changes "Reinicie para ver a mudança de fonte")

 (font-prefs-panel-title "Fonte")
 (font-name "Nome da Fonte")
 (font-size "Tamanho da Fonte")
 (set-font "Modificar Fonte...")
 (font-smoothing-label  "Suavização da Fonte")
 (font-smoothing-none "Nenhuma")
 (font-smoothing-some "Alguma")
 (font-smoothing-all "Total")
 (font-smoothing-default "Usar opções por defeito do sistema")
 (select-font-name "Seleccione o nome da fonte")
 (example-text "Texto Exemplo:")
 (only-warn-once "Avisar apenas uma vez quando as definições e as interacções não estiverem sincronizadas")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "À espera do ficheiro de tranca das preferências...")
 (pref-lock-not-gone
  "O ficheiro de tranca:\n\n   ~a\n\nprevine que as preferências de serem guardadas. Certifique-se que nenhum software Racket está a correr e apague este ficheiro.")
 (still-locked-exit-anyway? "As preferências não foram guardadas com sucesso. Pretende continuar?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indentação")
 (indenting-prefs-extra-regexp "Expressões regulares extra")

 (square-bracket-prefs-panel-label "Parêntesis Recto")
  
 ; filled with define, lambda, or begin
 (enter-new-keyword "Insira nova palavra chave do tipo ~a:")
 (x-keyword "Palavra Chave ~a")
 (x-like-keywords "Palavras Chave do tipo ~a")

  ; used in Square bracket panel
 (skip-subexpressions "Número de sub-expressões a saltar")
  
 (expected-a-symbol "esperava um símbolo, recebeu: ~a")
 (already-used-keyword "\"~a\" já é uma palavra chave na lista de indentação")
 (add-keyword "Adicionar")
 (remove-keyword "Remover")
 
 ;;; find/replace
 (find-and-replace "Procurar e Substituir")
 (find "Procurar")
 (replace "Substituir")
 (dock "Esconder")
 (undock "Não Esconder")
 (replace&find-again "Substituir && Procurar de Novo") ;;; need double & to get a single &
 (forward "Avançar")
 (backward "Recuar")
 (hide "Esconder")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Procurar nos Ficheiros...")
 (mfs-string-match/graphics "String match (handles files with graphics)")
 (mfs-regexp-match/no-graphics "Expressão Regular (apenas com ficheiros de texto simples)")
 (mfs-searching... "A Procurar...")
 (mfs-configure-search "Configurar Procura") ;; dialog title
 (mfs-files-section "Ficheiros")   ;; section in config dialog
 (mfs-search-section "Procura") ;; section in config dialog
 (mfs-dir "Directorio")
 (mfs-recur-over-subdirectories "Recursão sobre subdirectorias")
 (mfs-regexp-filename-filter "Expressão Regular para filtro do nome do ficheiro")
 (mfs-search-string "String de Procura")
 (mfs-drscheme-multi-file-search "Procura Multi Ficheiro - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" não é um directorio")
 (mfs-open-file "Abrir Ficheiro")
 (mfs-stop-search "Parar Procura")
 (mfs-case-sensitive-label "Case sensitive")
 (mfs-no-matches-found "No matches found.")
 (mfs-search-interrupted "Procura Abortada.")
 
 ;;; reverting a file
 (are-you-sure-revert
  "De certeza que pretende recuperar este ficheiro? Esta modificação não poderá ser desfeita.")
 (are-you-sure-revert-title
  "Recuperar?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Erro a gravar") ;; title of error message dialog
 (error-saving-file/name "Existiu um erro a gravar ~a.")
 (error-loading "Erro a carregar")
 (error-loading-file/name "Existiu um erro a carregar ~a.")
 (unknown-filename "<< desconhecido >>")

 ;;; finder dialog
 (must-specify-a-filename "Deverá especificar um nome para o ficheiro")
 (file-does-not-exist "O ficheiro \"~a\" não existe.")
 (ask-because-file-exists "O ficheiro \"~a\" já existe. Deseja substituí-lo?")
 (dne-or-cycle "The file \"~a\" contains a nonexistent directory or a cycle.")
 (get-file "Get file")
 (put-file "Put file")
 (full-pathname "Full pathname")
 (show-dot-files "Show files and directories that begin with a dot.")
 (up-directory-button-label "Up directory")
 (add-button-label "Add") ;;; for multi-file selection
 (add-all-button-label "Add all") ;;; for multi-file selection
 (remove-button-label "Remove") ;;; for multi-file selection
 (file-wrong-form "That filename does not have the right form.")
 (select-files "Select files")
 (select-file "Select a file")
 (dir-dne "That directory does not exist.")
 (file-dne "That file does not exist.")
 (empty-filename "The filename must have some letters in it.")
 (that-is-dir-name "That is a directory name.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Ficheiro")
 (edit-menu "Editar")
 (help-menu "Ajuda")
 (windows-menu "Janelas")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Ficheiro")

 (new-info  "Abrir novo ficheiro")
 (new-menu-item "&Novo")
 (new-...-menu-item "&Novo...")

 (open-info "Abrir ficheiro do disco")
 (open-menu-item "&Abrir...")

 (open-recent-info "A lista dos ficheiros abertos mais recentes")
 (open-recent-menu-item "Abrir Recente")
 
 (revert-info "Revert this file to the copy on disk")
 (revert-menu-item "&Revert")

 (save-info "Gravar este ficheiro para o disco")
 (save-menu-item "&Gravar")

 (save-as-info "Prompt for a filename and save this file to disk")
 (save-as-menu-item "Gravar &Como...")

 (print-info "Enviar este ficheiro para a impressora")
 (print-menu-item "&Imprimir...")

 (close-info "Fechar este ficheiro")
 (close-menu-item "&Fechar")

 (quit-info "Fechar todas as janelas")
 (quit-menu-item-windows "&Fechar")
 (quit-menu-item-others "&Sair")
 
 (edit-menu-label "&Editar")
 
 (undo-info "Desfazer a acção mais recente")
 (undo-menu-item "&Desfazer")

 (redo-info "Undo the most recent undo")
 (redo-menu-item "&Refazer")

 (cut-info "Mover os items seleccionados para o clipboard para colar mais tarde")
 (cut-menu-item "Cor&tar")

 (copy-info "Copiar os items seleccionados para o clipboard para colar mais tarde")
 (copy-menu-item "&Copiar")

 (paste-info "Colar os items copiados ou cortados mais recentemente, em vez dos items seleccionados")
 (paste-menu-item "&Colar")

 (clear-info "Apagar os items seleccionados sem afectar o clipboard ou a colagem")
 (clear-menu-item-windows "&Delete")

 (select-all-info "Seleccionar o documento todo")
 (select-all-menu-item "Se&leccionar Tudo")
 
 (find-info "Procurar por uma string")
 (find-menu-item "Procurar...")

 (find-again-info "Procurar pela mesma string que anteriormente")
 (find-again-menu-item "Procurar de novo")
 
 (replace-and-find-again-info "Substituir o texto corrente e procurar pela mesma string que anteriormente")
 (replace-and-find-again-menu-item "Substituir && Procurar de Novo")

 (preferences-info "Configurar as tuas preferências")
 (preferences-menu-item "Preferências...")

 (keybindings-info "Show the currently active keybindings")
 (keybindings-menu-item "Keybindings")
 (keybindings-show-active "Show Active Keybindings")
 (keybindings-frame-title "Keybindings")
 (keybindings-sort-by-name "Sort by Name")
 (keybindings-sort-by-key "Sort by Key")
 (keybindings-add-user-defined-keybindings "Add User-defined Keybindings...")
 (keybindings-menu-remove "Remove ~a")
 (keybindings-choose-user-defined-file "Please choose a file containing keybindings.")

 (user-defined-keybinding-error "Error running keybinding ~a\n\n~a")
 (user-defined-keybinding-malformed-file "The file ~a does not contain a module written in the framework/keybinding-lang language.")  
  
 ;; menu items in the "special" menu
 (insert-text-box-item "Inserir Caixa de Texto")
 (insert-image-item "Inserir Imagem...")
 (insert-comment-box-menu-item-label "Inserir Caixa de Comentário")
 (insert-lambda "Inserir λ")

 (wrap-text-item "Wrap Text")

 (windows-menu-label "&Janelas")
 (bring-frame-to-front "Bring Frame to Front")       ;;; title of dialog
 (bring-frame-to-front... "Bring Frame to Front...") ;;; corresponding title of menu item
 (most-recent-window "Janela Mais Recente")

 (view-menu-label "&Ver")
 (show-overview "Show Program Contour") 
 (hide-overview "Hide Program Contour")
 (show-module-browser "Show Module Browser")
 (hide-module-browser "Hide Module Browser")

 (help-menu-label "&Ajuda")
 (about-info "Credits and details for this application")
 (about-menu-item "Sobre...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Would you like to create a new window, or clear out the current one?")
 (clear-current "Clear Current")
 (new-window "Nova Janela")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Sair")
 (quit "Sair")
 (are-you-sure-exit "Tem a certeza que deseja sair?")
 (are-you-sure-quit "Tem a certeza que deseja sair?")
 
 ;;; autosaving
 (error-autosaving "Error autosaving \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Autosaving is turned off\nuntil the file is saved.")
 (recover-autosave-files-frame-title "Recover Autosaved Files")
 (autosave-details "Detalhes")
 (autosave-recover "Recuperar")
 (autosave-unknown-filename "<<desconhecido>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Ficheiro de auto-gravação:")
  (autosave-original-label: "Ficheiro original:")
  (autosave-autosave-label "Ficheiro de auto-gravação")
  (autosave-original-label "Ficheiro original")
  (autosave-compare-files "Comparar ficheiros de auto-gravação")

  (autosave-show-autosave "Ficheiro de Autogravação") ;; title of a window showing the autosave file

  (autosave-explanation "O DrRacket encontro ficheiros de auto-gravação que poderá conter trabalho ainda não gravado.")

  (autosave-recovered! "Recuperado!") ;; status of an autosave file
  (autosave-deleted "Removido")       ;; status of an autosave file

  (autosave-error-deleting "Erro a remover ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Remover")
  (autosave-delete-title "Remover")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Completo")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Escolha um local para gravar o ficheiro de auto-gravação.")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "The file has been modified since it was last saved. Overwrite the modifications?")
 (overwrite-file-button-label "Overwrite")
 
 (definitions-modified 
  "The definitions text has been modified in the file-system; please save or revert the definitions text.")
 (drscheme-internal-error "DrRacket Internal Error")
 
 ;;; tools
 (invalid-tool-spec "The tool specification in collection ~a's info.rkt file is invalid. Expected either a string or a non-empty list of strings, got: ~e")
 (error-invoking-tool-title "Error invoking tool ~s;~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (tool-tool-urls-same-length
  "expected `tool-urls' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "error loading info.rkt file for ~s")
 (tool-error-phase1 "Error in phase 1 for tool ~s; ~s")
 (tool-error-phase2 "Error in phase 2 for tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< end of buffer >>")
 (sort-by-name "Sort by name")
 (sort-by-position "Sort by position in file")
 (no-definitions-found "<< no definitions found >>")
 (jump-to-defn "Jump to definition of ~a")

 (recent-items-sort-by-age "Sort by Age")
 (recent-items-sort-by-name "Sort by Name")
 
 ;;; view menu
 (hide-definitions-menu-item-label "Esconder &Definições")
 (show-definitions-menu-item-label "Mostrar &Definições")
 (definitions-menu-item-help-string "Mostrar/Esconder janela de definições")
 (show-interactions-menu-item-label "Mostrar &Interacções")
 (hide-interactions-menu-item-label "Esconder &Interacções")
 (interactions-menu-item-help-string "Mostrar/Esconder a janela de interacções")
 (show-toolbar "Mostrar &Barra de Ferramentas")
 (hide-toolbar "Esconder &Barra de Ferramentas")

 ;;; file menu
 (save-definitions-as "Gravar Definições &Como...")
 (save-definitions "Gravar Definições")
 (print-definitions "Imprimir Definições...")
 (about-drscheme "Sobre DrRacket")
 (save-other "Gravar Outro")
 (save-definitions-as-text "Gravar Definições Como Texto...")
 (save-interactions "Gravar Interacções")
 (save-interactions-as "Gravar Interacções Como...")
 (save-interactions-as-text "Gravar Interacções Como Texto...")
 (print-interactions "Imprimir Interacções...")
 (new-tab "Novo Tab")
 (close-tab "Fechar Tab") ;; must not have any &s in it.
 
 ;;; edit-menu
 (split-menu-item-label "&Dividir")
 (collapse-menu-item-label "C&olapsar")
 
 ;;; language menu
 (language-menu-name "&Linguagem")
 
 ;;; scheme-menu
 (scheme-menu-name "Ra&cket")
 (execute-menu-item-label "Correr")
 (execute-menu-item-help-string "Reiniciar o programa na janela de definições")
 (break-menu-item-label "Parar")
 (break-menu-item-help-string "Break the current evaluation")
 (kill-menu-item-label "Matar")
 (kill-menu-item-help-string "Matar a avaliação actual")
 (clear-error-highlight-menu-item-label "Clear Error Highlight")
 (clear-error-highlight-item-help-string "Removes the pink error highlighting")
 (reindent-menu-item-label "&Reindentar")
 (reindent-all-menu-item-label "Reindentar &Tudo")
 (semicolon-comment-out-menu-item-label "&Comentar com Ponto e Virgula")
 (box-comment-out-menu-item-label "&Comentar com uma Caixa")
 (uncomment-menu-item-label "&Descomentar")

 (convert-to-semicolon-comment "Converter para Comentário Ponto e Virgula")
 
 ;;; executables
 (create-executable-menu-item-label "Criar Executável...")
 (create-executable-title "Criar Executável")
 (must-save-before-executable "Tem de gravar o seu programa antes de criar um executável.")
 (save-a-mred-launcher "Save a GRacket Launcher")
 (save-a-mzscheme-launcher "Save a Racket Launcher")
 (save-a-mred-stand-alone-executable "Save a GRacket Stand-alone Executable")
 (save-a-mzscheme-stand-alone-executable "Save a Racket Stand-alone Executable")

 (definitions-not-saved "The definitions window has not been saved. The executable will use the latest saved version of the definitions window. Continue?")
 (launcher "Launcher")
 (stand-alone "Stand-alone")
 (executable-type "Type")
 (executable-base "Base")
 (filename "Filename: ")
 (create "Create")
 ;; "choose-an-executable" changed to "specify-a"
 ;(please-choose-an-executable-filename "Please choose a filename to save the executable.")
 ;; Replaced by generic ~a-must-end-with-~a
 ;(windows-executables-must-end-with-exe
 ; "The filename\n\n  ~a\n\nis illegal. Under Windows, executables must end with .exe.")
 ;(macosx-executables-must-end-with-app
 ; "The filename\n\n  ~a\n\nis illegal. Under MacOS X, an executable must be a directory whose name ends with .app.")
 (warning-directory-will-be-replaced
  "WARNING: the directory:\n\n  ~a\n\nwill be replaced. Proceed?")
 
 (create-servlet "Create Servlet...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Create Servlet does not work with the ~a language.")
  
 ;;; buttons
 (execute-button-label "Correr") 
 (save-button-label "Gravar")
 (break-button-label "Parar")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Procurar no Directorio de Ajuda por \"~a\"")
 (exact-lucky-search-help-desk-for "Exact lucky search in Help Desk for \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "Colapsar expressão-s")
 (expand-sexp "Expandir expressão-s")
 
 ;;; fraction dialog
 (enter-fraction "Inserir Fracção")
 (whole-part "Parte Inteira")
 (numerator "Numerador")
 (denominator "Denominador")
 (invalid-number "Número Inválido: deve ser exacto, real, não-inteiro.")
 (insert-fraction-menu-item-label "Inserir Fracção...")

 ;; number snip popup menu
 (show-decimal-expansion "Ver expansão decimal")
 (show-mixed-fraction-view "Ver como fracção mista")
 (show-improper-fraction-view "Ver como fracção imprópria")
 (show-more-decimal-places "Mostrar mais casas decimais")
 
 ;;; Teachpack messages
 (select-a-teachpack "Seleccionar Pacote de Ensino")
 (clear-teachpack "Apagar Pacote de Ensino ~a")
 (teachpack-error-label "DrRacket - Erro no Pacote de Ensino")
 (teachpack-didnt-load "O ficheiro do Pacote de Ensino ~a não foi carregado correctamente.")
 (add-teachpack-menu-item-label "Adicionar Pacote de Ensino...")
 (clear-all-teachpacks-menu-item-label "Apagar Todos os Pacotes de Ensino")
 (drscheme-teachpack-message-title "Pacote de Ensino do DrRacket")
 (already-added-teachpack "Já adicionou Pacote de Ensino ~a")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Por favor seleccione uma linguagem. Estudantes na maioria dos cursos introdutórios devem escolher a linguagem seleccionada por defeito.")
 (language-dialog-title "Escolher Linguagem")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Estilo de Saída")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "escrever")
  ;(print-printing-style "print-actual") ;; this is possibly incorrect now -- it shows up in the 'output syntax' part of the language dialog
 (sharing-printing-label "Mostrar partilha nos valores")
 (use-pretty-printer-label "Inserir nova linha nos valores printed")
 (input-syntax "Sintaxe de Entrada")
 (dynamic-properties "Propriedades Dinâmicas")
 (output-syntax "Sintaxe de Saída")
 (no-debugging-or-profiling "No debugging or profiling")
 (debugging "Depuração")
 (debugging-and-profiling "Debugging and profiling")
 (test-coverage "Syntactic test suite coverage")
 (show-details-button-label "Mostrar Detalhes")
 (hide-details-button-label "Esconder Detalhes")
 (choose-language-menu-item-label "Escolher Linguagem...")
 (revert-to-language-defaults "Reverter para as Opções por Defeito da Linguagem")
 (fraction-style "Estilo de Fracção")
 (use-mixed-fractions "Fracções Mistas")
 (use-repeating-decimals "Repeating decimals")
 (decimal-notation-for-rationals "Usar notação decimal para racionais")
 (please-select-a-language "Por favor seleccione uma linguagem")

 
 ;;; languages
 (beginning-student "Beginning Student")
 (beginning-one-line-summary "define, cond, structs, constants, and primitives")
 (beginning-student/abbrev "Beginning Student with List Abbreviations")
 (beginning/abbrev-one-line-summary "Beginner, with list style printing in the REPL")
 (intermediate-student "Intermediate Student")
 (intermediate-one-line-summary "Beginner plus lexical scope")
 (intermediate-student/lambda "Intermediate Student with lambda")
 (intermediate/lambda-one-line-summary "Intermediate plus higher-order functions")
 (advanced-student "Advanced Student")
 (advanced-one-line-summary "Intermediate plus lambda and mutation")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Pretty Big")
 (pretty-big-scheme-one-line-summary "Adds syntax and functions from the HtDP languages")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS, with no frills")
 (expander "Expander")
 (expander-one-line-summary "Expands, rather than evaluates, expressions")
 (professional-languages "Professional Languages")
 (teaching-languages "Teaching Languages")
 (experimental-languages "Experimental Languages")
 
 ;(module-language-one-line-summary "Run creates a REPL in the context of the module, including the module's declared language")
  

 ;;; debug language
 (unknown-debug-frame "[desconhecido]")
 (backtrace-window-title "Backtrace - DrRacket")
 (files-interactions "~a's interactions") ;; filled with a filename
 (current-interactions "interacções")
 (current-definitions "definições")
 (mzscheme-w/debug "Textual (MzScheme, inclui R5RS)")
 (mzscheme-one-line-summary "Implementação de Scheme da PLT")
 (mred-w/debug "Gráfico (MrEd, inclui MzScheme)")
 (mred-one-line-summary "Adiciona support GUI ao MzScheme")

 ;; profiling
 (profiling-low-color "Baixo")
 (profiling-high-color "Elevado")
 (profiling-choose-low-color "Por favor seleccione uma cor baixa")
 (profiling-choose-high-color "Por favor seleccione uma cor elevada")
 (profiling "Profiling")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Profiling Color Range") 
 (profiling-scale "Profiling Color Scale")
 (profiling-sqrt "Raiz quadrada")
 (profiling-linear "Linear")
 (profiling-square "Quadrado")
 (profiling-number "Número de Chamadas")
 (profiling-time "Tempo Acumulado")
 (profiling-update "Update Profile")
 (profiling-col-percent-time "% Tempo")
 (profiling-col-function "Função")
 (profiling-col-time-in-msec "Mseg")
 (profiling-col-calls "Chamadas")
 (profiling-show-profile "Show Profile")
 (profiling-hide-profile "Hide Profile")
 (profiling-unknown-src "<< desconhecido >>")
 (profiling-no-information-available "There is no profiling information available. Please be sure that profiling is enabled in your language and you have run your program.")
 (profiling-clear? "Changing the definitions window invalidates the profiling information. Continue?")
 
 ;; test coverage
 (test-coverage-clear? "Changing the definitions window invalidates the test coverage information. Continue?")
 (test-coverage-clear-and-do-not-ask-again "Yes, and don't ask again")
 (test-coverage-ask? "Ask about clearing test coverage")
  
 ;; tracing
 (tracing-enable-tracing "Enable tracing")
 (tracing-show-tracing-window "Show Tracing")
 (tracing-hide-tracing-window "Hide Tracing")
 (tracing-tracing-nothing-to-show "No tracing results are available. Be sure your language supports tracing and it is enabled.")

 ;;; repl stuff
 (evaluation-terminated "Evaluation Terminated")
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
 (last-stack-frame "show the last stack frame")
 (last-stack-frames "show the last ~a stack frames")
 (next-stack-frames "show the next ~a stack frames")
 
 ;;; welcoming message in repl
 (language "Linguagem")
 (custom "costumizado")
 (teachpack "Pacote de Ensino")
 (welcome-to "Benvindo a")
 (version "versão")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Do you want to kill the evaluation?")
 (just-break "Just Break")
 (kill "Kill")
 (kill? "Kill?")

 ;;; version checker

 ;; special menu
 (special-menu "S&pecial")
 
 ;; large semi colon letters
 (insert-large-letters... "Insert Large Letters...")
 (large-semicolon-letters "Large Semicolon Letters")
 (text-to-insert "Text to insert")

 (module-browser-filename-format "Full Filename: ~a (~a lines)")
 (module-browser-root-filename "Root Filename: ~a")
 (module-browser-font-size-gauge-label "Font Size")
 (module-browser-progress-label "Module overview progress")
 (module-browser-adding-file "Adding file: ~a...")
 (module-browser-laying-out-graph-label "Laying out graph")
 (module-browser-open-file-format "Open ~a")
 (module-browser "Module Browser") ;; frame title
 (module-browser... "Module Browser...") ;; menu item title
 (module-browser-error-expanding "Error expanding the program:\n\n~a")
 (module-browser-show-lib-paths "Show files loaded by (lib ..) paths")
 (module-browser-progress "Module Browser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Module Browser: compiling definitions")
 (module-browser-show-lib-paths/short "Follow lib requires") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Refresh") ;; button label in show module browser pane in drscheme window.
; (module-browser-only-in-plt-and-module-langs
;  "The module browser is only available for programs in the PLT languages and in the module language (and only for programs that have modules in them).")
 (module-browser-name-length "Name length")
 (module-browser-name-short "Short")
 (module-browser-name-medium "Medium")
 (module-browser-name-long "Long")
 (module-browser-open-all "Open all files shown here")

 (happy-birthday-matthias "Parabéns, Matthias!")
 (happy-birthday-matthew "Parabéns, Matthew!")
 (happy-birthday-shriram "Parabéns, Shriram!")

 (mrflow-using-default-language-title "Default Language Used")
 (mrflow-using-default-language "The language currently used does not have a type table defined for its primitives. Using R5RS Scheme instead.")
 (mrflow-button-title "Analyze")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-popup-menu-show-type "Show Type")
 (mrflow-popup-menu-hide-type "Hide Type")
 (mrflow-popup-menu-show-errors "Show Errors")
 (mrflow-popup-menu-hide-errors "Hide Errors")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Tack All Arrows")
 (snips-and-arrows-popup-menu-untack-all-arrows "Untack All Arrows")
 (snips-and-arrows-user-action-disallowed-title "User Changes Currently Disallowed")
 (snips-and-arrows-user-action-disallowed "User changes are disallowed in editors that contain tool-inserted snips.  Hide all snips before modifying the content of the editor.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Hide all snips in editor")

 (xml-tool-insert-xml-box "Inserir Caixa XML")
 (xml-tool-insert-scheme-box "Inserir Caixa Racket")
 (xml-tool-insert-scheme-splice-box "Inserir Racket Splice Box")
 (xml-tool-xml-box "Caixa XML")
 (xml-tool-scheme-box "Caixa Racket")
 (xml-tool-scheme-splice-box "Racket Splice Box")
 (xml-tool-switch-to-scheme "Switch to Racket box")
 (xml-tool-switch-to-scheme-splice "Switch to Racket splice box")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Eliminiate whitespace in empty tags")
 (xml-tool-leave-whitespace-alone
  "Leave whitespace alone")
 
 (show-recent-items-window-menu-item "Show Recently Opened Files in Separate Window")
 (show-recent-items-window-label "Recently Opened Files")
 (number-of-open-recent-items "Number of recent items")
 (switch-anyway "Switch File Anyway")

 (stepper-program-has-changed "ATENÇÃO: O programa foi modificado.")
 (stepper-program-window-closed "ATENÇÃO: A janela do programa desapareceu.")

 (stepper-name "Stepper")
 (stepper-language-level-message
  "The language level is set to \"~a\". Currently, the stepper works only for the \"~a\" through the \"~a\" language levels.")
 (stepper-button-label "Step")
 (stepper-previous-application "Application")
 (stepper-previous "Step")
 (stepper-next "Step")
 (stepper-jump-to-beginning "Home")
 (stepper-next-application "Application")
 

 (dialog-back "Back")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "The program in the definitions window is still running. Close anyway?")
  (program-has-open-windows "The program in the definitions window has open windows. Close this window anyway?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Command-line arguments as a vector of strings, in read syntax")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<default collection paths>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Please choose a collection path")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Default collection paths are already present")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection Paths")

  ;; button labels
  (ml-cp-add "Add")
  (ml-cp-add-default "Add Default")
  (ml-cp-remove "Remove")
  (ml-cp-raise "Raise")
  (ml-cp-lower "Lower")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Modo Java")
  (profj-java-mode-color-keyword "Palavra-Chave")
  (profj-java-mode-color-string "string")
  (profj-java-mode-color-literal "literal")
  (profj-java-mode-color-comment "comentário")
  (profj-java-mode-color-error "erro")
  (profj-java-mode-color-identifier "identificador")
  (profj-java-mode-color-default "defeito")

  (profj-insert-java-comment-box "Inserir Caixa de Comentário Java")
  (profj-insert-java-interactions-box "Inserir Caixa de Interacções Java")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Empty test case")
  (test-case-too-many-expressions-error "Too many expressions in a test case.")
  ;; DrRacket window menu items
  (test-case-insert "Insert Test Case")
  (test-case-disable-all "Disable all Test Cases")
  (test-case-enable-all "Enable all Test Cases")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Should be")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Actual")
  (test-case-predicate "Predicate")
  (test-case-should-raise "Should Raise")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Error Message")

  (test-case-menu-title "Test Case")
  (test-case-switch-to-error-box "Switch to Error Test Box")
  (test-case-switch-to-nonerror-box "Switch to Nonerror Test box")
  (test-case-collapse "Collapse Test Case")
  (test-case-show-actual "Show Actual Value")
  (test-case-enable "Enable Test Case")
  (test-case-show-predicate "Show Predicate")
  (test-case-show-error-message "Show Error Message")
  (test-case-convert-to-text "Convert to text")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "Empty interaction")
  (profjBoxes-too-many-expressions-error "Too many expressions in a box")
  (profjBoxes-interactions-label "Interactions")
  (profjBoxes-bad-java-id-error "Malformed Java ID")
  (profjBoxes-examples-label "Examples")
  (profjBoxes-add-new-example-button "Add new example")
  (profjBoxes-type "Type")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Name")
  (profjBoxes-value "Value")
  (profjBoxes-insert-java-examples "Insert Java Examples")
  (profjBoxes-insert-java-interactions "Insert Java Interactions")

  ;; Slideshow
  (slideshow-hide-picts "Show Nested Boxes")
  (slideshow-show-picts "Mostrar Imagens")
  (slideshow-cannot-show-picts "Cannot show picts; run program to cache sizes first")
  (slideshow-insert-pict-box "Insert Pict Box") 

  ;; GUI Tool
  (gui-tool-heading "Ferramenta GUI")
  (gui-tool-before-clicking-message "Antes de clicar no ícone de uma ferramenta, use \"Inserir GUI\" do menu \"Especial\" para inserir um item GUI raíz, ou seleccionar um já inserido GUI.")
  (gui-tool-show-gui-toolbar "Mostrar Barra de Ferramentas GUI")
  (gui-tool-hide-gui-toolbar "Esconder Barra de Ferramentas GUI")
  (gui-tool-insert-gui "Inserir GUI")
  )
