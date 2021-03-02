VERSION=1.2

# override PREFIX, Scheme, and LIB as necessary
PREFIX=/usr

# scheme executable
Scheme=$(PREFIX)/bin/scheme

# target location for stex
LIB=$(PREFIX)/lib/stex$(VERSION)

m := $(shell echo '(machine-type)' | $(Scheme) -q)

Install=./sbin/install

exec = $m/scheme-prep $m/html-prep $m/fixbibtex

all: $(exec)

$m/scheme-prep: src/dsm.ss src/preplib.ss src/script.ss src/scheme-prep.ss
	if [ ! -d $m ] ; then mkdir $m ; fi
	sed -e 's;^#! /usr/bin/scheme --program;#! $(Scheme) --program;' src/scheme-prep.ss > $m/scheme-prep.ss
	echo '(reset-handler abort) (library-directories (quote "src::$m")) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$m/scheme-prep.ss") (compile-whole-program "$m/scheme-prep.wpo" "$m/scheme-prep")' | $(Scheme) -q
	chmod 755 $m/scheme-prep

$m/html-prep: src/dsm.ss src/preplib.ss src/script.ss src/html-prep.ss
	if [ ! -d $m ] ; then mkdir $m ; fi
	sed -e 's;^#! /usr/bin/scheme --program;#! $(Scheme) --program;' src/html-prep.ss > $m/html-prep.ss
	echo '(reset-handler abort) (library-directories (quote "src::$m")) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$m/html-prep.ss") (compile-whole-program "$m/html-prep.wpo" "$m/html-prep")' | $(Scheme) -q
	chmod 755 $m/html-prep

$m/fixbibtex: src/fixbibtex.ss
	-if [ ! -d $m ] ; then mkdir $m ; fi
	sed -e 's;^#! /usr/bin/scheme --program;#! $(Scheme) --program;' src/fixbibtex.ss > $m/fixbibtex.ss
	echo '(reset-handler abort) (library-directories (quote "src::$m")) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$m/fixbibtex.ss") (compile-whole-program "$m/fixbibtex.wpo" "$m/fixbibtex")' | $(Scheme) -q
	chmod 755 $m/fixbibtex

install: $(exec)
	$(Install) -o root -g root -m 755 -d $(LIB)
	$(Install) -o root -g root -m 755 -d $(LIB)/inputs
	$(Install) -o root -g root -m 644 inputs/* $(LIB)/inputs
	$(Install) -o root -g root -m 755 -d $(LIB)/gifs
	$(Install) -o root -g root -m 644 gifs/* $(LIB)/gifs
	$(Install) -o root -g root -m 755 -d $(LIB)/math
	$(Install) -o root -g root -m 644 math/* $(LIB)/math
	$(Install) -o root -g root -m 755 -d $(LIB)/$m
	$(Install) -o root -g root -m 644 $(exec) $(LIB)/$m
	(umask 022; sed -e 's;^LIB=.*;LIB=$(LIB);' Mf-stex > $(LIB)/Mf-stex)
	(umask 022; sed -e 's;include ~/stex/Mf-stex;include $(LIB)/Mf-stex;' Makefile.template > $(LIB)/Makefile.template)

uninstall:
	rm -rf $(LIB)

clean:
	rm -f Make.out

distclean: clean
	rm -rf $m
