
TRIGGER_FILES = history manifest makefile version.tex \
		schelog.scm schelog.tex

default: 
	@echo Please read the file INSTALL.

%.html: %.tex 
	tex2page $(@:%.html=%)
	while grep -i "rerun: tex2page" $(@:%.html=%.hlog); do \
	tex2page $(@:%.html=%); \
	done

schelog.pdf: schelog.tex
	pdftex $^

schelog.tar: 
	echo tar cf schelog.tar schelog/manifest > .tarscript
	for f in `grep "^[^;]" manifest`; do \
	echo tar uf schelog.tar schelog/$$f >> .tarscript; \
	done
	chmod +x .tarscript
	cd ..; schelog/.tarscript
	mv ../schelog.tar .

schelog.tar.bz2: $(TRIGGER_FILES)
	make schelog.tar
	bzip2 -f schelog.tar

schelog.tar.gz: $(TRIGGER_FILES)
	make schelog.tar
	gzip -f schelog.tar

html: schelog.html 

pdf: schelog.pdf

dist: schelog.tar.bz2

webdist: schelog.tar.gz html

clean:
	@rm -f *~ *.bak
	cd dialects; rm -f *~ *.bak
