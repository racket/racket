all:
	setup-plt -p -l mztake
	setup-plt -l frtime
	setup-plt -l stepper

plt:
	mred -u make-plt.ss
	mv *.plt ../../web/files/

clean:
	${RM} -r compiled private/compiled
	${RM} *distro*
