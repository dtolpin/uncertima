zip: clean
	mkdir -p solve/doc solve/examples solve/src solve/src/monitor
	(cd src/algorithm; make distclean)
	(cd src/algorithm && find . -name .svn -prune -o -print \
		| cpio -dumpv ../../solve/src)
	(cd src/algorithm; make distclean)
	(cd src/algorithm && find . -name .svn -prune -o -print \
		| cpio -dumpv ../../solve/src/monitor)
	(cd doc/examples && find . -name .svn -prune -o -print \
		| cpio -dumpv ../../solve/examples)
	cp doc/readme.solve solve/readme.txt
	mv solve/src/solve.man solve/src/solve.ps solve/doc
	zip -r solve-`date +%Y%m%d`.zip solve/
	rm -rf solve

clean:
	rm -rf solve-*.zip solve


