
default: package

build: clean
	mkdir -p build/icfp08
	mkdir -p build/bin
	(cd ICFP/src; make jar)
	cp -r submission/bin build
	find build/bin -name .svn -print | xargs rm -rf

package: build test_submit
	cp -r submission/* build/icfp08
	find build/icfp08 -name .svn -print | xargs rm -rf
	cp -r ICFP/src build/icfp08
	find build/icfp08 -name .svn -print | xargs rm -rf
	cp -r build/bin build/icfp08
	find build/icfp08 -name .svn -print | xargs rm -rf
	util/package
    
test_submit:
	echo "Test"
    
clean:
	-rm -rf build
	-rm -f icfp08.tgz
	(cd ICFP/src; make clean)
