#!/usr/bin/make

all: main

main:
	javac `find . -name \*Test.java -o -name \*.java -print`

run: main
	java bigriver.Main localhost 17676

clean:
	-find . -name \*.class -print | xargs -r rm

jar: main
	find . -name \*.class -print | xargs jar -cf ../../build/bin/bot.jar
