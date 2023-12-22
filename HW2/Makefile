parser: lex.yy.c mertali-hw2.tab.c mertali-hw2.tab.h
	gcc -o mertali-hw2 lex.yy.c mertali-hw2.tab.c -lfl

lex.yy.c: mertali-hw2.flx
	flex mertali-hw2.flx 

mertali-hw2.tab.c mertali-hw2.tab.h: mertali-hw2.y
	bison -d mertali-hw2.y

clean:
	rm -f lex.yy.c
	rm -f mertali-hw2.tab.h 
	rm -f mertali-hw2.tab.c 
	rm -f mertali-hw2