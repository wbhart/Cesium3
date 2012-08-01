INC=-I/home/wbhart/gc/include
LIB=-L/home/wbhart/gc/lib
OBJS=input.o ast.o exception.o parser.o

calc: calc.c $(OBJS)
	gcc -O2 -o calc calc.c $(INC) $(OBJS) $(LIB) -lgc

ast.o: ast.c ast.h
	gcc -c -O2 -o ast.o ast.c $(INC)

exception.o: exception.c exception.h
	gcc -c -O2 -o exception.o exception.c $(INC)

parser.o: parser.c parser.h
	gcc -c -O2 -o parser.o parser.c $(INC)

input.o: input.c input.h
	gcc -c -O2 -o input.o input.c $(INC)

