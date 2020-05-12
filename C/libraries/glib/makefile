CFLAGS = `pkg-config glib-2.0 --cflags`
CLIBS = `pkg-config glib-2.0 --libs`
SRC = glib_basics.c
OUT = basics

build:
	gcc $(CFLAGS) $(SRC) -o $(OUT) $(CLIBS)

run:
	./$(OUT)

clean:
	rm -f $(OUT)
