EXECS = myping

CPPFLAGS = -Wall
LDFLAGS	=
LDLIBS =

TARGET: $(EXECS)

myping.o err.o dropnobody.o: err.h

myping: myping.o in_cksum.o err.o dropnobody.o

.PHONY: clean TARGET
clean:
	rm -f $(EXECS) *.o *~
