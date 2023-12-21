########################################################################
### Makefile
###

PYTHON=python2
PYDOC=pydoc2

PDFLATEX=pdflatex
ACROREAD="/cygdrive/c/Program Files/Adobe/Acrobat 7.0/Reader/AcroRd32"

CFLAGS=-Wall -O -fomit-frame-pointer -fexpensive-optimizations -fstrength-reduce -finline-functions $(CXTRA)

MAS=msp430-as
MAFLAGS=-mmcu=msp430x1611 --gstabs

MCC=msp430-gcc
MCFLAGS=-DSOFTMSP -mmcu=msp430x1611 -mdisable-hwmul -Wall -O -g $(MCXTRA)
#MCFLAGS=-DSOFTMSP -mmcu=msp430x1611 -mdisable-hwmul -Wall -O $(MCXTRA)

### hwmul support
#MCFLAGS=-DSOFTMSP -mmcu=msp430x1611 -Wall -O $(MCXTRA)

### libm support
MLDFLAGS=-lm

OBJCOPY=msp430-objcopy
OBJDUMP=msp430-objdump

all:	python pydocs soft430.exe sjref.hex sjrefa.hex sjrefb.hex

bundle:
	BASE=msp430-16bit-simulator-`TZ=UTC date +%Y%m%d%H%M%SZ`; \
	git archive --format=tar --prefix=$$BASE/ HEAD > $$BASE.tar; \
	xz $$BASE.tar

python:	
	$(PYTHON) setup.py build

pydocs:
	PYTHONPATH=`(cd build/lib.* && pwd)`; export PYTHONPATH; \
	cd doc && ( \
	$(PYDOC) -w _soft430; \
	$(PYDOC) -w _py430; \
	$(PYDOC) -w py430 )

soft430.exe:	soft430.c soft430.h
	$(CC) $(CFLAGS) -o $@ $<

sjrefa.exe:	sjrefa.o sjcsmac.o
	$(MCC) $(MCFLAGS) -o $@ $+

sjrefb.exe:	sjrefb.o sjcsmac.o
	$(MCC) $(MCFLAGS) -o $@ $+

%.exe:	%.o
	$(MCC) $(MCFLAGS) -o $@ $< $(MLDFLAGS)

%.o:	%.asm
	$(MAS) $(MAFLAGS) -o $@ -a=`basename $@|sed -e 's,\.o$$,.lst,'` $<

%.o:	%.c
	$(MCC) $(MCFLAGS) -fverbose-asm -save-temps "-Wa,-a=`basename $@|sed -e 's,\.o$$,.lst,'`" -c -o $@ $<

%.sym:	%.exe
	$(OBJDUMP) -t $< | sort > $@

%.sym:	%.o
	$(OBJDUMP) -t $< > $@

%.hex:	%.exe
	$(OBJCOPY) -O ihex $< $@

clean:	
	/bin/rm -f *~ *.pyc *.[isoa] *.exe *.hex *.lst *.sym *.stackdump
	/bin/rm -f *.aux *.log
	/bin/rm -rf build msp430-16bit-simulator-[0-9][0-9]*Z.tar.xz

### EOF Makefile

