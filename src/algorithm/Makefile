# $Id: Makefile 811 2010-01-19 22:52:48Z tolpin $

PACKAGES=-package str,unix,xmlrpc-light,nethttpd-for-netcgi2 -linkpkg

OCC=ocamlfind ocamlc
OCFLAGS=-g $(PACKAGES)

XCC=ocamlfind ocamlopt
#XCFLAGS=-noassert -unsafe -p $(PACKAGES)
XCFLAGS=$(PACKAGES)

OFILES=ndist.cmo \
       conf.cmo debug.cmo stat.cmo \
       pdefinition.cmo plexer.cmo pparser.cmo pops.cmo \
       loc.cmo expression.cmo markov.cmo model.cmo space.cmo \
       object.cmo hypothesis.cmo remote.cmo \
       log.cmo \
       algo.cmo montecarlo.cmo planhead.cmo metahead.cmo \
       myopic.cmo blinkered.cmo \
       finder.cmo error.cmo init.cmo \
       mk_s.cmo	

XFILES=ndist.cmx \
       conf.cmx debug.cmx stat.cmx \
       pdefinition.cmx plexer.cmx pparser.cmx pops.cmx \
       loc.cmx expression.cmx markov.cmx model.cmx space.cmx \
       object.cmx hypothesis.cmx remote.cmx \
       log.cmx \
       algo.cmx montecarlo.cmx planhead.cmx metahead.cmx \
       myopic.cmx blinkered.cmx \
       finder.cmx error.cmx init.cmx \
       mk_s.cmx	

.SUFFIXES: .mll .mly .ml .mli .cmi .cmo .cmx .1 .man .ps

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc $<

.mly.mli:
	ocamlyacc $<

.mli.cmi:
	$(OCC) $(OCFLAGS) -c $<

.ml.cmo:
	$(OCC) $(OCFLAGS) -c $<

.ml.cmx:
	$(XCC) $(XCFLAGS) -c $<

.1.man:
	groff -man -Tascii $< > $@

.1.ps:
	groff -man -Tps $< > $@

all: solve solvex simd simxd solve.ps solve.man

solve: $(OFILES) solve.cmo
	$(OCC) $(OCFLAGS) -o $@ $^

solvex: $(XFILES) solve.cmx
	$(XCC) $(XCFLAGS) -o $@ $^

simd: $(OFILES) simd.cmo
	$(OCC) $(OCFLAGS) -o $@ $^

simxd: $(XFILES) simd.cmx
	$(XCC) $(XCFLAGS) -o $@ $^

clean: 
	rm -f *.cm? *.o *.jo pparser.ml plexer.ml plexer.mli

distclean: clean solve.man solve.ps
	rm -f ./#*# ./*~ ./.*.sw? solve solvex solve.jar

# dependencies
algo.cmo: stat.cmi space.cmi pops.cmi plan.cmi pdefinition.cmo object.cmi \
    ndist.cmi model.cmi loc.cmi expression.cmi debug.cmi conf.cmi algo.cmi 
algo.cmx: stat.cmx space.cmx pops.cmx plan.cmi pdefinition.cmx object.cmx \
    ndist.cmx model.cmx loc.cmx expression.cmx debug.cmx conf.cmx algo.cmi 
all.cmo: 
all.cmx: 
blinkered.cmo: plan.cmi myopic.cmi algo.cmi blinkered.cmi 
blinkered.cmx: plan.cmi myopic.cmx algo.cmx blinkered.cmi 
conf.cmo: conf.cmi 
conf.cmx: conf.cmi 
debug.cmo: conf.cmi debug.cmi 
debug.cmx: conf.cmx debug.cmi 
error.cmo: pdefinition.cmo object.cmi error.cmi 
error.cmx: pdefinition.cmx object.cmx error.cmi 
expression.cmo: pops.cmi pdefinition.cmo expression.cmi 
expression.cmx: pops.cmx pdefinition.cmx expression.cmi 
finder.cmo: pdefinition.cmo object.cmi model.cmi log.cmi debug.cmi algo.cmi \
    finder.cmi 
finder.cmx: pdefinition.cmx object.cmx model.cmx log.cmx debug.cmx algo.cmx \
    finder.cmi 
hypothesis.cmo: object.cmi model.cmi hypothesis.cmi 
hypothesis.cmx: object.cmx model.cmx hypothesis.cmi 
init.cmo: error.cmi init.cmi 
init.cmx: error.cmx init.cmi 
loc.cmo: loc.cmi 
loc.cmx: loc.cmi 
log.cmo: plan.cmi pdefinition.cmo model.cmi loc.cmi log.cmi 
log.cmx: plan.cmi pdefinition.cmx model.cmx loc.cmx log.cmi 
markov.cmo: pops.cmi ndist.cmi debug.cmi conf.cmi markov.cmi 
markov.cmx: pops.cmx ndist.cmx debug.cmx conf.cmx markov.cmi 
metahead.cmo: stat.cmi space.cmi pops.cmi pdefinition.cmo object.cmi \
    ndist.cmi model.cmi loc.cmi debug.cmi conf.cmi algo.cmi metahead.cmi 
metahead.cmx: stat.cmx space.cmx pops.cmx pdefinition.cmx object.cmx \
    ndist.cmx model.cmx loc.cmx debug.cmx conf.cmx algo.cmx metahead.cmi 
mk_s.cmo: pops.cmi pdefinition.cmo ndist.cmi 
mk_s.cmx: pops.cmx pdefinition.cmx ndist.cmx 
model.cmo: pops.cmi pdefinition.cmo ndist.cmi markov.cmi loc.cmi conf.cmi \
    model.cmi 
model.cmx: pops.cmx pdefinition.cmx ndist.cmx markov.cmx loc.cmx conf.cmx \
    model.cmi 
montecarlo.cmo: pops.cmi pdefinition.cmo object.cmi model.cmi loc.cmi \
    debug.cmi algo.cmi montecarlo.cmi 
montecarlo.cmx: pops.cmx pdefinition.cmx object.cmx model.cmx loc.cmx \
    debug.cmx algo.cmx montecarlo.cmi 
myopic.cmo: pops.cmi plan.cmi pdefinition.cmo object.cmi algo.cmi myopic.cmi 
myopic.cmx: pops.cmx plan.cmi pdefinition.cmx object.cmx algo.cmx myopic.cmi 
ndist.cmo: ndist.cmi 
ndist.cmx: ndist.cmi 
object.cmo: pops.cmi pdefinition.cmo ndist.cmi loc.cmi debug.cmi object.cmi 
object.cmx: pops.cmx pdefinition.cmx ndist.cmx loc.cmx debug.cmx object.cmi 
pdefinition.cmo: ndist.cmi 
pdefinition.cmx: ndist.cmx 
planhead.cmo: stat.cmi pops.cmi pdefinition.cmo object.cmi model.cmi loc.cmi \
    debug.cmi algo.cmi planhead.cmi 
planhead.cmx: stat.cmx pops.cmx pdefinition.cmx object.cmx model.cmx loc.cmx \
    debug.cmx algo.cmx planhead.cmi 
plexer.cmo: pparser.cmo pdefinition.cmo 
plexer.cmx: pparser.cmx pdefinition.cmx 
pops.cmo: pparser.cmo plexer.cmo pdefinition.cmo pops.cmi 
pops.cmx: pparser.cmx plexer.cmx pdefinition.cmx pops.cmi 
pparser.cmo: pdefinition.cmo ndist.cmi pparser.cmi
pparser.cmx: pdefinition.cmx ndist.cmx pparser.cmi
remote.cmo: pdefinition.cmo object.cmi ndist.cmi remote.cmi 
remote.cmx: pdefinition.cmx object.cmx ndist.cmx remote.cmi 
simd.cmo: remote.cmi pops.cmi pdefinition.cmo object.cmi init.cmi error.cmi \
    debug.cmi conf.cmi simd.cmi 
simd.cmx: remote.cmx pops.cmx pdefinition.cmx object.cmx init.cmx error.cmx \
    debug.cmx conf.cmx simd.cmi 
solve.cmo: stat.cmi space.cmi remote.cmi pops.cmi planhead.cmi \
    pdefinition.cmo object.cmi myopic.cmi montecarlo.cmi model.cmi \
    metahead.cmi log.cmi init.cmi hypothesis.cmi finder.cmi error.cmi \
    debug.cmi conf.cmi blinkered.cmi algo.cmi solve.cmi 
solve.cmx: stat.cmx space.cmx remote.cmx pops.cmx planhead.cmx \
    pdefinition.cmx object.cmx myopic.cmx montecarlo.cmx model.cmx \
    metahead.cmx log.cmx init.cmx hypothesis.cmx finder.cmx error.cmx \
    debug.cmx conf.cmx blinkered.cmx algo.cmx solve.cmi 
space.cmo: pdefinition.cmo loc.cmi space.cmi 
space.cmx: pdefinition.cmx loc.cmx space.cmi 
stat.cmo: stat.cmi 
stat.cmx: stat.cmi 
algo.cmi: space.cmi plan.cmi pdefinition.cmo object.cmi model.cmi loc.cmi 
blinkered.cmi: plan.cmi algo.cmi 
conf.cmi: 
debug.cmi: 
error.cmi: pdefinition.cmo object.cmi 
expression.cmi: pdefinition.cmo loc.cmi 
finder.cmi: space.cmi pdefinition.cmo object.cmi model.cmi log.cmi loc.cmi \
    algo.cmi 
hypothesis.cmi: pdefinition.cmo model.cmi 
init.cmi: 
loc.cmi: 
log.cmi: plan.cmi pdefinition.cmo model.cmi loc.cmi 
markov.cmi: ndist.cmi 
metahead.cmi: plan.cmi algo.cmi 
model.cmi: pdefinition.cmo ndist.cmi loc.cmi 
montecarlo.cmi: plan.cmi algo.cmi 
myopic.cmi: plan.cmi pdefinition.cmo object.cmi algo.cmi 
ndist.cmi: 
object.cmi: pdefinition.cmo ndist.cmi loc.cmi 
planhead.cmi: plan.cmi algo.cmi 
plan.cmi: loc.cmi 
pops.cmi: pdefinition.cmo 
remote.cmi: pdefinition.cmo object.cmi 
simd.cmi: 
solve.cmi: 
space.cmi: pdefinition.cmo loc.cmi 
stat.cmi: 
