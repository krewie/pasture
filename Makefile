ERLC=/usr/local/bin/erlc
ERLCFLAGS=-o
SRCDIR=src
BEAMDIR=./ebin
APP_NAME=simulator
WEBSRC=src/auto
WEBBEAM=./ebin/auto

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ mkdir -p $(WEBBEAM) ;
	@ erlc $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
	@ erlc $(ERLCFLAGS) $(WEBBEAM) $(WEBSRC)/*.erl ;
clean: 
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf $(WEBBEAM) ;
run:
	@erl -pa ebin/ ebin/auto/ -s frame init -s simulator init;