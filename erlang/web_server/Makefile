.SUFFIXES: .erl .beam .ear

.erl.beam:
	erlc -W $<

MODS= http_driver tcp_server  web_server 

all: ${MODS:%=%.beam} 

doc: web_server.html 

web_server.html: web_server.ehtml  ${MODS:%=%.erl}
	ehtml2html web_server.ehtml

../web_server.tgz: web_server.html 
	make clean
	cd ..; tar hcvf - web_server | gzip > web_server.tgz
clean:
	rm -rf *.beam one two erl_crash.dump

publish: web_server.html ../web_server.tgz 
	mkdir -p /home/joe/public_html/tutorials/web_server
	cp * /home/joe/public_html/tutorials/web_server
	cp ../web_server.tgz  /home/joe/public_html/tutorials
