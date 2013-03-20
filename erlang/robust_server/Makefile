.SUFFIXES: .erl .beam .ear

.erl.beam:
	ecc -W $<

MODS= bank robust_bank_client bank_client robust_bank_manager \
      bank_manager robust_bank_server bank_server tcp_server

all:  robust_server.html

code: ${MODS:%=%.beam} 

robust_server.html: robust_server.ehtml  ${MODS:%=%.erl}
	ehtml2html robust_server.ehtml

../robust_server.tgz: robust_server.html 
	make clean
	cd ..; tar hcvf - robust_server | gzip > robust_server.tgz
	
clean:
	rm -rf *.beam one two erl_crash.dump

publish: robust_server.html ../robust_server.tgz 
	mkdir -p /home/joe/public_html/tutorials/robust_server
	cp * /home/joe/public_html/tutorials/robust_server
	cp ../robust_server.tgz  /home/joe/public_html/tutorials

	
