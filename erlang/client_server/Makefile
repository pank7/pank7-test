all: server1 server2 client.beam client_server.html

client_server.html: client_server.ehtml client.erl server1.c server2.c \
	            gen_server.c
	ehtml2html client_server.ehtml

client.beam: client.erl
	erlc client.erl

server1: server1.c
	gcc -o server1 server1.c

server2: gen_server.o server2.o
	gcc -o server2 server2.o gen_server.o


../client_server.tgz: client_server.html 
	make clean
	cd ..; tar hcvf - client_server | gzip > client_server.tgz

clean:
	rm -f *.o *.beam server1 server2

publish: client_server.html ../client_server.tgz 
	mkdir -p /home/joe/public_html/tutorials/client_server
	cp * /home/joe/public_html/tutorials/client_server
	cp ../client_server.tgz  /home/joe/public_html/tutorials
