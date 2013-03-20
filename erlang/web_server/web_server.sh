#!/bin/sh
 
##
## usage web_server.sh {start|stop|debug}
##
 
##   PA   = path to the web server
##   PORT  = port to run as
 
PA=$HOME/tutorials/dev/web_server
PORT=4501
ERL=/usr/local/bin/erl
HOSTNAME=`hostname`
export HEART_COMMAND="$PA/web_server.sh start"

case $1 in

  start)
    $ERL -boot start_sasl -sname webserver001 -pa $PA \
         -heart -detached -s web_server start $PORT 
    echo  "Starting Webserver"
    ;;
 
  debug)
    $ERL -sname  webserver001 -pa $PA -s web_server start $PORT
    ;;
 
  stop)
    $ERL -noshell -sname webserver_stopper -pa $PA \
           -s web_server stop webserver001@$HOSTNAME
    echo "Stopping webserver"
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
                    
