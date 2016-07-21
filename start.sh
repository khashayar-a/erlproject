#!/bin/bash
# activate erlang r15b03
. /opt/erlang/r15b03/activate
# run erlang as a deamon and move output to pipes directory
# user long name (required to cooperate with wombat)
# restrict EPMD ports from 9100 to 9105
run_erl -daemon pipes/ logs "erl -name erlproject -setcookie erlproject -config app -kernel inet_dist_listen_min 9100 inet_dist_listen_max 9110 -pa ebin -pa deps/*/ebin -eval 'application:start(erlproject).'"
