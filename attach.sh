#!/bin/bash
# activate erlang r15b03
. /opt/erlang/r15b03/activate
echo "'CTRL + D' to quit erlang repl without quitting crawler"
to_erl pipes/
