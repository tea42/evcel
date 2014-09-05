#!/bin/bash

set -e


kill_program(){
  program="$1"
  pid_file="$program.pid"
  if [[ -e $pid_file ]]; then
    pid=`cat $pid_file`

    num_tries_left=20
    while $(kill -0 $pid 2> /dev/null ) && (( num_tries_left > 0)); do
      kill $pid
      sleep 1
      (( num_tries_left -- ))
    done

    if $( kill -0 $pid 2> /dev/null ); then
      echo "Failed to kill $program, exiting"
      exit 1
    else
      echo "$program has terminated"
      rm $pid_file
    fi
  else
    echo "No pid file for $program found - nothing to shut down"
  fi
}

kill_program "kafka"
kill_program "zookeeper"

exit 0
