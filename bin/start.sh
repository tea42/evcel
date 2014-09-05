#!/bin/bash
set -e

ZOOKEEPER_LOG=logs/zookeeper.log
KAFKA_LOG=logs/kafka.log

clean_logs(){
  mkdir -p logs
  rm -f $ZOOKEEPER_LOG $KAFKA_LOG
}

clean_test_data(){
  rm -rf kafka-test-data
  rm -rf zookeeper-test-data
}

check_not_already_running(){
  if [[ -e kafka.pid ]] || [[ -e zookeeper.pid ]]; then
    echo "Process id files exist - can't launch"
    exit 1
  fi
}

classpath() {
  ls eventstore/lib_managed/*.jar \
    scala-libs/*.jar \
    | xargs | sed 's/ /:/g'
}

process_options() {
  while true; do
    case "${1-""}" in
      -h | --help ) 
          display_usage; 
          exit 0;;
      -t | --test-mode ) 
          LOGBACK_CONFIG=config/logback-unit-tests.xml;
          ZOOKEEPER_CONFIG=config/zookeeper.test.properties; 
          KAFKA_CONFIG=config/kafka.test.properties; 
          shift;;
      *  ) 
          break;;
    esac
  done
  LOGBACK_CONFIG=${LOGBACK_CONFIG-config/logback.xml}
  KAFKA_CONFIG=${KAFKA_CONFIG-config/kafka.properties}
  ZOOKEEPER_CONFIG=${ZOOKEEPER_CONFIG-config/zookeeper.properties}
}

display_usage() {
cat << EOF

  usage
    start.sh <option>*

  options
    -h, --help
      display this message

    -t, --test-mode
      run using test configurations 
          config/zookeeper.test.properties
          config/kafka.test.properties

EOF
}

launch_zookeeper(){
  echo "Launching zookeeper"
  nohup java -cp "$(classpath)" -Dzookeeper.jmx.log4j.disable=true -Dlogback.configurationFile=$LOGBACK_CONFIG org.apache.zookeeper.server.quorum.QuorumPeerMain $ZOOKEEPER_CONFIG > $ZOOKEEPER_LOG 2>&1 < /dev/null &
  echo $! > zookeeper.pid
}

launch_kafka(){
  echo "Launching kafka"
  nohup java -cp "$(classpath)" -Dlogback.configurationFile=$LOGBACK_CONFIG kafka.Kafka $KAFKA_CONFIG > $KAFKA_LOG 2>&1 < /dev/null &
  echo $! > kafka.pid
  echo "Creating Calendars topic"
  sleep 2
  java -cp "$(classpath)" -Dzookeeper.jmx.log4j.disable=true -Dlogback.configurationFile=$LOGBACK_CONFIG kafka.admin.TopicCommand --zookeeper localhost:2181 --create --topic Calendars --partitions 1 --replication-factor 1
  java -cp "$(classpath)" -Dzookeeper.jmx.log4j.disable=true -Dlogback.configurationFile=$LOGBACK_CONFIG kafka.admin.TopicCommand --zookeeper localhost:2181 --create --topic "CalendarStore2Tests.oneWrite" --partitions 1 --replication-factor 1
}

process_options $*
check_not_already_running
clean_logs
clean_test_data
launch_zookeeper
launch_kafka





