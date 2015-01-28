#!/usr/bin/env ruby

require 'logger'
require 'date'

$stdout.sync = true
$stderr.sync = true

$logger = Logger.new($stdout)
$logger.formatter = proc do |severity, datetime, progname, msg|
  "#{datetime}: #{severity} - #{msg}\n"
end


class DateTime
  def is_workday_time?
    wday >=1 && wday <= 5 && hour >= 7 && hour <= 19
  end
end

class Checks

  def Checks.no_new_files_after_tests_have_run
    if `git status --short | wc -l`.to_i > 0
      $logger.error("Some artifacts left after tests have run: #{`git status`}")
      return false
    end
    return true
  end

  def Checks.check_commit_times
    sep = 31.chr
    fmt = ["%h", "%at", "%ct"].join(sep)
    commit_times=`git log --format=#{fmt}`.split("\n")
    commit_times.collect{ |line| line.split(sep)}.each do |hash, author_unix_time, committer_unix_time|
      author_time, committer_time = [author_unix_time, committer_unix_time].collect{ |unix_time|
        DateTime.strptime(unix_time, "%s")
      }
      if author_time.is_workday_time? || committer_time.is_workday_time?
        $logger.error("Commit #{hash} has an invalid time. Author #{author_time}, committer #{committer_time}")
        return false
      end
    end
    true
  end

  def Checks.check_all
    no_new_files_after_tests_have_run && check_commit_times
  end
end

if Checks.check_all
  $logger.info("Check completed successfully")
else
  exit 1
end
