module Scheduler = Scheduler
module Async_io = Async_io
module Event = Event
module File_watcher = File_watcher
module Shutdown = Shutdown
module Thread_safe_channel = Thread_safe_channel

module For_tests = struct
  module Inotify = Inotify
  module Fsevents = Fsevents
  module Fswatch_win = Fswatch_win
end

module For_benchmarks = struct
  module Thread_pool = Thread_pool
end
