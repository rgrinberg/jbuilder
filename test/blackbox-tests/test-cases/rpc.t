Rpc connection fails when dune isn't running

  $ dune rpc
  Error: not running
  [1]

Launch dune with rpc in the background

  $ dune build -w @all &

  $ jobs
  [1]+  Running                 dune build -w @all &

Now try to connect

  $ dune rpc
  Error: not running
  [1]

  $ kill %1

  $ jobs
  [1]+  Running                 dune build -w @all &


  $ jobs
  [1]+  Running                 dune build -w @all &
  $ jobs
  [1]+  Done(1)                 dune build -w @all
  $ jobs
  $ jobs
