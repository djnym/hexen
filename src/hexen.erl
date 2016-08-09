-module(hexen).

-export([ main/1 ]).

main([]) ->
  main (["https://repo.hex.pm/"]);
main([RepoUrl]) ->
  application:set_env(hexen, mirror_url, RepoUrl),
  hexen_app:start(),
  receive
    finished -> halt(0)
  after
    infinity -> halt(0)
  end,
  halt(0).
