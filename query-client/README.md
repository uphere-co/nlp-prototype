query-client
============

This subproject is to provide CLI client for query-daemon.

We have two programs: cmdclient and stresstest

To run them, enter nix-shell by 

```
$ nix-shell shell.nix --argstr uphere-nix-overlay (uphere-nix-overlay directory)
```
and then run each commmand like (I am using interpreter here for simplicity)
```
$ runhaskell cmdclient.hs --port (this client's port) -g (this client's global IP) -l (this client's local IP) -s (query daemon server IP) -q (query daemon server port)
```
For stresstest, you replace `cmdclient.hs` with `stresstest.hs`

To hide heart beat, just append `2>/dev/null` in the above command since heartbeating log is via stderr.
