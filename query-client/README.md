query-client
============

This subproject is to provide CLI client for query-daemon.

We have two programs: `cmdclient` and `stresstest`
These programs work only if query server is running. To know how to run query server,
please refer to `../query/README.md`. 

To run them, enter nix-shell by 
```
$ nix-shell shell.nix --argstr uphere-nix-overlay (uphere-nix-overlay directory)
```
and then run each commmand like (I am using interpreter here for simplicity)
```
$ runhaskell cmdclient.hs --port (this client's port) -g (this client's global IP) -l (this client's local IP) -s (query server IP) -q (query server port)
```
For stresstest, you replace `cmdclient.hs` with `stresstest.hs`

`(query server IP)` and `(query server port)` are the parameters used when running query server. Note that the IP should be visible to
the client. If server's global IP is not visible, then try to use local IP. 

-----

To hide heart beat, just append `2>/dev/null` in the above command since heartbeating log is via stderr.

-----

Example usage
```
$ runhaskell cmdclient.hs --port 12345 -g 192.168.1.102 -l 192.168.1.102 -s 192.168.1.102 -q 38742 2>/dev/null 
$ runhaskell cmdclient.hs --port 12345 -g 192.168.1.102 -l 192.168.1.102 -s 192.168.1.102 -q 38742
```
