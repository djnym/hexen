# Hex.pm mirroring escript

This project provides an escript which does the following

1. copy all packages from hex.pm to a local directory
2. run a webserver which can be used as a HEX_MIRROR/HEX_CDN
3. continue to sync packages from hex.pm as long as it is running

## Building

% rebar3 compile
% sudo cp _build/default/bin/hexen /usr/bin

## Running

% mkdir hex.pm
% cd hex.pm
% hexen

The escript will start a web server on port 31337, so you can then use
HEX_CDN=http://127.0.0.1:31337/ or HEX_MIRROR=http://127.0.0.1:31337/
to pull packages from the local hex.

To stop the server, just control-C.
