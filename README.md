Erlang SPDY 
===========

This is a library application designed to make it easy to add SPDY support to
other erlang apps, specifically webservers like cowboy, misultin and mochiweb.

This app does contain a basic demo server which just returns a "Hello World"
response to a GET / request received over SPDY. You should really use an
existing server, or at least a more robust tcp listener/acceptor method.

To run the demo server:

    $ rebar compile                            # compile everything
    $ erl -pa ebin/ -boot start_sasl -s espdy  # espdy app doesn't run anything
    erl> espdy_demo_server:start(6121, false). # start server, non-ssl

Current Status
--------------

The demo serves up a nice hardcoded page, and can handle a SPDY v2 session
properly to chrome.

No flow control, although that got pushed into SPDY/3 anyway

Probably some other stuff to mention here.

Testing SPDY with Chrome
------------------------

Run Chrome with these flags:

    --use-spdy=no-ssl --new-window --user-data-dir=/tmp/foo

This will open a new chrome window:

* with a new chrome profile, ignoring your existing settings
* in SPDY-only mode - sites using normal HTTP will not load
* force SPDY to not use SSL, for simpler testing

Now go to `http://localhost:6121/` in the new Chrome window.

You can view stats on Chrome's SPDY status at chrome://net-internals/#spdy
**However**, it crashes with --use-spdy=no-ssl unless you have a very recent
version. I'm testing with stunnel and https, and it works fine when Chrome is
started with --use-spdy=ssl


### OS X example
    /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --use-spdy=no-ssl --new-window --user-data-dir=/tmp/foo http://localhost:6121/ 

### Linux example
    chromium-browser --use-spdy=no-ssl --new-window --user-data-dir=/tmp/foo  http://localhost:6121/ 

Erlang SPDY Design Notes
------------------------

This library is useful once you have accepted a socket that will be used for
the SPDY protocol. Once accepted, you start a `espdy_session` process,
providing a callback module. (See the demo server).

`espdy_session` will spawn a `espdy_stream` process for each new stream.

`espdy_stream` uses the callback module to respond to SPDY requests.

Two zlib contexts are maintained for each session. For inflating and deflating.

`espdy_parser` takes care of all the binary-to-record work and visa-versa, and
requires a compression context to use.

SPDY Resources
--------------
http://dev.chromium.org/spdy

Chrome supports SPDY v2 spec currently: 

* http://dev.chromium.org/spdy/spdy-protocol/spdy-protocol-draft2

SPDY v3 is being specced, but is not in the wild yet afaik.
