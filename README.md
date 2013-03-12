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
    erl> espdy_demo_server:start(6121, false). # start server, non-SSL (SPDY/2 only), OR
    erl> espdy_demo_server_ssl:start(6121).    # start server, SSL (SPDY/3 and SPDY/2)

Please note that you should be running Erlang OTP R16B or later, as that was the
first release that included the TLS Next Protocol Negotiation (NPN)
functionality.

Current Status
--------------

The SSL demo server serves up a hardcoded page, and advertises NPN support for
SPDY/3 and SPDY/2. It can serve either of these versions to Chrome or another
compatible SPDY client.

The non-SSL demo server serves up the same hardcoded page, but only supports
SPDY v2 sessions at the moment. This is because [a bug in Chrome][chrome-bug]
causes it to only use SPDY/2 in non-SSL mode.

There is no flow control, although that got pushed into SPDY/3 anyway.

Probably some other stuff to mention here.

[chrome-bug]: https://code.google.com/p/chromium/issues/detail?id=181598 "SPDY sans SSL in Chrome"

Testing SPDY with Chrome
------------------------

Run Chrome with these flags:

    # Non-SSL mode
    --use-spdy=no-ssl --new-window --user-data-dir=/tmp/foo
    # Or, with NPN/SSL:
    --use-spdy=ssl --new-window --user-data-dir=/tmp/foo

This will open a new Chrome window:

* with a new chrome profile, ignoring your existing settings
* in SPDY-only mode - sites using normal HTTP will not load
* force SPDY to not use SSL, for simpler testing (if you've opted for `no-ssl`)

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

As with other SPDY implementations, the server does not allow frames with
different SPDY versions to be sent on the same session. The server will
terminate the stream or session with the appropriate error message if a frame
version mismatch is encountered.

### Tests

[EUnit tests][parser-tests] are provided to verify the correct behavior of all
binary encoding and decoding. However, there are not yet any tests for the
session or stream management logic.

[parser-tests]: https://github.com/RJ/erlang-spdy/blob/master/test/espdy_parser_test.erl "espdy_parser EUnit Tests"

SPDY Resources
--------------
http://dev.chromium.org/spdy

Chrome supports SPDY v2 and v3 specs currently:

* http://dev.chromium.org/spdy/spdy-protocol/spdy-protocol-draft2
* http://dev.chromium.org/spdy/spdy-protocol/spdy-protocol-draft3
