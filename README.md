# teapot

Let your colleagues know when the tea is ready.

I often brew tea in my office, and there a a few coworkers who
like to taste a cup now and then, but they can't look through walls.

*teapot* serves a simple web page to display the current state of your teapot.
Status updates are done entirely manual via shell scripts.

*teapot* is not an implementation of RFC 2119 / 7168 (HTCPCP-TEA).
Neither does it contain functionality to control computerized
teap pots.

## Usage

Run `stack exec teapot-exe` to start the service on port 8090.

Opening http://<host>:8090 in a browser will display the state of the pot.

To start steeping, you specify the number of cups, the tea, and the steeping time
in minutes in the URL, for example:

    curl -X POST http://<host>:8090/6/Darjeeling%202nd%20Flush/3

The status will display "steeping..." and change to available after 3 minutes.
Use PUT to serve cups, for example:

    curl -X PUT http://<host>:8090/1

This will decrement the available number of cups. The pot will transition to empty
when no tea is left.