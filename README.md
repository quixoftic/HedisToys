# HedisToys

HedisToys is a simple executable for experimenting with the Haskell
[hedis package](http://hackage.haskell.org/package/hedis). I wrote it
as a kind of playground for experimenting with hedis, and with
[Redis](http://redis.io/), in general.

HedisToys itself doesn't do anything useful, but given the dearth of
examples on how to use hedis, I hope you find HedisToys useful for
understanding how to use the package.

## Caveats

HedisToys assumes you have a Redis database to experiment with. Don't
run it against a production server!

HedisToys does not clean up after itself. I recommend that you start a
localhost instance of Redis in the foreground, with logging to stdout,
in order to monitor Redis while the toys run. I've included a sample
`redis.conf` file that I use for my testing. To use it, run the
following command:

<pre>redis-server redis.conf</pre>

Then simply stop the server with `CTRL-C` when you're
finished.

The included config disables database dumps to disk, so that each time
you start redis using this config, you'll have a fresh database (and
no garbage left on your disk).

## Running

<pre>
hedistoys 0.1

hedistoys [COMMAND] ... [OPTIONS]
  Experiments with the hedis package

Common flags:
  -? --help     Display help message
  -V --version  Print version information

hedistoys toy1 [OPTIONS]
  A simple "hello world" toy, demonstrating how to use hedis 'get' and 'set',
  and the types returned by them.

hedistoys toy2 [OPTIONS]
  Runs the 'get' part of the "hello world" toy in a Redis transaction.

hedistoys toy3 [OPTIONS]
  'get' a non-existent key.

hedistoys toy4 [OPTIONS]
  Add a single element to a set, print the number of elements in the set. Note:
  not idempotent.

hedistoys toy5 [OPTIONS]
  Same as toy 4, but without 'do' notation.

hedistoys toy6 [OPTIONS]
  Atomic test-and-set for getting/creating new keys.
</pre>

## License

HedisToys is licensed under the [BSD 3-clause
license](http://opensource.org/licenses/bsd-3-clause).

## Contact

For questions, please contact us [here](mailto:src@quixoftic.com).
