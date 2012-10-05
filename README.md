# HedisTest

HedisTest is a simple executable for testing the Haskell [hedis
package](http://hackage.haskell.org/package/hedis). I wrote it as a
kind of playground for experimenting with hedis, and with
[Redis](http://redis.io/), in general.

HedisTest itself doesn't do anything useful, but given the dearth of
examples on how to use hedis, I hope you find HedisTest useful for
understanding how to use the package.

## Caveats

HedisTest assumes you have a Redis database to experiment with. Don't
run it against a production server!

All of the tests are ephemeral (i.e., they don't depend on results of
previous runs). HedisTest does not clean up after itself. I recommend
that you start a localhost instance of Redis in the foreground, with
logging to stdout, in order to monitor Redis while the tests run. I've
included a sample `redis.conf` file that I use for my testing. To use
it, run the following command:

<pre>redis-server redis.conf</pre>

Then simply stop the server with `CTRL-C` when you're
finished.

The included config disables database dumps to disk, so that each time
you start redis using this config, you'll have a fresh database (and
no garbage left on your disk).

## Running

<pre>
hedistest 0.1

hedistest [COMMAND] ... [OPTIONS]
  Test the hedis package

Common flags:
  -? --help     Display help message
  -V --version  Print version information

hedistest test1 [OPTIONS]
  A simple "hello world" test, demonstrating how to use hedis 'get' and 'set',
  and the types returned by them.

hedistest test2 [OPTIONS]
  Runs the 'get' part of the "hello world" test in a Redis transaction.
</pre>

## License

HedisTest is licensed under the [BSD 3-clause
license](http://opensource.org/licenses/bsd-3-clause).

## Contact

For questions, please contact us [here](mailto:src@quixoftic.com).
