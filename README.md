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
hedistoys 0.4

hedistoys [COMMAND] ... [OPTIONS]
  Experiments with the hedis package

Common flags:
  -? --help     Display help message
  -V --version  Print version information

hedistoys helloworld [OPTIONS]
  A simple "hello world" toy, demonstrating how to use hedis 'get' and 'set',
  and the types returned by them.

hedistoys helloworldtx [OPTIONS]
  Runs the 'get' part of the "hello world" toy in a Redis transaction.

hedistoys get [OPTIONS] KEYNAME
  'get' the value of the provided key.

hedistoys set [OPTIONS] KEYNAME VALUE
  'set' the value of the provided key.

hedistoys sadd [OPTIONS] KEYNAME [VALUE ...]
  Add one or more elements to a set, print the number of elements that were
  added to the set.

hedistoys saddprime [OPTIONS] KEYNAME [VALUE ...]
  Same as the sadd toy, but implemented without 'do' notation.

hedistoys smembers [OPTIONS] KEYNAME
  Get the members of the given set.

hedistoys testandset [OPTIONS] KEYNAME IDKEYNAME
  Atomic test-and-set for getting/creating keys with unique, monotonically
  increasing integer values. The next new key ID is stored in the key with name
  IDKEYNAME.

hedistoys testandsetprime [OPTIONS] KEYNAME IDKEYNAME
  Same as the testandset toy, but implemented independently of a particular
  RedisCtx context.
</pre>

## License

HedisToys is licensed under the [BSD 3-clause
license](http://opensource.org/licenses/bsd-3-clause).

## Contact

For questions, please contact us [here](mailto:src@quixoftic.com).
