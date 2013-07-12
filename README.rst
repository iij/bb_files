========
bb files
========

This is a plugin for `Basho Bench <https://github.com/basho/basho_bench>`_
which provides feature for accessing local files.
Using the plugin, you can use Basho Bench for putting many local files
inside a directory as soon as possible with multiple protocols provided by
Basho Bench's drivers.

How it works
============
This plugin provides two functions: bb_files:keygen/2 and bb_files:valgen/1.
These functions work as key generator and value generator respectively and
return new functions that bind Basho Bench's worker id.

To call the keygen returned function, the worker obtained new filename.
And subsequently call to the valgen returned function, the worker obtained
a binary data object that contains the contents of the filename.
Since the worker id is unique over all workers, returned values never
confuse even if running multiple concurrent workers.

Basho Bench seems to be designed to repeat random tasks as many as possible
during a specific period of time. In contrast, putting local files needs
to be terminated explicitly when all files transferred whether the duration
is remained or not.

Hence, the keygen returned function throws ``{stop, complete}`` tuple when
it reaches end of traverse. It's the only way for terminating workers
gracefully from a key generator.

How to use
==========
First, installing Erlang.

Second, building this plugin::

  $ git clone https://github.com/iij/bb_files.git
  $ cd bb_files
  $ make

Third, building Basho Bench::

  $ cd ..
  $ git clone https://github.com/basho/basho_bench.git
  $ cd basho_bench
  $ make all

Finally, setting up configuration.

There is sample configuration file ``examples/files_to_riak.config``.
You must have to review settings to reflect your environment.

There are 2 important settings to use this plugin.

{code_paths, ["../bb_files"]}:
  "../bb_files" replace with path to actual bb_files directory.

{key_generator, {function, bb_files, keygen, ["../bb_files"]}}:
  "../bb_files" replace with path to a directory you want to put.
  Please note that specifying multiple directory in array form causes
  a crash.

There are ``basho_bench_driver_riakc_pb`` driver related settings.

{riakc_pb_ips, [{127,0,0,1}]}:
  An IP address of riak host.

{riakc_pb_port, 8087}:
  A port of riak protocol buffer interface.

{riakc_pb_bucket, <<"bb_files">>}:
  A bucket name where to put.
