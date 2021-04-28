# Contributing to AbSolute

This guide is for anybody who wants to contribute to AbSolute.  Before
all, thank you for working on AbSolute and we encourage you to discuss
your project with us to be sure your contributions can eventually be
integrated.

### Getting ready

Install the [apron library](http://apron.cri.ensmp.fr/library/), the [dune](https://github.com/ocaml/dune) build system, the odoc documentation generator and the [picasso](https://github.com/ghilesZ/picasso) library (used for visualisation). You can install all of these by doing:

```sh
opam install dune apron apronext odoc picasso
```

You can then clone and build AbSolute (both the tool and the library) by doing:

```sh
git clone https://github.com/mpelleau/AbSolute
cd AbSolute
make
```

Then, verify everything is working well on an example:

```sh
./_build/default/absolute-solver/absolute.exe problems/booth.abs
```

Or maybe simpler create an executable in the root directory and run it by doing:
```sh
make absolute
./absolute problems/booth.abs
```

### Guidelines
If you want to contribute to the project, note that functionnal
programming is the prefered style. Also we strongly recommend the use
of automating indentation, for example using
[ocamlformat](https://github.com/ocaml-ppx/ocamlformat).

### Monitoring the performances
We recommend you to monitor the memory and time usage of the changes you may introduce in the solver. To do so, you can install [memtrace](https://github.com/janestreet/memtrace) and [memtrace_viewer](https://github.com/janestreet/memtrace_viewer) for the memory, and landmark for the time usage (available via opam):

```sh
opam install memtrace memtrace_viewer landmarks
```
This will allow you to create an executable *memabsolute* from the Makefile, by doing `make memabsolute`. This executable can be used in the same way than absolute but it adds to it a few changes: it initializes a memory trace, adds trace points using landmarks, calls absolute, and then outputs a memory trace in the file `memperf.log` and time usage trace in the file `timeperf.json`.

### Additional cool stuff about `dune`:

* From the [manual](https://dune.readthedocs.io/en/latest/): "The dune build and dune runtest commands support a -w (or --watch) flag. When it is passed, dune will perform the action as usual, and then wait for file changes and rebuild (or rerun the tests). This feature requires inotifywait or fswatch to be installed."
* `dune utop src/` starts a REPL with all the absolute libs loaded (need to install `utop` first: `opam install utop`).
* `.merlin` files are generated automatically (which means it is in sync with the building script).

### Creating a new feature

We usually first create a branch or a fork, add and test the new
feature, and then create a pull request to merge it into the
mainstream repository.  When contributing to AbSolute, please first
discuss the change you wish to make via an issue, then feel free to
fork and submit a PR.

Dont forget to test the project using

```sh
dune runtest
```

### Publishing

The project is published with [opam](http://opam.ocaml.org/).
If you want to release a new version of AbSolute, here are the steps:

1. Create a new release through the [Github interface](https://github.com/mpelleau/AbSolute/releases/new), for example `v0.3.0` following the conventions of [semantic versionning](https://semver.org/).
2. Update the `src` field in [absolute.opam](https://github.com/mpelleau/AbSolute/blob/master/absolute.opam) with the address of the `.tar.gz` given on the [release page](https://github.com/mpelleau/AbSolute/releases).
3. Perform `cp absolute.opam packages/absolute/absolute.0.3.0/opam` with the same version number as the one used at step (1).
4. `opam admin index` to regenerate `urls.txt` and `index.tar.gz`.
5. `git add packages/absolute/absolute.0.3.0/opam` followed by `git commit` and `git push`.

The new version is now installable with `opam update` followed by `opam install absolute`.
