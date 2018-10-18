# Contributing to AbSolute

This guide is for anybody who wants to contribute to AbSolute.
Before all, we encourage you to discuss your project with us to be sure your contributions can eventually be integrated.

### Getting ready

We first install the build system [dune](https://github.com/ocaml/dune), the librairies [apron](http://apron.cri.ensmp.fr/library/) and [VPL](https://github.com/VERIMAG-Polyhedra/VPL), and then clone and build AbSolute from the Github repository:

```sh
opam repo add absolute https://raw.githubusercontent.com/mpelleau/AbSolute/master
opam repo add vpl https://raw.githubusercontent.com/VERIMAG-Polyhedra/opam-vpl/master
opam install dune
opam install apron
opam install vpl-core
git clone https://github.com/mpelleau/AbSolute
cd AbSolute
dune build
```

Then, you can test the project using

```sh
dune runtest
```

Additional cool stuff about `dune`:

* From the [manual](https://dune.readthedocs.io/en/latest/): "The dune build and dune runtest commands support a -w (or --watch) flag. When it is passed, dune will perform the action as usual, and then wait for file changes and rebuild (or rerun the tests). This feature requires inotifywait or fswatch to be installed."
* `dune utop src/` starts a REPL with all the absolute libs loaded (need to install `utop` first: `opam install utop`).
* `.merlin` files are generated automatically (which means it is in sync with the building script).

### Creating a new feature

We usually first create a branch or a fork, add and test the new feature, and then create a pull request to merge it into the mainstream repository.

### Publishing

The project is published with [opam](http://opam.ocaml.org/).
If you want to release a new version of AbSolute, here are the steps:

1. Create a new release through the [Github interface](https://github.com/mpelleau/AbSolute/releases/new), for example `v0.3.0` following the conventions of [semantic versionning](https://semver.org/).
2. Update the `src` field in [absolute.opam](https://github.com/mpelleau/AbSolute/blob/master/absolute.opam) with the address of the `.tar.gz` given on the [release page](https://github.com/mpelleau/AbSolute/releases).
3. Perform `cp absolute.opam packages/absolute/absolute.0.3.0/opam` with the same version number as the one used at step (1).
4. `opam admin index` to regenerate `urls.txt` and `index.tar.gz`.
5. `git add packages/absolute/absolute.0.3.0/opam` followed by `git commit` and `git push`.

The new version is now installable with `opam update` followed by `opam install absolute`.
