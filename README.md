# AbSolute

AbSolute is a constraint solver based on abstract domains from the theory of abstract interpretation.
It implements the solving method presented in: ["A Constraint Solver Based on Abstract Domains"](https://hal.archives-ouvertes.fr/hal-00785604/file/Pelleau_Mine_Truchet_Benhamou.pdf).

This repository provides code for two packages: the AbSolute solver and the libabsolute library. You can use the later from you OCaml programs, its documentation is available [here](https://mpelleau.github.io/AbSolute/libabsolute/index.html).

AbSolute is still in developpement, and have not been fully tested.
Feel free to fill an [issue](https://github.com/mpelleau/AbSolute/issues) or contact any member of the developpement team if you want to report a bug or suggest a feature.

Contributors: Marie Pelleau, Ghiles Ziat, Alexandre Marechal, Pierre Talbot, Antoine Miné, Charlotte Truchet.
Supported by ANR CoVerif.

# The AbSolute Solver

## Quick introductory example

In AbSolute, you first declare your variables in the `init` section, and then the constraints on these variables in the `constraints` section:

```c
/* simple example with sinus and cosinus */
init{
  real x = [-10;10];
  real y = [-5;5];
}

constraints{
  y < sin(x) + 1;
  y > cos(x) - 1;
}
```

This model is saved into a file that can be fed into the AbSolute solver. You can checkout our Emacs mode for a more convenient use<sup id="a1">[1](#emacs)</sup>.
We also have nice graphics showing how the problem was solved:

<img src="https://github.com/mpelleau/AbSolute/blob/master/imgs/t2.png" width="400" height="400">

You can see other examples of problems in the [problems](https://github.com/mpelleau/AbSolute/tree/master/problems) directory.
Please also consult our [documentation](https://github.com/mpelleau/AbSolute/blob/master/documentation.pdf) for more information.

## Getting Started

The installation process should be easy, if you have any problem, see the Section `Troubleshooting`, fill an issue or email us directly.

### Installation

The easiest way to install AbSolute is through the OCaml package manager [opam](http://opam.ocaml.org/).
You will have to [install it](http://opam.ocaml.org/doc/Install.html) if you do not have it. For example:
```sh
apt-get install opam # on Debian, see opam documentation for other distributions.
opam init --comp 4.09.0 # Initialize ~/.opam with a freshly compiled OCaml 4.09.0
```

The next step is to download and build AbSolute.
If you intent to modify the source code and possibly contribute to the project, jump to the "Developpers" section.
Otherwise, you can install it from `opam`:

```sh
opam repo add absolute https://raw.githubusercontent.com/mpelleau/AbSolute/master
opam install absolute
# Retrieve a model file
wget https://raw.githubusercontent.com/mpelleau/AbSolute/master/problems/booth.abs
# Test the file with your fresh installation
absolute booth.abs
# Display solver options
absolute --help
```

### Developpers

Install the [apron library](http://apron.cri.ensmp.fr/library/), the dune build system, the odoc documentation generator and the picasso library (used for visualisation):

```sh
opam install dune apron apronext odoc picasso
```

You might want to get the developpement version of the apronext and the picasso library (which used to be part of AbSolute) in which case you can install them by doing:

```sh
git clone https://github.com/ghilesZ/apronext
make install -C apronext
git clone https://github.com/ghilesZ/picasso
make install -C picasso 
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

If you want to contribute to the project, note that functionnal programming is the prefered style. Also we strongly recommend the use of [ocamlformat](https://github.com/ocaml-ppx/ocamlformat).

##### Results
Beside the ouptut, AbSolute uses the following return code to describe the results.
- 1 for an internal error
- 2 misuse of options
- 3 the problem is unsatisfiable
- 4 the problem *may* admit solutions but the solver failed to find one
- 5 the problem is satisfiable 

### Troubleshooting

1. For some reason, having both packages `libapron` and `libapron-dev` installed will make the building of AbSolute fail.
Therefore, the easiest way to deal with apron is to install it with and only with opam : https://opam.ocaml.org/packages/

### Citing AbSolute:

```bibtex
@inproceedings{DBLP:conf/vmcai/PelleauMTB13,
  author    = {Marie Pelleau and
               Antoine Min{\'{e}} and
               Charlotte Truchet and
               Fr{\'{e}}d{\'{e}}ric Benhamou},
  title     = {A Constraint Solver Based on Abstract Domains},
  booktitle = {Verification, Model Checking, and Abstract Interpretation, 14th International
               Conference, {VMCAI} 2013, Rome, Italy, January 20-22, 2013. Proceedings},
  pages     = {434--454},
  year      = {2013},
  crossref  = {DBLP:conf/vmcai/2013},
  url       = {https://doi.org/10.1007/978-3-642-35873-9\_26},
  doi       = {10.1007/978-3-642-35873-9\_26},
  timestamp = {Wed, 24 May 2017 08:30:31 +0200},
  biburl    = {https://dblp.org/rec/bib/conf/vmcai/PelleauMTB13},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}
```

##### <b id="emacs">1</b>. A simple emacs mode for editing AbSolute problem description files[↩](#a1):
You can simply add the following to your .emacs:
```lisp
(require 'generic-x) ;; you will need this

(define-generic-mode 'absolute-mode          ;; name of the mode
  '("/*" "*/")                               ;; comments start with '/*' and end with '*/'
  '("init" "constraints" "solutions"
    "real" "int" "in" "notin")               ;; keywords
  '(("\\(?:cos\\|exp\\|s\\(?:in\\|qrt\\)\\)"
     . 'font-lock-function-name-face))       ;; function names
  '("\\.abs$")                               ;; files for which to activate this mode
  nil                                        ;; other actions to perform
  "A mode for AbSolute files"                ;; doc string
  )
```
