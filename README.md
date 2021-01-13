# AbSolute

AbSolute is a constraint solver based on abstract domains from the theory of abstract interpretation.
It implements the solving method presented in: ["A Constraint Solver Based on Abstract Domains"](https://hal.archives-ouvertes.fr/hal-00785604/file/Pelleau_Mine_Truchet_Benhamou.pdf).

This repository provides code for two packages: the AbSolute solver and the libabsolute library. You can use the later from you OCaml programs.

AbSolute is still in developpement, and have not been fully tested.
Feel free to fill an [issue](https://github.com/mpelleau/AbSolute/issues) or contact any member of the developpement team if you want to report a bug or suggest a feature.

Contributors: Marie Pelleau, Ghiles Ziat, Alexandre Marechal, Pierre Talbot, Antoine Miné, Charlotte Truchet.
Supported by ANR CoVerif.

## Quick introductory example

In AbSolute, you first declare your variables in the `init` section, and then the constraints on these variables in the `constraints` section:

```c
/* simple example with sinus and cosinus */
init{
  real x = [-10;10];
  real y = [-5;5];
}

constraints{
  y < (sin x) + 1;
  y > (cos x) - 1;
}
```

This model is saved into a file that can be fed into the AbSolute solver. You can checkout our Emacs mode for a more convenient use<sup id="a1">[1](#emacs)</sup>.
We also have nice graphics showing how the problem was solved:

<img src="https://github.com/mpelleau/AbSolute/blob/master/imgs/t2.png" width="400" height="400">

You can see other examples of problems in the [problems](https://github.com/mpelleau/AbSolute/tree/master/problems) directory.
Please also consult our [documentation](https://github.com/mpelleau/AbSolute/blob/master/documentation.pdf) for more information.

## Getting Started

The installation process should be easy, if you have any problem, see the Section `Troubleshooting`, fill an issue or email us directly.

### Requirements

The following is a list of the dependencies to build AbSolute; note that we explain in the next section how to install `OCaml` and `Apron` if you do not have them already.

- An ANSI C compiler
- OCaml >= 4.03 : http://ocaml.org/
- Apron: http://apron.cri.ensmp.fr/library/
- dune

### Installation

We install OCaml and AbSolute through the OCaml package manager [opam](http://opam.ocaml.org/).
You will have to [install it](http://opam.ocaml.org/doc/Install.html) if you do not have it. For example:
```sh
apt-get install opam # on Debian, see opam documentation for other distributions.
opam init --comp 4.06.1 # Initialize ~/.opam with a freshly compiled OCaml 4.06.1
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

Install the [apron library](http://apron.cri.ensmp.fr/library/) dependency, and then clone and build AbSolute from the Github repository:

```sh
opam install dune apron apronext picasso
git clone https://github.com/mpelleau/AbSolute
cd AbSolute
make
```

Then, verify everything is working well on an example:

```sh
./absolute problems/booth.abs
```

##### Coding style
Functionnal is the prefered style. Also we strongly recommend the use of [ocamlformat](https://github.com/ocaml-ppx/ocamlformat).

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
    "real" "int")                            ;; keywords
  '(("\\(?:cos\\|exp\\|s\\(?:in\\|qrt\\)\\)"
     . 'font-lock-function-name-face))       ;; function names
  '("\\.abs$")                               ;; files for which to activate this mode
  nil                                        ;; other actions to perform
  "A mode for AbSolute files"                ;; doc string
  )
```
