# AbSolute

AbSolute is a constraint solver based on abstract domains. It implements the solving method presented in: ["A Constraint Solver Based on Abstract Domains"](https://hal.archives-ouvertes.fr/hal-00785604/file/Pelleau_Mine_Truchet_Benhamou.pdf).

### Solving example:

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
<img src="https://github.com/mpelleau/AbSolute/blob/master/imgs/t2.png" width="400" height="400">

You can see other examples of problems in the **problem** directory

### Build 
A simple **make** will do the job. 
##### warning:
For some reason, having both packages **libapron** and **libapron-dev** installed will make the building of absolute fail.
Therefore, the easiest way to deal with apron is to install it with **and only with** opam : https://opam.ocaml.org/packages/

### Use
```sh 
./solver.opt problem
```

###### options
to display the list of options, type: **./solver.opt -help** or **./solver.opt --help**


### Requirements
- An ANSI C compiler
- OCaml > 4.05 : http://ocaml.org/
- Apron: http://apron.cri.ensmp.fr/library/
- Zarith : https://forge.ocamlcore.org/projects/zarith

### Current
AbSolute is currently still in developpement, and have not been fully tested.
Feel free to contact any member of the developpement team if you want to report a bug or suggest a feature
