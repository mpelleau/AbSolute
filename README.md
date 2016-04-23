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
<img src="https://github.com/mpelleau/AbSolute/blob/master/imgs/t2.png" width="100" height="100">


You can see other examples of problems in the **problem** directory

### Build 
- The solver: 
```sh 
make
```

### Use
```sh 
./solver.opt problem
```

###### options
  -p v : change the precision to "v". default is 0.001
  
  -v : with visualization
  
  -domain d : change the domain to "d". default is "box"


### Requirements
- An ANSI C compiler (only gcc with ansi option has been tested)
- Ocaml > 4.01
- Apron: http://apron.cri.ensmp.fr/library/

### Current
AbSolute is currently still in developpement, and have not been tested.
Feel free to contact any member of the developpement team if you want to suggest 
