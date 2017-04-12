# Euclidean single facility location problem

# Objective Function:  convex
# Constraint Functions: none

param d := 2;		# dimension
param m := 1000;		# number of facilities

param a {1..m, 1..d};   # coordinates of existing facility
param w {1..m};		# weight associated with each existing facility

var x {1..d};

param best_val_found := 193.9418467;
param eps := 1.939418467; 		# = max(1, 1% x best_val_found)

subject to sumEucl: 
    sum {i in 1..m} w[i]*sqrt( sum {j in 1..d} (x[j] - a[i,j])^2 ) <= best_val_found + eps;

let {i in 1..m, j in 1..d} a[i,j] := Uniform01();
let {i in 1..m} w[i] := Uniform01();

#let {j in 1..d} x[j] := 0.5;

#solve;

#display x;
