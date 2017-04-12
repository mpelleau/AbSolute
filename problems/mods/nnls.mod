# Objective: convex quadratic
# Constraints: bounds

param m;
param n;

param b {1..m};
param A {1..m, 1..n};

var x {1..n} >= 0;

param best_val_found := 1496210.774;
param eps := 14962; 		# = max(1, 1% x best_val_found)

subject to sum_sqs: sum {i in 1..m} ( b[i] - sum {j in 1..n} A[i,j]*x[j] )^2 <= best_val_found + eps;

data;

#param m := 500;
#param n :=  150;
param m := 1000;
param n :=  300;
#param m := 512;
#param n := 1024;
let {i in 1..m, j in 1..n} A[i,j] := 0;
let {i in 1..m, j in 1..n: Uniform01() < 0.21} A[i,j] := 10*(Uniform01()-1);
param x0 {j in 1..n} := Uniform01();
let {i in 1..m} b[i] := sum {j in 1..n} A[i,j]*x0[j] + 100*(Uniform01()-0.5);

#option loqo_options "timing=1 verbose=2";
#option minos_options "timing=1 superbasics=1000";
#option lancelot_options "timing=1";

#option #solver loqo;		# 16.16
#option #solver minos;		# 17.80
#option #solver lancelot;	#  0.95

#option presolve 1;
#solve;
#display x;
