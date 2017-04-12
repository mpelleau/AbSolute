# Objective: nonconvex nonlinear
# Constraints: convex quadratic

param n := 50;
param pi := 3.14159;

option randseed '';

param theta {i in 1..n} := 2*pi*Uniform01();
param phi   {i in 1..n} :=   pi*Uniform01();

var x {1..n};
var y {1..n};
var z {1..n};

param best_val_found := 595.6547316;
param eps := 6; 	# = max(1 , 1% x best_val_found) 

subject to separation:
    sum {i in 1..n} sum {j in i+1..n} 
	log( (x[i] - x[j])^2 + (y[i] - y[j])^2 + (z[i] - z[j])^2 ) <= best_val_found + eps;

subject to on_the_ball {i in 1..n}:
    x[i]^2 + y[i]^2 + z[i]^2 <= 1;

let {i in 1..n} x[i] := cos(theta[i])*sin(phi[i]);
let {i in 1..n} y[i] := sin(theta[i])*sin(phi[i]);
let {i in 1..n} z[i] :=               cos(phi[i]);

#option #solver loqo;
#option loqo_options "pred_corr=0 mufactor=0.0 steplen=0.5 \
#	iterlim=400 verbose=2 sigfig=10 inftol=1.0e-6";

#option #solver minos;   ## fails!!!

#fix x[1];
#fix y[1];
#fix z[1];

# #display separation;

#solve;

#display x, y, z;
##display {i in 1..n, j in i+1..n} x[i]*x[j] + y[i]*y[j] + z[i]*z[j];
##display max {i in 1..n} (x[i]^2 + y[i]^2 + z[i]^2);
##display min {i in 1..n} (x[i]^2 + y[i]^2 + z[i]^2);
