# Objective: nonconvex nonlinear
# Constraints: convex quadratic

param n := 20;
param pi := 4*atan(1);

var x {i in 0..n} := 0.5 * cos(2*i*pi/n);
var y {i in 0..n} := 0.5 * sin(2*i*pi/n);

param best_val_found := 9.008137987;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to area: 
     0.5 * sum {i in 0..n-1} (x[i]*y[i+1] - x[i+1]*y[i]) >= best_val_found - eps;

#subject to ordered {i in 1..n-1}: x[i]*y[i+1] - x[i+1]*y[i] >= 0;

subject to xanchored {i in {0,n}}: x[i] = 0.5;
subject to yanchored {i in {0,n}}: y[i] = 0;

subject to diam {i in 0..n-1, j in i+1..n-1}: 
    (x[i] - x[j])^2 + (y[i] - y[j])^2 <= 4;

subject to registration: x[1] = x[n-1];

#let {i in 0..n} x[i] := cos(2*i*pi/n);
#let {i in 0..n} y[i] := sin(2*i*pi/n);
let x[0] := 1; let y[0] := 0; let x[n] := 1; let y[n] := 0;
let {i in 1..n-1} x[i] := cos(2*i*pi/(n-1) - pi/(n-1));
let {i in 1..n-1} y[i] := sin(2*i*pi/(n-1) - pi/(n-1));

#printf {i in 0..n}: "%f %f \n", x[i],y[i];

##display area;

#option loqo_options "timing=1 verbose=2";
#option minos_options "timing=1 outlev=3 superbasics=3000";
#option lancelot_options "timing=1";

#option #solver loqo;	# for n=14, optimal after 0m0.93s
#option #solver minos;   # for n=14, optimal after 0m10.12s.  Wrong answer
#option #solver lancelot;   # for n=14, optimal after 0m4.33s.  

#solve;
#display x,y;
/*
printf {i in 0..n}: "%f %f \n", x[i],y[i] > 'pgon2.out';
##display {i in 0..n-1} sqrt(x[i]^2 + y[i]^2);
##display {i in 0..n-1} sqrt((x[i+1]-x[i])^2 + (y[i+1]-y[i])^2);
##display area, n*sin(2*pi/n)/2, n*sin(2*pi/n)/(2*cos(pi/(2*n))^2);
#display area, if (n mod 2 == 0) 
	      then n*sin(2*pi/n)/2 
	      else n*sin(2*pi/n)/(2*cos(pi/(2*n))^2);

*/
#param m := n-1;
##display m*sin(2*pi/m)/(2*cos(pi/(2*m))^2) + 4*(1-cos(pi/(2*m)))*sin(pi/(2*m));
#
#printf {i in 0..n-1, j in i+1..n-1: 
#    sqrt((x[j]-x[i])^2 + (y[j]-y[i])^2) > 1.9999}: "%3d %3d \n", i, j;

