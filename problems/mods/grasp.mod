# Objective: linear
# Constraints: convex epsilon-second-order cone
# Feasible set: convex

# This model finds the smallest amount of normal force required
# to "grasp" an object given a set of possible grasping points.
#
# Reference: "Applications of Second-Order Cone Programming",
# M.S. Lobo, L. Vandenberghe, S. Boyd, and H. Lebret, 1998

param pi := 4*atan(1);

param n := 5;	# number of lifting points
param mu := 0.3; 	# coeff. of friction

# p = (p1,p2,p3) is a contact point on a parabolic "nose cone" to be lifted
param p1 {j in 1..n} := 0.3 + cos(2*pi*j/n);
param p2 {j in 1..n} :=       sin(2*pi*j/n);
param p3 {j in 1..n} := p1[j]^2 + p2[j]^2;

# v = (v1,v2,v3) is the unit normal vector at contact point p = (p1,p2,p3)
param grad_norm {j in 1..n} := sqrt( (2*p1[j])^2 + (2*p2[j])^2 + 1);
param v1 {j in 1..n} := -2*p1[j]/grad_norm[j];
param v2 {j in 1..n} := -2*p2[j]/grad_norm[j];
param v3 {j in 1..n} :=       1/grad_norm[j];

# external applied force
param f1_ext := 0;
param f2_ext := 0;
param f3_ext := -1;

# external applied torque
param torq1_ext := 0;
param torq2_ext := 0;
param torq3_ext := 0;

# f = (f1,f2,f3) is the contact force at point p = (p1,p2,p3)
var f1 {j in 1..n};
var f2 {j in 1..n};
var f3 {j in 1..n};

# nf is the normal force at point p = (p1,p2,p3)
var nf {j in 1..n} = v1[j]*f1[j] + v2[j]*f2[j] + v3[j]*f3[j];

# tf = (tf1,tf2,tf3) is the tangential force at point p = (p1,p2,p3)
var tf1 {j in 1..n} = f1[j] - v1[j]*nf[j];
var tf2 {j in 1..n} = f2[j] - v2[j]*nf[j];
var tf3 {j in 1..n} = f3[j] - v3[j]*nf[j];

# torq = (torq1,torq2,torq3) is the torq about (0,0,0) at point p = (p1,p2,p3)
var torq1 {j in 1..n} = p2[j]*f3[j] - f2[j]*p3[j];
var torq2 {j in 1..n} = f1[j]*p3[j] - p1[j]*f3[j];
var torq3 {j in 1..n} = p1[j]*f2[j] - f1[j]*p2[j];

var t;	# an upper bound on the normal contact force

param best_val_found := 0.4378456619;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to pressure: t <= best_val_found + eps;

subject to t_bnds {j in 1..n}: nf[j] <= t;

subject to friction {j in 1..n}: 
	sqrt(tf1[j]^2 + tf2[j]^2 + tf3[j]^2) <= mu*nf[j];

subject to f_balance1: sum {j in 1..n} f1[j] = -f1_ext;
subject to f_balance2: sum {j in 1..n} f2[j] = -f2_ext;
subject to f_balance3: sum {j in 1..n} f3[j] = -f3_ext;

subject to t_balance1: sum {j in 1..n} torq1[j] = -torq1_ext;
subject to t_balance2: sum {j in 1..n} torq2[j] = -torq2_ext;
subject to t_balance3: sum {j in 1..n} torq3[j] = -torq3_ext;

let {j in 1..n} f1[j] := 1;
let {j in 1..n} f2[j] := 1;
let {j in 1..n} f3[j] := 1;

#solve;

#display f1, f2, f3;
