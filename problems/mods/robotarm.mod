# Objective: linear
# Constraints: nonconvex nonlinear

#######################################################################
# A robot arm as depicted in Monika's thesis at IWR.
# Letting the pivot point of the robot be the origin of the coordinate
# system and using spherical coordinates, one of the rotational modes
# corresponds to changes in theta and the other corresponds to changes
# in phi (these are the two angles of spherical coordinates).  Also, the
# length, rho, of the arm can change.  We assume that the arm is a rigid
# bar of length L that protrudes a distance rho from the origin to the
# gripping end and sticks out a distance L-rho in the other direction
# (see the pictures at Monika's web site).   
#
# This model ignores the
# fact that the spherical coordinate reference-frame is a non-interial
# frame and therefore should have terms for coriolis and centrifugal
# forces.
#######################################################################

#######################################################################
# Here is the model:

param pi := 3.14159;

param n;		# number of intervals in which time is discretized into

param L;		# length of arm

param max_u_rho;
param max_u_the;
param max_u_phi;

param rho_0;		# initial position
param the_0;		# initial position (theta)
param phi_0;		# initial position

param rho_n;		# final   position
param the_n;		# final   position
param phi_n;		# final   position

set Np := {0..n};		# discrete times for position
set Nv := {0.5..n-0.5 by 1};	# discrete times for velocity
set Na := {1..n-1};		# discrete times for acceleration

var rho {Np} >=0, <= L; 	# position
var the {Np} >= -pi, <= pi; 	# position
var phi {Np} >=0, <= pi; 	# position

var rho_dot {Nv};	# velocity
var the_dot {Nv};	# velocity
var phi_dot {Nv};	# velocity

var rho_dot2 {Na};	# acceleration
var the_dot2 {Na};	# acceleration
var phi_dot2 {Na};	# acceleration

var I_the {Na} >=0;	# moment of inertia for rotations in theta
var I_phi {Na} >=0;	# moment of inertia for rotations in phi

var T >=0;		# total time

minimize time: T;

subject to rho_dot_def {i in Nv}:n*(rho[i+0.5] - rho[i-0.5]) = T*rho_dot[i];
subject to the_dot_def {i in Nv}:n*(the[i+0.5] - the[i-0.5]) = T*the_dot[i];
subject to phi_dot_def {i in Nv}:n*(phi[i+0.5] - phi[i-0.5]) = T*phi_dot[i];

subject to rho_acc_def {i in Na}: 
	n*(rho_dot[i+0.5] - rho_dot[i-0.5]) = T*rho_dot2[i];
subject to the_acc_def {i in Na}: 
	n*(the_dot[i+0.5] - the_dot[i-0.5]) = T*the_dot2[i];
subject to phi_acc_def {i in Na}: 
	n*(phi_dot[i+0.5] - phi_dot[i-0.5]) = T*phi_dot2[i];

subject to I_the_def {i in Na}: 
	I_the[i] = ((L-rho[i])^3 + rho[i]^3) * sin(phi[i])^2 / 3;
subject to I_phi_def {i in Na}: 
	I_phi[i] = ((L-rho[i])^3 + rho[i]^3) / 3;

subject to rho_ctrl_bnds {i in Na}: 
	-max_u_rho <= L * rho_dot2[i] <= max_u_rho;
subject to the_ctrl_bnds {i in Na}: 
	-max_u_the <= I_the[i] * the_dot2[i] <= max_u_the;
subject to phi_ctrl_bnds {i in Na}: 
	-max_u_phi <= I_phi[i] * phi_dot2[i] <= max_u_phi;

subject to init_rho: rho[0] = rho_0;
subject to init_the: the[0] = the_0;
subject to init_phi: phi[0] = phi_0;

subject to finl_rho: rho[n] = rho_n;
subject to finl_the: the[n] = the_n;
subject to finl_phi: phi[n] = phi_n;

subject to init_rho_dot: rho_dot[0.5] = 0;
subject to init_the_dot: the_dot[0.5] = 0;
subject to init_phi_dot: phi_dot[0.5] = 0;

subject to finl_rho_dot: rho_dot[n-0.5] = 0;
subject to finl_the_dot: the_dot[n-0.5] = 0;
subject to finl_phi_dot: phi_dot[n-0.5] = 0;

#######################################################################
# Here is data corresponding to a specific rotation.

data;

let n := 50;

let L := 5;

let max_u_rho := 1;
let max_u_the := 1;
let max_u_phi := 1;

let rho_0 := 0.9*L;
let the_0 := 0;
let phi_0 := pi/4;

let rho_n := 0.9*L;
let the_n := 2*pi/3;
let phi_n := pi/4;

#######################################################################
# In order to get convergence, it seems to be necessary to initialize
# to something sort of reasonable:

#let {i in Np} rho[i] := ((n-i)*rho_0 + i*rho_n)/n;
#let {i in Np} the[i] := ((n-i)*the_0 + i*the_n)/n;
#let {i in Np} phi[i] := ((n-i)*phi_0 + i*phi_n)/n;

let {i in Np} rho[i] 
    := if (i in 0..n/2) then rho_0 + 2*(rho_n-rho_0)*i^2/n^2
			else rho_n + 2*(rho_0-rho_n)*(i-n)^2/n^2;
let {i in Np} the[i] 
    := if (i in 0..n/2) then the_0 + 2*(the_n-the_0)*i^2/n^2
			else the_n + 2*(the_0-the_n)*(i-n)^2/n^2;
let {i in Np} phi[i] 
    := if (i in 0..n/2) then phi_0 + 2*(phi_n-phi_0)*i^2/n^2
			else phi_n + 2*(phi_0-phi_n)*(i-n)^2/n^2;

#display the;

let T := 1000;
let {i in Nv} rho_dot[i] := n*(rho[i+0.5] - rho[i-0.5])/T;
let {i in Nv} the_dot[i] := n*(the[i+0.5] - the[i-0.5])/T;
let {i in Nv} phi_dot[i] := n*(phi[i+0.5] - phi[i-0.5])/T;

let {i in Na} rho_dot2[i] := n*(rho_dot[i+0.5] - rho_dot[i-0.5])/T ;
let {i in Na} the_dot2[i] := n*(the_dot[i+0.5] - the_dot[i-0.5])/T ;
let {i in Na} phi_dot2[i] := n*(phi_dot[i+0.5] - phi_dot[i-0.5])/T ;

let {i in Na} I_the[i] := ((L-rho[i])^3 + rho[i]^3) * sin(phi[i])^2 / 3;
let {i in Na} I_phi[i] := ((L-rho[i])^3 + rho[i]^3) / 3;

#######################################################################
# And now we #solve it using LOQO.  Note that some parameter settings
# had to be changed from their defaults.

#option #solver loqo;
#option loqo_options "pred_corr=0 mufactor=0.0 steplen=0.9 sigfig=6 \
#	iterlim=200 verbose=2 bndpush=100";
#option #solver minos;
#solve;

option display_eps 0.0001;

#display rho, the, phi;
#display rho_dot, the_dot, phi_dot;
#display rho_dot2, the_dot2, phi_dot2;

