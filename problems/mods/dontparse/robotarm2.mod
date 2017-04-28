# Objective: linear
# Constraints: nonconvex nonlinear

#######################################################################
# A robot arm as depicted in Monika's thesis at IWR.
# Letting the pivot point of the robot be the origin of the coordinate
# system and using spherical coordinates, one of the rotational modes
# corresponds to changes in lat and the other corresponds to changes
# in lon (these are the two angles of spherical coordinates).  Also, the
# length, rho, of the arm can change.  We assume that the arm is a rigid
# bar of length 2L that protrudes a distance L+rho from the origin to the
# gripping end and sticks out a distance L-rho in the other direction
# (see the pictures at Monika's web site).  
#
# This model incorporates coriolis and centrifugal forces.
#######################################################################

#######################################################################
# Here is the model:

param pi := 3.14159;
param g := 9.81;	# acceleration due to gravity

param n;		# number of intervals in which time is discretized into

param L;		# half-length of arm
param m_L;		# mass of load
param m_B;		# mass of bar (unloaded arm)
param m_LB := m_L + m_B;		
param I_1_23;		# moment of inertia of bar about bar axis
param I_2_23;		# 
param I_3_23;		#
param I_3_1;		# moment of inertia of base about base axis

param max_F;
param max_T_lon;
param max_T_lat;

param rho_0;		# initial position
param lon_0;		# initial position (theta)
param lat_0;		# initial position

param rho_n;		# final   position
param lon_n;		# final   position
param lat_n;		# final   position

set Np := {0 .. n};		# discrete times for position
set Nv := {0.5 .. n-0.5 by 1};	# discrete times for velocity
set Na := {1 .. n-1};		# discrete times for acceleration

var rho {Np}; # >= -L, <= L; 	# position
var lon {Np}; # >= -pi, <= pi; 	# position
var lat {Np}; # >= -pi/2, <= pi/2; # position

var rho_dot {Nv};	# velocity
var lon_dot {Nv};	# velocity
var lat_dot {Nv};	# velocity

var rho_dot2 {Na};	# acceleration
var lon_dot2 {Na};	# acceleration
var lat_dot2 {Na};	# acceleration

var     F {Na} >= -max_F, <= max_F;
var T_lon {Na} >= -max_T_lon, <= max_T_lon;
var T_lat {Na} >= -max_T_lat, <= max_T_lat;

var T >=0;		# total time

var s {Na}; # >= -L, <= 2*L;

var a {Na}; # >= 0;
var a_1 {Na}; # >= 0;
var a_2 {Na}; # >= 0;

minimize time: T;

subject to rho_dot_def {i in Nv}:n*(rho[i+0.5] - rho[i-0.5]) = T*rho_dot[i];
subject to lon_dot_def {i in Nv}:n*(lon[i+0.5] - lon[i-0.5]) = T*lon_dot[i];
subject to lat_dot_def {i in Nv}:n*(lat[i+0.5] - lat[i-0.5]) = T*lat_dot[i];

subject to rho_acc_def {i in Na}: 
	n*(rho_dot[i+0.5] - rho_dot[i-0.5]) = T*rho_dot2[i];
subject to lon_acc_def {i in Na}: 
	n*(lon_dot[i+0.5] - lon_dot[i-0.5]) = T*lon_dot2[i];
subject to lat_acc_def {i in Na}: 
	n*(lat_dot[i+0.5] - lat_dot[i-0.5]) = T*lat_dot2[i];

subject to s_def {i in Na}: 
	s[i] = rho[i] + m_L*L/m_LB;
subject to a_def {i in Na}: 
	a[i] = I_3_23 - I_2_23 + m_B*rho[i]^2 + m_L*(rho[i] + L)^2;
subject to a_1_def {i in Na}: 
	a_1[i] = I_3_1 + I_2_23 + a[i]*cos(lat[i])^2;
subject to a_2_def {i in Na}: 
	a_2[i] = I_1_23 + m_B*rho[i]^2 + m_L*(rho[i] + L)^2;

subject to F_def {i in Na}: 
	F[i] = m_LB * 
		(
		rho_dot2[i] 
		- s[i]*
		    ( ((lon_dot[i-0.5]+lon_dot[i+0.5])^2)/4 * cos(lat[i])^2 
		    + ((lat_dot[i-0.5]+lat_dot[i+0.5])^2)/4 )
		+ g * sin(lat[i]) 
		);

subject to T_lon_def {i in Na}: 
	T_lon[i] = 
		a_1[i] * lon_dot2[i] 
		- 2 * a[i] * sin(lat[i]) * cos(lat[i]) 
		    * (lon_dot[i-0.5]+lon_dot[i+0.5]) 
		    * (lat_dot[i-0.5]+lat_dot[i+0.5])/4
		+ 2 * m_LB * s[i] * cos(lat[i])^2 
		    * (rho_dot[i-0.5]+rho_dot[i+0.5])
		    * (lon_dot[i-0.5]+lon_dot[i+0.5])/4
		;

subject to T_lat_def {i in Na}: 
	T_lat[i] = 
		a_2[i] * lat_dot2[i] 
		+ a[i] * sin(lat[i]) * cos(lat[i]) 
		       * ((lon_dot[i-0.5]+lon_dot[i+0.5])^2)/4
		+ 2 * m_LB * s[i] 
		    * (rho_dot[i-0.5]+rho_dot[i+0.5])
		    * (lat_dot[i-0.5]+lat_dot[i+0.5])/4
		+ m_LB * g * s[i] * cos(lat[i])
		;

subject to init_rho: rho[0] = rho_0;
subject to init_lon: lon[0] = lon_0;
subject to init_lat: lat[0] = lat_0;

subject to finl_rho: rho[n] = rho_n;
subject to finl_lon: lon[n] = lon_n;
subject to finl_lat: lat[n] = lat_n;

subject to init_rho_dot: rho_dot[0.5] = 0;
subject to init_lon_dot: lon_dot[0.5] = 0;
subject to init_lat_dot: lat_dot[0.5] = 0;

subject to finl_rho_dot: rho_dot[n-0.5] = 0;
subject to finl_lon_dot: lon_dot[n-0.5] = 0;
subject to finl_lat_dot: lat_dot[n-0.5] = 0;

#######################################################################
# Here is data corresponding to a specific rotation.

data;

let n := 8;

let L      :=  0.75;		# half-length of arm
let m_L    :=  0;		# mass of load
let m_B    := 40;		# mass of bar (unloaded arm)
let I_1_23 := 18.5;		# moment of inertia of bar about bar axis
let I_2_23 :=  0.12;		# 
let I_3_23 := 18.5;		#
let I_3_1  :=  0.0;		# moment of inertia of base about base axis

let max_F := 5;
let max_T_lon := 300;
let max_T_lat := 300;

let rho_0 := 0.9*L;
let lon_0 := 0;
let lat_0 := pi/4;

let rho_n := 0.9*L;
let lon_n := 2*pi/3;
let lat_n := pi/4;

#######################################################################
# In order to get convergence, it seems to be necessary to initialize
# to something sort of reasonable:

let {i in Np} rho[i] := ((n-i)*rho_0 + i*rho_n)/n;
let {i in Np} lon[i] := ((n-i)*lon_0 + i*lon_n)/n;
let {i in Np} lat[i] := ((n-i)*lat_0 + i*lat_n)/n;
let T := 10000;
let {i in Nv} rho_dot[i] := n*(rho[i+0.5] - rho[i-0.5])/T;
let {i in Nv} lon_dot[i] := n*(lon[i+0.5] - lon[i-0.5])/T;
let {i in Nv} lat_dot[i] := n*(lat[i+0.5] - lat[i-0.5])/T;

let {i in Na} rho_dot2[i] := n*(rho_dot[i+0.5] - rho_dot[i-0.5])/T ;
let {i in Na} lon_dot2[i] := n*(lon_dot[i+0.5] - lon_dot[i-0.5])/T ;
let {i in Na} lat_dot2[i] := n*(lat_dot[i+0.5] - lat_dot[i-0.5])/T ;

let {i in Na}
	s[i] := rho[i] + m_L*L/m_LB;
let {i in Na} 
	a[i] := I_3_23 - I_2_23 + m_B*rho[i]^2 + m_L*(rho[i] + L)^2;
let {i in Na} 
	a_1[i] := I_3_1 + I_2_23 + a[i]*cos(lat[i])^2;
let {i in Na} 
	a_2[i] := I_1_23 + m_B*rho[i]^2 + m_L*(rho[i] + L)^2;

let {i in Na} 
	F[i] := m_LB * 
		(
		rho_dot2[i] 
		- s[i]*
		    ( ((lon_dot[i-0.5]+lon_dot[i+0.5])^2)/4 * cos(lat[i])^2 
		    + ((lat_dot[i-0.5]+lat_dot[i+0.5])^2)/4 )
		+ g * sin(lat[i]) 
		);

let {i in Na} 
	T_lon[i] := 
		a_1[i] * lon_dot2[i] 
		- 2 * a[i] * sin(lat[i]) * cos(lat[i]) 
		    * (lon_dot[i-0.5]+lon_dot[i+0.5]) 
		    * (lat_dot[i-0.5]+lat_dot[i+0.5])/4
		+ 2 * m_LB * s[i] * cos(lat[i])^2 
		    * (rho_dot[i-0.5]+rho_dot[i+0.5])
		    * (lon_dot[i-0.5]+lon_dot[i+0.5])/4
		;

let {i in Na} 
	T_lat[i] := 
		a_2[i] * lat_dot2[i] 
		+ a[i] * sin(lat[i]) * cos(lat[i]) 
		       * ((lon_dot[i-0.5]+lon_dot[i+0.5])^2)/4
		+ 2 * m_LB * s[i] 
		    * (rho_dot[i-0.5]+rho_dot[i+0.5])
		    * (lat_dot[i-0.5]+lat_dot[i+0.5])/4
		+ m_LB * g * s[i] * cos(lat[i])
		;

#######################################################################
# And now we #solve it using LOQO.  Note that some parameter settings
# had to be changed from their defaults.

#option #solver loqo;
#option loqo_options "pred_corr=0 mufactor=0 steplen=0.2 sigfig=6 \
#	iterlim=200 verbose=2";
#option #solver minos;

#option substout 1;
#solve;

option display_eps 0.0001;

#display rho, lon, lat;
#display rho_dot, lon_dot, lat_dot;
#display rho_dot2, lon_dot2, lat_dot2;
#display F, T_lon, T_lat;
#display T,a,a_1,a_2;
