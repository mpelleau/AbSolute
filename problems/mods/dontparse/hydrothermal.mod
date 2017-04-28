# Objective: nonconvex nonlinear
# Constraints: nonconvex nonlinear

param nr;
param nd;
param nt;
param ni;

set Nr := {1 .. nr};
set Nd := {1 .. nd};
set Nt := {1 .. nt};
set Ni := {1 .. ni};

set Ni0 := {0 .. ni};
set Nr2 := {2 .. nr};

param cl {Nt};
param cq {Nt};

param P_lo {Nt};
param P_hi {Nt};

param h_hi {Ni};
param RI   {Ni};
param RD   {Ni};
param L    {Ni};

param sb {Nr};
param sl {Nr};
param sq {Nr};
param sc {Nr};
param v0 {Nr};
param vf {Nr};
param v_max {Nr};
param d_max {Nr};

param g  {Nr, Ni};
param w  {Nr, Ni};

param rI_max{Nt};
param gI_max{j in Nt} := P_hi[j] - P_lo[j] - rI_max[j];
param rD_max{Nt};
param gD_max{j in Nt} := P_hi[j] - P_lo[j] - rD_max[j];

var P  {j in Nt, Ni} >= P_lo[j], <= P_hi[j];
var rI {j in Nt, Ni} >= 0, <= rI_max[j];
var gI {j in Nt, Ni} >= 0, <= gI_max[j];
var rD {j in Nt, Ni} >= 0, <= rD_max[j];
var gD {j in Nt, Ni} >= 0, <= gD_max[j];
var v  {k in Nr, Ni0} >= 0, <= v_max[k];
var h  {Nr, Ni} >= 0;
var d  {k in Nr, i in Ni} >= 0, <= d_max[k];
var htot {i in Ni} >= 0, <= h_hi[i];

#minimize thermal_power:
#    sum {j in Nt, i in Ni} (cl[j]*P[j,i] + cq[j]*P[j,i]^2);

param best_val_found := 190880.6284;
param eps := 1908.806284; 		# = max(1, 1% x best_val_found)

subject to thermal_power:
    sum {j in Nt, i in Ni} (cl[j]*P[j,i] + cq[j]*P[j,i]^2 - cl[j]*P_lo[j] - cq[j]*P_lo[j]^2) <= best_val_found + eps;

subject to h_def {i in Ni, k in Nr}:
    h[k,i] = g[k,i]*(
		sb[k] + 
		sl[k]*(v[k,i-1]+v[k,i])/2 +
		sq[k]*(v[k,i]-v[k,i-1])^2/3 +
		sq[k]*v[k,i-1]*v[k,i] +
		sc[k]*(v[k,i-1]^2 + v[k,i]^2)*(v[k,i-1] + v[k,i])/4
	     )*d[k,i];

subject to htot_def {i in Ni}:
    htot[i] = sum {k in Nr} h[k,i];

subject to water_flow_bal {k in {1}, i in Ni}:
    v[k,i-1] + w[k,i] - d[k,i] - v[k,i] = 0;

subject to water_flow_bal2 {k in Nr2, i in Ni}:
    v[k,i-1] + w[k,i] - d[k,i] + d[k-1,i] - v[k,i] = 0;

subject to hydro_thermal_power_flo_bal {i in Ni}:
    htot[i] + sum {j in Nt} P[j,i] = L[i];

subject to init_vol {k in Nr}:
    v[k,0] = v0[k];

subject to finl_vol {k in Nr}:
    v[k,ni] = vf[k];

subject to Incr {j in Nt, i in Ni}:
    P_hi[j] - P[j,i] = rI[j,i] + gI[j,i];

subject to Decr {j in Nt, i in Ni}:
   -P_lo[j] + P[j,i] = rD[j,i] + gD[j,i];

subject to RI_bnd {i in Ni}:
    h_hi[i] - htot[i] + sum {j in Nt} rI[j,i] >= RI[i];

subject to RD_bnd {i in Ni}:
              htot[i] + sum {j in Nt} rD[j,i] >= RD[i];

data;

param nr := 2;
param nd := 2;
param nt := 1;
param ni := 4;

param cl := 
	1  10
	;
param cq := 
	1  1
	;

param P_lo := 
	1  100
	;
param P_hi := 
	1  250
	;

param h_hi :=
	1  75
	2  75
	3  75
	4  75
	;
param RI   :=
	1  30
	2  30
	3  30
	4  30
	;
param RD   :=
	1  35
	2  35
	3  35
	4  35
	;
param L   :=
	1  280
	2  250
	3  275
	4  250
	;

param sb :=
	1  0.385835e+02
	2  0.114e+02
	;
param sl :=
	1  0.1914066e+00
	2  0.1169998e+00
	;
param sq :=
	1  -0.4302308e-03
	2  -0.1938173e-03
	;
param sc :=
	1  0.4850604e-06
	2  0.3095786e-06
	;
param v0 :=
	1  0.35e+03
	2  0.158e+03
	;
param vf :=
	1  0.35e+03
	2  0.158e+03
	;
param v_max :=
	1  354
	2  160
	;
param d_max :=
	1  5.184
	2  3.456
	;

param g :   1   2   3   4 :=
	1  .2  .2  .2  .2
	2  .2  .2  .2  .2
	;

param w :   1   2   3   4 :=
	1  1.296 1.296 1.296 1.296 
	2  .0864 .0864 .0864 .0864 
	;

param rI_max := 
	1  45
	;
param rD_max := 
	1  65
	;

#let {j in Nt, i in Ni} P[j,i] := 100;
#let {j in Nt, i in Ni} rI[j,i] := 100;
#let {j in Nt, i in Ni} gI[j,i] := 100;
#let {j in Nt, i in Ni} rD[j,i] := 100;
#let {j in Nt, i in Ni} gD[j,i] := 100;
#let {k in Nr, i in Ni0} v[k,i] := 100;
#let {k in Nr, i in Ni} h[k,i] := 100;
#let {k in Nr, i in Ni} d[k,i] := 100;
#let {i in Ni} htot[i] := 100;
#let {i in Ni, k in Nr} h_def[i,k]  := 100;
#let {i in Ni} htot_def[i] := 100;
#
#let {k in {1}, i in Ni} water_flow_bal[k,i] := 100;
#let {k in Nr2, i in Ni} water_flow_bal2[k,i] := 100;
#let {i in Ni} hydro_thermal_power_flo_bal[i] := 100;
#let {k in Nr} init_vol[k] := 100;
#let {k in Nr} finl_vol[k] := 100;
#let {j in Nt, i in Ni} Incr[j,i] := 100;
#let {j in Nt, i in Ni} Decr[j,i] := 100;
#let {i in Ni} RI_bnd[i] := 100;
#let {i in Ni} RD_bnd[i] := 100;

#option #solver "/home/teal/rvdb/lp/loqo/narcis/loqo";
#option #solver "/usr/people/rvdb/lp/loqo/bkup/3.08/32/loqo";
#option loqo_options "convex timing=1 bndpush=1000 verbose=2 pred_corr=0 steplen=0.95";
#option #solver minos;
#option minos_options "timing=1";

#solve;

#display v,d,h;
#display P, rI, rD, gI, gD;
#display htot;/*, h_hi;*/
