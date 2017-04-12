# Objective: linear
# Constraints: convex quadratic
# Feasible set: convex

################################################################
# This is the linear array beam pattern problem described in:
#     H. Lebret and S. Boyd, "Antenna Array Pattern Synthesis via
#     Convex Optimization", IEEE Trans on Signal Processing, 45, 
#     526-532, 1997.
################################################################

param pi := 4*atan(1);
param rpd := 2*pi/360;

param lambda;			# wavelength
param spacing, default 1;

param n;
set N := {1..n};
param y {N};

set DEGS0 	  := {-20..-4   by 0.5};  # sidelobe
set DEGS1 	  := {-3.5..0   by 0.5};  # min lobe
set DEGS2 	  := {0.5..27.5 by 0.5};  # mainlobe
set DEGS3 	  := {28..60    by 0.5};  # sidelobe
set DEGS023       := DEGS0 union DEGS2 union DEGS3;
set DEGS_EQUALITY := {10, 15, 20, 26};
set DEGS 	  := {-20..60 by 0.5};
param theta0, in DEGS;

param up_bnd {DEGS023};

param w0_real {N};
param w0_imag {N};

param G0_real {theta in DEGS} 
    := sum {k in N} (
	w0_real[k] * cos(2*pi*y[k]*sin(rpd*theta)/lambda)
	-
	w0_imag[k] * sin(2*pi*y[k]*sin(rpd*theta)/lambda)
      )
      ;

param G0_imag {theta in DEGS} 
    := sum {k in N} (
	w0_real[k] * sin(2*pi*y[k]*sin(rpd*theta)/lambda)
	+
	w0_imag[k] * cos(2*pi*y[k]*sin(rpd*theta)/lambda)
      )
      ;

var w_real {N};
var w_imag {N};
var t;

################################################################
# Note: sin(theta) is used instead of cos(theta) because antenna
# is taken to lie on the imaginary axis
################################################################

var G_real {theta in DEGS} 
    = sum {k in N} (
	w_real[k] * cos(2*pi*y[k]*sin(rpd*theta)/lambda)
	-
	w_imag[k] * sin(2*pi*y[k]*sin(rpd*theta)/lambda)
      )
      ;

var G_imag {theta in DEGS} 
    = sum {k in N} (
	w_real[k] * sin(2*pi*y[k]*sin(rpd*theta)/lambda)
	+
	w_imag[k] * cos(2*pi*y[k]*sin(rpd*theta)/lambda)
      )
      ;

param best_val_found := 6.879784886;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to beampattern: 10000*t <= best_val_found + eps;

subject to t_bnds {theta in DEGS1}:
    G_real[theta]^2 + G_imag[theta]^2 <= t;

subject to G_bnds {theta in DEGS023: theta not in DEGS_EQUALITY}:
    (G_real[theta]^2 + G_imag[theta]^2)/up_bnd[theta]^2 <= 1;

subject to normalization_real: G_real[theta0] = G0_real[theta0];
subject to normalization_imag: G_imag[theta0] = G0_imag[theta0];

subject to equality_constrs_real {theta in DEGS_EQUALITY}: 
    G_real[theta] = G0_real[theta];

subject to equality_constrs_imag {theta in DEGS_EQUALITY}: 
    G_imag[theta] = G0_imag[theta];

data;

let lambda := 2;
let spacing := 0.56*lambda;

let theta0 := 6.5;

let {theta in DEGS0 union DEGS3} up_bnd[theta] := 10^(-27.3/20);

let n := 24;
let {k in N} y[k] := (k-1)*spacing;

data;

param w0_real :=
 1 -0.0238 
 2 -0.0894 
 3 -0.148  
 4 -0.1    
 5 -0.0943 
 6 -0.219  
 7 -0.3589 
 8 -0.205  
 9  0.307  
10  0.831 
11  0.952  
12  0.660  
13  0.276  
14  0.0815 
15  0.0356 
16 -0.0442 
17 -0.163  
18 -0.202  
19 -0.148  
20 -0.0885 
21 -0.075  
22 -0.0911 
23 -0.0518 
24 -0.0191 
;


param w0_imag :=
 1  -0.0275
 2  -0.0251
 3   0.039
 4   0.092
 5   0.0643
 6   0.0671
 7   0.0299
 8   0.685
 9   0.86
10   0.556
11  -0.341
12  -0.459
13  -0.524
14  -0.406
15  -0.339
16  -0.332
17  -0.27
18  -0.132
19  -0.0294
20  -0.0197
21  -0.0314
22  -0.0027
23   0.0256
24   0.0303
;

let {theta in DEGS2} 
    up_bnd[theta] := sqrt(G0_real[theta]^2 + G0_imag[theta]^2);

param maxG0 := max {theta in DEGS2} up_bnd[theta];

let {k in N} w0_real[k] := w0_real[k]/maxG0;
let {k in N} w0_imag[k] := w0_imag[k]/maxG0;

let {k in N} w_real[k] := w0_real[k]/2; 
let {k in N} w_imag[k] := w0_imag[k]/2; 
let t := 0;

#let {theta in DEGS1} t_bnds[theta] := -1;
#let {theta in DEGS023: theta not in DEGS_EQUALITY} G_bnds[theta] := -1;

let {theta in DEGS2} up_bnd[theta] := 1.01*up_bnd[theta]/maxG0;

option loqo_options "timing=1 verbose=2 convex sigfig=7";
#option loqo_options "timing=1 bndpush=2 \
#	steplen=0.95 \
#	iterlim=500 sigfig=6 inftol=1.0e-1 verbose=2";
##option loqo_options "timing=1 convex pred_corr=0 mufactor=0.0 steplen=0.3 \
##	iterlim=400 sigfig=6 inftol=1.0e-1 verbose=2";
##option minos_options "timing=1 superbasics=3000";
##option lancelot_options "timing=1";

#option #solver loqo;	  # optimal after 0m52.75s
#option #solver '/home/teal/rvdb/lp/loqo/bkup/3.07/32/loqo';
#option #solver minos;   	  # terminates with "too many major iters" after
#option #solver lancelot;   # fails after 27m57.05s.  Result is infeasible.

option substout 1;

#solve;
#display w_real,w_imag,t;
/*
printf: "\n" > ant.out;

printf {theta in DEGS}: "%5.1f %10.3f \n", 
	theta, 
	 10*log10(abs(G_real[theta]^2 + G_imag[theta]^2))
	>> ant.out;

printf: "\n" >> ant.out;

printf {theta in DEGS0}: "%5.1f %10.3f \n", 
	theta, 20*log10(abs(up_bnd[theta]))
	>> ant.out;

printf {theta in DEGS1}: "%5.1f %10.3f \n", 
	theta, 10*log10(abs(t))
	>> ant.out;

printf {theta in DEGS2}: "%5.1f %10.3f \n", 
	theta, 20*log10(abs(up_bnd[theta]))
	>> ant.out;

printf {theta in DEGS3}: "%5.1f %10.3f \n", 
	theta, 20*log10(abs(up_bnd[theta]))
	>> ant.out;
*/
