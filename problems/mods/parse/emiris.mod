# 
# TITLE : camera displacement between two positions, scaled first frame.
# 
# REFERENCES:
# 
# Ioannis Z. Emiris:
# `Sparse Elimination and Application in Kinematics'
# PhD Thesis, Computer Science, University of California at Berkeley, 1994.
# 
# Ioannis Z. Emiris:
# `A general Solver Based on Sparse Resultants:
#  Numerical Issues and Kinematic Applications',
# INRIA Rapport de Recherche no 3110, January 1997, 29 pages
# Available via anonymous ftp to ftp.inria.fr
# 
# NOTE :
# 
# This system models the displacement of a camera between two positions
# in a static environment, coordinates of matched points in first instance.
# The coordinates of the frames have been scaled, i.e., all components have
# been divided by 1000.
# 




# Domains
var d1 >= -1.0e8, <= 1.0e8;
var d2 >= -1.0e8, <= 1.0e8;
var d3 >= -1.0e8, <= 1.0e8;
var q1 >= -1.0e8, <= 1.0e8;
var q2 >= -1.0e8, <= 1.0e8;
var q3 >= -1.0e8, <= 1.0e8;

# Constants
param a11 := -3.6;
param a12 := 4.1;
param a13 := 2.0;
param a14 := 0.1;
param a15 := 4.1;
param a16 := 1.8;
param a17 := 3.7;
param a18 := -0.2;
param a19 := 2.0;
param a110 := 3.7;
param a111 := -4.0;
param a112 := 0.3;
param a113 := 0.1;
param a114 := -0.2;
param a115 := 0.3;
param a116 := 5.8;

param a21 := -2.140796;
param a22 := -3.998792;
param a23 := 3.715992;
param a24 := -0.2828;
param a25 := -3.998792;
param a26 := -1.575196;
param a27 := -3.998792;
param a28 := 0.0;
param a29 := 3.715992;
param a210 := -3.998792;
param a211 := -2.140796;
param a212 := 0.2828;
param a213 := -0.2828;
param a214 := 0.0;
param a215 := 0.2828;
param a216 := 5.856788;

param a31 := 0.3464;
param a32 := 0.1732;
param a33 := -5.999648;
param a34 := -0.1732;
param a35 := 0.1732;
param a36 := -5.999648;
param a37 := -0.1732;
param a38 := 0.3464;
param a39 := -5.999648;
param a310 := -0.1732;
param a311 := -0.3464;
param a312 := -0.1732;
param a313 := -0.1732;
param a314 := 0.3464;
param a315 := -0.1732;
param a316 := 5.999648;

param a41 := -5701.3;
param a42 := -2.9;
param a43 := 3796.7;
param a44 := -1902.7;
param a45 := -2.9;
param a46 := -5698.7;
param a47 := 1897.3;
param a48 := 3803.3;
param a49 := 3796.7;
param a410 := 1897.3;
param a411 := 5703.1;
param a412 := 0.7;
param a413 := -1902.7;
param a414 := 3803.3;
param a415 := 0.7;
param a416 := 5696.9;

param a51 := -6.8;
param a52 := -3.2;
param a53 := 1.3;
param a54 := 5.1;
param a55 := -3.2;
param a56 := -4.8;
param a57 := -0.7;
param a58 := -7.1;
param a59 := 1.3;
param a510 := -0.7;
param a511 := 9.0;
param a512 := -1.0;
param a513 := 5.1;
param a514 := -7.1;
param a515 := -1.0;
param a516 := 2.6;

subject to
cons1 : d1*q1 + d2*q2 + d3*q3  = 1;

cons2 : d1*(a11*q1 + a12*q2  + a13*q3  + a14) +d2*(a15*q1 + a16*q2  + a17*q3  + a18) +d3*(a19*q1 + a110*q2 + a111*q3 + a112) +a113*q1 + a114*q2 + a115*q3 + a116 = 0;

cons3 : d1*(a21*q1 + a22*q2  + a23*q3  + a24) +d2*(a25*q1 + a26*q2  + a27*q3  + a28) +d3*(a29*q1 + a210*q2 + a211*q3 + a212) +a213*q1 + a214*q2 + a215*q3 + a216 = 0;

cons4 : d1*(a31*q1 + a32*q2  + a33*q3  + a34) +d2*(a35*q1 + a36*q2  + a37*q3  + a38) +d3*(a39*q1 + a310*q2 + a311*q3 + a312) +a313*q1 + a314*q2 + a315*q3 + a316 = 0;

cons5 : d1*(a41*q1 + a42*q2  + a43*q3  + a44) +d2*(a45*q1 + a46*q2  + a47*q3  + a48) +d3*(a49*q1 + a410*q2 + a411*q3 + a412) +a413*q1 + a414*q2 + a415*q3 + a416 = 0;

cons6 : d1*(a51*q1 + a52*q2  + a53*q3  + a54) +d2*(a55*q1 + a56*q2  + a57*q3  + a58) +d3*(a59*q1 + a510*q2 + a511*q3 + a512) +a513*q1 + a514*q2 + a515*q3 + a516 = 0;


#  ORIGINAL SYSTEM:
#  - d1*q1 - d2*q2 - d3*q3 + 1 = 0,
# 
#  - 3.6*d1*q1 + 4.1*d1*q2 + 2.0*d1*q3 + 0.1*d1 + 4.1*d2*q1 + 1.8*d2*q2
#  + 3.7*d2*q3 - 0.2*d2 + 2.0*d3*q1 + 3.7*d3*q2 - 4.0*d3*q3 + 0.3*d3 + 
# 0.1*q1 - 0.2*q2 + 0.3*q3 + 5.8 = 0,
# 
#  - 2.140796*d1*q1 - 3.998792*d1*q2 + 3.715992*d1*q3 - 0.2828*d1 - 
# 3.998792*d2*q1 - 1.575196*d2*q2 - 3.998792*d2*q3 + 3.715992*d3*q1 - 
# 3.998792*d3*q2 - 2.140796*d3*q3 + 0.2828*d3 - 0.2828*q1 + 0.2828*q3 + 5.856788 = 0,
# 
# 0.3464*d1*q1 + 0.1732*d1*q2 - 5.999648*d1*q3 - 0.1732*d1 + 0.1732*d2*
# q1 - 5.999648*d2*q2 - 0.1732*d2*q3 + 0.3464*d2 - 5.999648*d3*q1 - 
# 0.1732*d3*q2 - 0.3464*d3*q3 - 0.1732*d3 - 0.1732*q1 + 0.3464*q2 - 
# 0.1732*q3 + 5.999648 = 0,
# 
#  - 5701.3*d1*q1 - 2.9*d1*q2 + 3796.7*d1*q3 - 1902.7*d1 - 2.9*d2*q1 - 
# 5698.7*d2*q2 + 1897.3*d2*q3 + 3803.3*d2 + 3796.7*d3*q1 + 1897.3*d3*q2
#  + 5703.1*d3*q3 + 0.7*d3 - 1902.7*q1 + 3803.3*q2 + 0.7*q3 + 5696.9 = 0,
# 
#  - 6.8*d1*q1 - 3.2*d1*q2 + 1.3*d1*q3 + 5.1*d1 - 3.2*d2*q1 - 4.8*d2*q2
#  - 0.7*d2*q3 - 7.1*d2 + 1.3*d3*q1 - 0.7*d3*q2 + 9.0*d3*q3 - d3 + 5.1*q1
#  - 7.1*q2 - q3 + 2.6 = 0
# 

#solve;
#display d1, d2, d3, q1, q2, q3;
