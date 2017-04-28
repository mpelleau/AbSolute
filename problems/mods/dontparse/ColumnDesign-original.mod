#===============================================================================
# Problem:	Column Design
# Instance:	Full dimensional
# Modeling:	This is the original model.
# Objective:	Find (a,b,e,P,H,L)
# Authors:	Applied Computing and Mechanics Laboratory (IMAC) - EPFL,
#		http://imacsg4.epfl.ch:8080/PGSL/java/column.html
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), LIA - EPFL
# Version:	0.1
# Class:	NOI0-RYN-24-24
# Variables:	
#		Main: 6
#		Auxiliary: 18
# Constraints:	
#		Linear: 8
#		Non-linear: 16
#===============================================================================


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param rho := 78; # (Unit = kN/m2, density of steel)
param fy := 235000.0; # (Unit = kN/m2, yield stress of steel)
param gammaR := 1.1; # (Unit = 0, safety factor, gammaR)
param alpha := 0.21; # (Unit = 0)
param E := 210e6; # (Unit = kN/m2, Youngs modulus of steel)
param lambdaE := 94; # (Unit = 0, lambdaE)
param maxWeight := 10; # (Unit = kN)

param Pi := 3.14159265358979323846;
param ZeroPlus := 0.00000001; # sufficiently small positive number

#-------------------------------------------------------------------------------
# 0 parameters; Enter your values here
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# 6 main variables + 12 bounds
#-------------------------------------------------------------------------------
var a >=0.01, <=2; #unit=meter
var b >=0.01, <=1; #unit=meter
var e >=0.005, <=0.01; # unit=meter
var P >= 100, <=1000; #(Unit = kN)
var H >= 1, <=10; # (Unit = m)
var L >=0.1, <=5; # (Unit = m)


#-------------------------------------------------------------------------------
# 18 auxiliary variables
#-------------------------------------------------------------------------------
var Nd;
var Md;
var A; #Area
var W; #weight
var Iy; #Moment of inertia
var lk; #effective length
var Ncr; #Euler load
var Z; #Iy/y  called w in swiss code
var Mr;
var ry; #radius of gyration
var lambdaK;
var lambdaKBar;
var phi;
var t1;
var k;
var NLambda;
var t2;
var loadByResistance;


#-------------------------------------------------------------------------------
# constraints define the auxiliary variables
#-------------------------------------------------------------------------------
subject to

AC00: Nd = P;
AC01: Md = Nd*(L-b/2);
AC02: A = 2*b*e + 2*(a-2*e)*e; #Area
AC03: W = rho*A*H; #weight
AC04: Iy = (a*b^3-(a-2*e)*(b-2*e)^3)/12; #Moment of inertia
AC05: lk = 2*H; #effective length
AC06: Ncr = Pi^2*E*Iy/lk^2; #Euler load
AC07: Z = Iy/(b/2); #Iy/y  called w in swiss code
AC08: Mr = Z*fy;
# original AC09: ry = sqrt(Iy/A); #radius of gyration
AC09: ry = sqrt(Iy/(A+ZeroPlus)+ZeroPlus); #radius of gyration
# original AC10: lambdaK = 2*H/ry;
AC10: lambdaK = 2*H/(ry+ZeroPlus);
AC11: lambdaKBar = lambdaK/lambdaE;
AC12: phi = 0.5*(1+alpha*(lambdaKBar-0.2)+lambdaKBar^2);
AC13: t1 = max(0, phi^2-lambdaKBar^2);
# original AC14: k = min(1, 1/(phi+sqrt(t1));
AC14: k = min(1, 1/(phi+sqrt(t1+ZeroPlus)));
AC15: NLambda = k*fy*A;
AC16: if abs(1-Nd/Ncr) <= 1e-5 then t2 = 1e-5;
AC17: if abs(1-Nd/Ncr) >= 1e-5 then t2 = 1-Nd/Ncr;
# original AC18: loadByResistance = (Nd/(NLambda/gammaR) + (1/t2)*Md/(Mr/gammaR));
AC18: loadByResistance = (Nd/(NLambda/(gammaR+ZeroPlus)+ZeroPlus) + (1/(t2+ZeroPlus))*Md/(Mr/(gammaR+ZeroPlus)+ZeroPlus));


#-------------------------------------------------------------------------------
# 5 constraints
#-------------------------------------------------------------------------------
subject to

C00 : loadByResistance <= 1 - ZeroPlus; #Strength
C01 : W <= maxWeight - ZeroPlus; #Cost(weight)
C02 : Nd <= Ncr - ZeroPlus; #Euler Buckling
C03 : a >= 4*e;
C04 : b >= 4*e;


#solve;
#display a, b, e, P, H, L;
