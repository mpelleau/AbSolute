#===============================================================================
# Problem:	Fatigue Design
# Instance:	Full dimensional
# Modeling:	This is the original model.
# Objective:	Find (L,qf,Z,years,fy)
# Authors:	Applied Computing and Mechanics Laboratory (IMAC) - EPFL,
#		http://imacsg4.epfl.ch:8080/PGSL/java/fatigue.html
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), LIA - EPFL
# Version:	0.1
# Class:	NOI0-RYN-15-15
# Variables:	
#		Main: 5
#		Auxiliary: 10
# Constraints:	
#		Linear: 5
#		Non-linear: 10
#===============================================================================


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param sigma_c := 115000; # (Unit = kN/m2, yield stress of steel)
param gamma := 1.1; # (Unit = 0, safety factor)

param ZeroPlus := 1e-12; # sufficiently small positive number


#-------------------------------------------------------------------------------
# 0 parameters; Enter your values here
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# 5 main variables
#-------------------------------------------------------------------------------
var L>=10, <=30;
var qf >=70, <=90;
var Z >=0.001, <=0.1;
var years >=1, <=200;
var fy >=100000, <=1000000;

#-------------------------------------------------------------------------------
# 5 auxiliary variables
#-------------------------------------------------------------------------------
var alpha;
var phi;
var qr;
var sigma;
var delta_sigma;
var sigma_e;
var load;
var cycles;
var sigma_r; # = sigma_c*(min(cycles/2, 2.5))^-1/3; #?????????
var resistance;


#-------------------------------------------------------------------------------
# constraints define the auxiliary variables
#-------------------------------------------------------------------------------
subject to

AC00:	if 1.44/(L^0.5 - 0.2) + 0.82 <= 1 then phi = 1;
AC01:	if 1.44/(L^0.5 - 0.2) + 0.82 >= 1.67 then phi = 1.67;
AC02:	if 1 <= 1.44/(L^0.5 - 0.2) + 0.82 <= 1.67 then phi = 1.44/(L^0.5 - 0.2) + 0.82;
AC03:	if (cycles >= 5 - ZeroPlus) then sigma_r = sigma_c*0.74;
AC04:	if (cycles <= 5) then sigma_r = (cycles/2)^-0.33333*sigma_c;
AC05:	alpha = <<4.0, 7.5, 20.0, 50.0; 0.0, -0.1, -0.1/12.5, -0.1/30, 0>> L + 1.3;
AC06:	qr = qf*phi;
AC07:	sigma = qr*L^2/8/Z;
AC08:	delta_sigma = sigma;
AC09:	sigma_e = alpha*delta_sigma;
AC10:	load = sigma_e;
AC11:	cycles = 0.05*years;
AC12:	resistance = sigma_r/gamma;


#-------------------------------------------------------------------------------
# 2 constraints
#-------------------------------------------------------------------------------

C00 :	sigma <= fy - ZeroPlus; #Strength
C01 :	load <= resistance - ZeroPlus; #Cost(weight)


#solve;
#display L, qf, Z, years, fy;