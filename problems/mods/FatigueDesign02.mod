#===============================================================================
# Problem:	Fatigue Design
# Instance:	02, no auxiliary variable
# Modeling:	This is a reformulated model from the original one.
# Objective:	Find (L,Z, years) if given (fy,qf)
# Authors:	Applied Computing and Mechanics Laboratory (IMAC) - EPFL,
#		http://imacsg4.epfl.ch:8080/PGSL/java/fatigue.html
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), LIA - EPFL
# Version:	0.1
# Class:	NOI0-RNN-3-2
# Variables:	
#		Main: 3
#		Auxiliary: 0
# Constraints:	
#		Linear: 0
#		Non-linear: 2
#===============================================================================


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param sigma_c := 115000; # (Unit = kN/m2, yield stress of steel)
param gamma := 1.1; # (Unit = 0, safety factor)

param ZeroPlus := 1e-12; # sufficiently small positive number


#-------------------------------------------------------------------------------
# 2 parameters; Enter your values here
#-------------------------------------------------------------------------------
param fy := 460000; # fy >=100000, <=1000000; kN/m2 (yield stress of steel???)
param qf := 80; #qf >=70, <=90;


#-------------------------------------------------------------------------------
# 3 variables
#-------------------------------------------------------------------------------
var L>=10, <=30;
var Z >=0.1, <=10; # Scale up 100 times
var years >=1, <=200;


#-------------------------------------------------------------------------------
# 2 constraints
#-------------------------------------------------------------------------------

subject to
C00 : (qf*(1.44/(sqrt(L)-0.2)+0.82))*L*L/8/(Z/100) <= fy - ZeroPlus; #Strength

C01 : (<<4.0, 7.5, 20.0, 50.0; 0.0, -0.1, -0.1/12.5, -0.1/30, 0>> L + 1.3)*((qf*(1.44/(sqrt(L)-0.2)+0.82))*L*L/8/(Z/100)) <= ((sigma_c*(min((0.05*years)/2, 2.5))^(-1/3))/gamma) - ZeroPlus; #Cost(weight)

#solve;
#display L, Z, years;