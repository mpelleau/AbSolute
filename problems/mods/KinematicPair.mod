#===============================================================================
# Problem:	Kinematic Pair: consisting of a wheel and pawl
# Instance:	Simplified version
# Description:	This is the original model.
# Find:		d, r_1, r_2, r_3, r_4, theta_2, theta_4, x_2, x_4
# Authors:	Simplified from the full version in "Constraint Consistency
#		Techniques for Continuous Domains", PhD Thesis by Djamila Sam.
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), EPFL/I&C/LIA
# Version:	0.1
# Class:	NOI0-ANN-2-3
# Variables:	
#		Main: 2
#		Auxiliary: 0
# Constraints:	
#		Linear: 0
#		Non-linear: 3
#===============================================================================


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param ZeroPlus := 1e-12; # sufficiently small positive number


#-------------------------------------------------------------------------------
# 2 variables
#-------------------------------------------------------------------------------
var x >=-50, <=50;
var y >=0, <=50;


#-------------------------------------------------------------------------------
# 3 constraints
#-------------------------------------------------------------------------------
subject to

C00 : 20 <= (x^2+y^2+ZeroPlus)^0.5 - ZeroPlus;
C01 : (x^2+y^2+ZeroPlus)^0.5 <= 50 - ZeroPlus;
C02 : 12*y/((x-12)^2+y^2)^0.5 <= 10 - ZeroPlus;

#solve;
#display x, y;
