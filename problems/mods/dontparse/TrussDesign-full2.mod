#===============================================================================
# Problem:	Truss Design
# Instance:	Full dimentional, no auxiliary variable
# Modeling:	This is a reformulated model from the original one.
# Objective:	Find (a,b,e) if given (P,H,L)
# Authors:	Applied Computing and Mechanics Laboratory (IMAC) - EPFL, 
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), LIA - EPFL
# Version:	0.1
# Class:	NOI0-RNN-5-10
# Variables:	
#		Main: 5
#		Auxiliary: 0
# Constraints:	
#		Linear: 2
#		Non-linear: 8
#===============================================================================

#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param E := 210e6; # (Unit = kN/m2, Youngs modulus of steel)
param T := 235000.0; # (Unit = kN/m2, yield stress of steel)
param A := 0.25; # Area of cross section of truss members
param r := 0.5; # Radius of gyration of the cross section of truss members
param Pi := 4*atan(1);
param ZeroPlus := 0.00000001; # sufficiently small positive number


#-------------------------------------------------------------------------------
# 5 variables
#-------------------------------------------------------------------------------
var x1 >=0.01, <=10;
var y1 >=0.01, <=10;
var P >=350, <=450;
var H >=5, <=10;
var L >=1, <=10;


#-------------------------------------------------------------------------------
# Constraints
#-------------------------------------------------------------------------------

subject to

#Original C00 : (P/tan(atan((H-y1)/(L-x1)))) < T*A;
C00 : (P/tan(atan((H-y1)/(L-x1)))) <= T*A - ZeroPlus;

#Original C01 : ((P*L/H)*tan(atan(y1/x1))) < T*A;
C01 : ((P*L/H)*tan(atan(y1/x1))) <= T*A - ZeroPlus;

#Original C02 : (P/sin(atan((H-y1)/(L-x1)))) < (Pi^2*E/((((L-x1)^2+(H-y1)^2)^0.5)/r)^2)*A;
C02 : (P/sin(atan((H-y1)/(L-x1)))) <= (Pi^2*E/((((L-x1)^2+(H-y1)^2)^0.5)/r)^2)*A - ZeroPlus;

#Original C03 : (P/sin(atan((H-y1)/(L-x1)))) < T*A;
C03 : (P/sin(atan((H-y1)/(L-x1)))) <= T*A - ZeroPlus;

#Original C04 : ((P*L/H)/cos(atan(y1/x1))) < (Pi^2*E/(((x1^2+y1^2)^0.5)/r)^2)*A;
C04 : ((P*L/H)/cos(atan(y1/x1))) <= (Pi^2*E/(((x1^2+y1^2)^0.5)/r)^2)*A - ZeroPlus;

#Original C05 : ((P*L/H)/cos(atan(y1/x1))) < T*A;
C05 : ((P*L/H)/cos(atan(y1/x1))) <= T*A - ZeroPlus;

#Original C06 : abs((P*L/H - P/tan(atan((H-y1)/(L-x1))))/cos(atan((H-y1)/x1))) < T*A;
C06 : abs((P*L/H - P/tan(atan((H-y1)/(L-x1))))/cos(atan((H-y1)/x1))) <= T*A - ZeroPlus;

#Original C07 : if (P*L/H - P/tan(atan((H-y1)/(L-x1))))/cos(atan((H-y1)/x1)) <= 0 then -((P*L/H - P/tan(atan((H-y1)/(L-x1))))/cos(atan((H-y1)/x1))) < (Pi^2*E/(((x1^2+(H-y1)^2)^0.5)/r)^2)*A;
C07 : if (P*L/H - P/tan(atan((H-y1)/(L-x1))))/cos(atan((H-y1)/x1)) <= 0 then -((P*L/H - P/tan(atan((H-y1)/(L-x1))))/cos(atan((H-y1)/x1))) <= (Pi^2*E/(((x1^2+(H-y1)^2)^0.5)/r)^2)*A - ZeroPlus;

#Original C08 : x1 < L;
C08 : x1 <= L - ZeroPlus;

#Original C09 : y1 < H;
C09 : y1 <= H - ZeroPlus;

#solve;
#display x1, y1, P, H, L;
