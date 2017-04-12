#===============================================================================
# Problem:	Column Design
# Instance:	Full dimensional; no auxiliary variable
# Modeling:	This is a reformulated model from the original one.
# Objective:	Find (a,b,e,P,H,L)
# Authors:	Applied Computing and Mechanics Laboratory (IMAC) - EPFL, 
#		http://imacsg4.epfl.ch:8080/PGSL/java/column.html
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), LIA - EPFL
# Version:	0.1
# Class:	NOI0-RNN-6-5
# Variables:	
#		Main: 6
#		Auxiliary: 0
# Constraints:	
#		Linear: 2
#		Non-linear: 3
#===============================================================================


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param rho := 78; # (Unit = kN/m3, density of steel)
param fy := 235000.0; # (Unit = kN/m2, yield stress of steel)
param gammaR := 1.1; # (Unit = 0, safety factor, gammaR)
param alpha := 0.21; # (Unit = 0)
param E := 210e+6; # (Unit = kN/m2, Youngs modulus of steel)
param lambdaE := 94; # (Unit = 0, lambdaE)
param maxWeight := 10; # (Unit = KN)

param Pi := 3.14159265358979323846;
param ZeroPlus := 1e-12; # sufficiently small positive number


#-------------------------------------------------------------------------------
# 0 parameters; Enter your values here
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# 6 variables
#-------------------------------------------------------------------------------
var a >=0.01, <=2; #unit=meter
var b >=0.01, <=1; #unit=meter
var e >=0.05, <=0.1; #scale up 10 times, unit=decimeter
var P >= 100, <=1000;
var H >= 1, <=10;
var L >=.1, <=5;


#-------------------------------------------------------------------------------
# 5 constraints
#-------------------------------------------------------------------------------
subject to

#Strength
#this constraint uses ^0.5 instead of sqrt()
C00 : (P/((min(1, 1/((0.5*(1 + alpha*(((2*H/((((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*(e/10)*(b+a) - 4*(e/10)^2))^0.5))/lambdaE)-0.2) + ((2*H/((((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*(e/10)*(b+a) - 4*(e/10)^2))^0.5))/lambdaE)^2)) + max(0, (0.5*(1 + alpha*(((2*H/((((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*(e/10)*(b+a) - 4*(e/10)^2))^0.5))/lambdaE)-0.2) + ((2*H/((((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*(e/10)*(b+a) - 4*(e/10)^2))^0.5))/lambdaE)^2))^2 - ((2*H/((((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*(e/10)*(b+a) - 4*(e/10)^2))^0.5))/lambdaE)^2)^0.5))*fy*(2*(e/10)*(b+a) - 4*(e/10)^2))/gammaR) + (1/(1-P/(Pi^2*E*((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*H)^2)))*(P*(L-b/2))/(((((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(b/2))*fy)/gammaR)) <= 1 - ZeroPlus; 


#Cost(weight)
C01 : (rho*(2*(e/10)*(b+a) - 4*(e/10)^2)*H) <= maxWeight - ZeroPlus;

#Euler Buckling
C02 : P <= (Pi^2*E*((e/10)*((a*(3*b^2+4*(e/10)^2)+b*(b^2+12*(e/10)^2)) - (e/10)*(6*a*b+6*b^2+8*(e/10)^2))/6)/(2*H)^2) - ZeroPlus ;

C03 : a >= 4*e/10;

C04 : b >= 4*e/10;


#solve;
#display a, b, e, P, H, L;