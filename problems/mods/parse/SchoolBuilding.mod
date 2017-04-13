#===============================================================================
# Problem:	School Building - Full dimensional
# Description:	This is the original model.
# Find:		(See the variables)
# Origin:	Taken from SpaceSolver, LIA/EPFL.
#		http://liawww.epfl.ch/~lottaz/SpaceSolver/
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), EPFL/I&C/LIA
# Version:	0.1
# Class:	NOI0-RNN-6-5
# Variables:	
#		Main: 6
#		Auxiliary: 0
# Constraints:	
#		Linear: 0
#		Non-linear: 5
#===============================================================================


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param ZeroPlus := 1e-12;

#-------------------------------------------------------------------------------
# 6 variables
#-------------------------------------------------------------------------------

var H_b		>=150	,	<=400	;	#300 Beam depth
var H_s		>=50	,	<=200	;	#100 Slab depth
var S		>=1500	,	<=2200	;	#2000 Beam spacing
var W		>=3000	,	<=13500	;	#10000 Beam span
var p		>=15	,	<=50	;	#30 -
var u		>=10	,	<=60	;	#40 -


#-------------------------------------------------------------------------------
# 5 constraints
#-------------------------------------------------------------------------------
subject to

C00:	u <= (3.18e-5*H_s + 0.0054)*S - ZeroPlus;
C01:	H_s >= 137.7 - 0.08633*S + 5.511e-5*S^2 - 8.358e-9*S^3 + ZeroPlus;
#C01:	H_s >= 137.7 - 0.08633*S + 5.511e-5*S^2 - 8.358e-7*S^3 + ZeroPlus; #In Djamila's PhD Thesis, the last item is 8.358e-7*S^3 ????
C02:	p = u + 9.62e-5*(0.0417*W)^1.5161;
C03:	H_b >= 0.077*(p*W^2)^0.3976 + ZeroPlus;
C04:	H_b >= 0.0168*(S*W^3)^0.2839 + ZeroPlus;


#-------------------------------------------------------------------------------
#solve;
#display H_b, H_s, S, W, p, u;