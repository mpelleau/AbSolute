#===============================================================================
# Problem:	A Stacked Gymnastic Halls - Full dimensional
# Description:	This is the original model.
# Find:		(E, H_s1, H_s2, L, c, d , h_a, k, m, P, q_concrete, t)
# Origin:	Taken from "Collaborative Design Using Solution Space",
#		PhD Thesis by Claudio Lottaz, LIA/EPFL.
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), EPFL/I&C/LIA
# Version:	0.1
# Class:	NOI0-RNN-12-15
# Variables:	
#		Main: 12
#		Auxiliary: 0
# Constraints:	
#		Linear: 10
#		Non-linear: 5
#===============================================================================

#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param Pi := 3.14159265358979323846;
param ZeroPlus := 1e-12;


#-------------------------------------------------------------------------------
# 5 parameters; Enter your values here
#-------------------------------------------------------------------------------

# by dynamics experts
param E_steel	:= 220000;	#

# by civil engineer
param R_f	:= 2;		#
param S		:= 1.65;	#

# additional
param h_t	:= 0.5;		#
param L_z	:= 11.5;	#


#-------------------------------------------------------------------------------
# 12 variables
#-------------------------------------------------------------------------------
var E		>=2	,<=10	;#2	# beam spacing [m]
var H_s1	>=5	,<=8	;#7	# height of lower gym [m]
var H_s2	>=5	,<=8	;#7	# height of upper gym [m]
var L		>=10	,<=50	;#27.4	# span [m]
var c		>=200	,<=500	;#200	# half cover plate width [mm]
var d		>=20	,<=40	;#20	# web thickness [mm]
var h_a		>=800	,<=4000	;#4000	# beam height [mm]
var k		>=1	,<=1e8	;#1e7	# modal stiffness [N/m]
var m		>=1	,<=1e6	;#1000	# modal weight [kg]
var P		>=0	,<=10	;#5	# depth of foundation [m]
var q_concrete	>=200	,<=2000	;#200	# self weight [kg/m]
var t		>=20	,<=100	;#20	# half cover plate thickness [mm]


#-------------------------------------------------------------------------------
# constraints
#-------------------------------------------------------------------------------

subject to

# Constraints by client
ClientC00:	H_s1 >= 5.5 + ZeroPlus;	# needed for certain sports
ClientC01:	H_s2 >= 5.5 + ZeroPlus;	# needed for certain sports
ClientC02:	E >= 2.5 + ZeroPlus;	# needed to mount sports facilities

# Constraints by contractor
ContractorC00:	4.5*t <= c;
ContractorC01:	c <= 8.5*t;
ContractorC02:	40*d <= h_a ;
ContractorC03:	h_a <= 100*d;

# Constraints by dynamics expert
DynamicsC00:	q_concrete = 200 * E^0.6;
DynamicsC01:	m = q_concrete * E * L / 2;
DynamicsC02:	k = E_steel/L^3/1E6*(3.37*h_a^3*d+42.84*h_a^2*t*c);
DynamicsC03:	(k/m)^(1/2) >= 8 * 2 * Pi;

# Constraints by civil engineer
CivilC00:	1/2 * h_a * d * 235 / 3^(1/2) >= 1000 * L * E^(8/5)*R_f*S;
CivilC01:	235*t*c*h_a/500 >= 250 * L * E^(8/5)*R_f*S;

# Constraints by Geologist
GeologistC00:	P <= 5 - ZeroPlus;

# Constraints by architect
ArchitectC00:	H_s1 + H_s2 + h_a/1000 + h_t <= L_z + P - ZeroPlus;
#ArchitectC01:	L = 27.4;


#-------------------------------------------------------------------------------
#solve;
#display E, H_s1, H_s2, L, c, d , h_a, k, m, P, q_concrete, t;
