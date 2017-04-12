#===============================================================================
# Problem:	A Storage Hall with 50T Crane - Full dimensional
# Description:	This is the original model.
# Find:		(See the variables below)
# Origin:	Taken from "Collaborative Design Using Solution Space",
#		PhD Thesis by Claudio Lottaz, LIA/EPFL.
# AMPL coding: Xuan-Ha VU (xuan-ha.vu@epfl.ch), EPFL/I&C/LIA
# Version:	0.1
# Class:	NOI0-RNN-37-36
# Variables:	
#		Main: 37
#		Auxiliary: 0
# Constraints:	
#		Linear: 16
#		Non-linear: 20
#===============================================================================

#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param Pi := 3.14159265358979323846;
param ZeroPlus := 1e-12;

#-------------------------------------------------------------------------------
# 15 parameters; Enter your values here
#-------------------------------------------------------------------------------
param h_1	:= 12;		# h1, clear height from floor to the crane
param L		:= 41;		# L, length of the building
param w		:= 20.8;	# l, width of the building
param Q_C	:= 500;		# QnomPR, crane load

param d_1	:= 1;		# a, minumum distance between the crane load and its supporting rail
param s_C	:= 19.5;	# s, span of the crane
param h_2	:= 3.7;		# h2, height from crane to the roof
param d_2	:= 0.65;	# c, distance from column axis to the rail supporting the crane

param f_y	:= 235;		# fy, elastic limit
param E		:= 210;		# Esteel, Young's modulus
param D		:= 7850;	# mv, steel density

param s		:= 5.8;		# e, spacing between frames
param lambda	:= 0.1;		# lamda, coefficient for crane's horizontal strength
param xi	:= 0.15;	# ksi, lifting coefficient
param gamma_r	:= 1.1;		# gammar, resistance factor

#-------------------------------------------------------------------------------
# 37 variables
#-------------------------------------------------------------------------------

#Geometric parameters
var w_C		>=390		,<=890		;#690, ec, flange width of column
var w_B		>=390		,<=890		;#540, et, flange width of beam
var h_tot	>=0		,<=50		;#15.7, htot, total height of the building

#Materials property features
var E_pB	>=0		,<=15000000	;#10000000, Zyt, plastic modulus of the beam
var M_BB	>=-3000		,<=3000		;#0, MDt, moment causing buckling for beam
var M_BC	>=-3000		,<=3000		;#0, MDc, moment causing buckling for column
var I_Bxx	>=0		,<=10000	;#2000, It, moment of Inertia of the beam
var I_Cxx	>=0		,<=10000	;#2000, Ic, moment of Inertia of the column
var A_C		>=0		,<=100000	;#20000, Ac, area of column cross section
var A_B		>=0		,<=100000	;#20000, At, area of beam cross section
var r_Cxx	>=0		,<=500		;#200, iyc, radius of gyration of the column

#Loads
var g		>=-50		,<=50		;#10, qpptoit, dead load of the roof
var q_s		>=-50		,<=50		;#20, qrneige, snow load
var q_w1	>=-50		,<=50		;#2, vr1, west facade wind pressure
var q_w2	>=-50		,<=50		;#2, vr2, west roof wind pressure
var q_w3	>=-50		,<=50		;#2, vr3, east roof wind pressure
var q_w4	>=-50		,<=50		;#2, vr4, east facade wind pressure

var g_C		>=-5000		,<=5000		;#0, QppPR, self weight of the crane
var Q_rmax	>=-5000		,<=5000		;#0, Qrmax, max. load on crane supporting rail
var Q_Tr	>=-10000	,<=10000	;#1000, QTr, braking load of the crane
var phi		>=0		,<=100		;#2, phi, dynamic amplification factor

#Structural analysis
var beta	>=0		,<=2		;#1.2, beta, effective length coefficient for buckling
var k		>=0		,<=20		;#1, k???, frame's relative stiffness coefficient
var M_dC	>=-3000		,<=3000		;#0, VAd1???, design moment in the column footing
var M_dB	>=-3000		,<=3000		;#0, MBd1, design moment in the frame joint
var V_dC	>=-3000		,<=3000		;#0, VAd1, shear force in the column footing
var M_pl	>=-3000		,<=3000		;#0, Mpl, plastic moment of the beam
var gamma_BR	>=0.1		,<=4		;#1, gammastpl, factor for cross section strength
var gamma_BB	>=0.1		,<=4		;#1, gammastD, factor for cross section buckling
var N_cry	>=0		,<=40000	;#10000, Ncry, critical elastic buckling load
var N_ky	>=0		,<=20000	;#10000, Nky, ultimate buckling load
var sigma_k	>=0		,<=250		;#170, sigmak, ultimate buckling stress
var l_kC	>=10		,<=50		;#30, lkc, effective length of the column
var lambda_k	>=0		,<=150		;#70, lamdak, slenderness ratio
var f_2nd	>=0		,<=100		;#1, fac2or, second order factor
var gamma_C	>=0.1		,<=4		;#1, gammasc, column security factor
var q_steel	>=10000		,<=150000	;#50000, qacier, quantity of steel

#-------------------------------------------------------------------------------
# constraints
#-------------------------------------------------------------------------------

subject to

# Constraints by client
#ClientC00:	h_1 = 12;
#ClientC01:	L = 41;
#ClientC02:	w = 20.8;
#ClientC03:	Q_C = 500;
ClientC04:	s >= 5;


# Constraints by crane supplier
#CraneC00:	d_1 = 1;
#CraneC01:	s_C = 19.5;
#CraneC02:	h_2 = 3.7;
CraneC03:	g_C = 2.55*s_C;
#CraneC04:	d_2 = 0.65;


# Constraints by wind expert
WindC00:	q_w1 = 0.96*s;
WindC01:	q_w2 = 0.6*s;
WindC02:	q_w3 = 0.36*s;
WindC03:	q_w4 = 0.36*s;


# Constraints by steel fabricator
SteelC00:	E_pB = -1065250 + 6310*w_B + 7.795*w_B^2;
SteelC01:	M_BB = -298.9 + 1.71*w_B + 0.00136*w_B^2;
SteelC02:	M_BC = -298.9 + 1.71*w_C + 0.00136*w_C^2;
SteelC03:	I_Bxx = 855 - 4.66*w_B + 0.00948*w_B^2;
SteelC04:	I_Cxx = 855 - 4.66*w_C + 0.00948*w_C^2;
SteelC05:	A_B = 4247 + 31.08*w_B;
SteelC06:	A_C = 4247 + 31.08*w_C;
SteelC07:	r_Cxx = 20.8 + 0.385*w_C;
#SteelC08:	f_y = 235;
#SteelC09:	E = 210;
#SteelC10:	D = 7850;


#Constraints by civil engineer
CivilC00:	h_tot = h_1 + h_2;
#CivilC01:	s = 5.8;
CivilC02:	g = 1.7*s + 1.7;
CivilC03:	q_s = 0.8*s;
CivilC04:	Q_rmax = Q_C*(s_C - d_1)/s_C;
CivilC05:	Q_Tr = 0.5*lambda*Q_rmax*phi;
#CivilC06:	lambda = 0.1;
CivilC07:	phi = 1 + xi*Q_C/Q_rmax;
#CivilC08:	xi = 0.15;
CivilC09:	beta = 20/h_tot;
CivilC10:	l_kC = beta*h_tot;
CivilC11:	k = I_Bxx*h_tot/(I_Cxx+ZeroPlus);
#CivilC12:	gamma_r = 1.1;
CivilC13:	M_pl = 1e-6*f_y*E_pB;
CivilC14:	gamma_BR = M_pl/(gamma_r*M_dB+ZeroPlus);
CivilC15:	gamma_BR >=1;
CivilC16:	gamma_BB = M_BB/(gamma_r*M_dB+ZeroPlus);
CivilC17:	gamma_BB >=1;
CivilC18:	N_cry = (Pi^2*E*I_Cxx)/l_kC^2;
CivilC19:	N_ky = 1e-3*sigma_k*A_C;
CivilC20:	sigma_k = 271.5 - 1.392*lambda_k;
CivilC21:	lambda_k = 1000*l_kC/r_Cxx;
CivilC22:	f_2nd = 1/(1 - V_dC/(N_cry+ZeroPlus));
CivilC23:	1/gamma_C = V_dC*gamma_r/(N_ky+ZeroPlus) + f_2nd*(M_dC*gamma_r/(M_BC+ZeroPlus));
CivilC24:	gamma_C >=1;
CicilC25:	q_steel = ((L + s)*D/(1e6*s))*(w*A_B + 2*h_tot*A_C);

#solve;
