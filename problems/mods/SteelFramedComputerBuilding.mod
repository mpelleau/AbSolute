#===============================================================================
# Problem:	A Steel-Framed Computer Building - Full dimensional
# Description:	This is the original model.
# Find:		(See the variables below)
# Origin:	Taken from "Collaborative Design Using Solution Space", 
#		PhD Thesis by Claudio Lottaz, LIA/EPFL.
# AMPL coding:	Xuan-Ha VU (xuan-ha.vu@epfl.ch), EPFL/I&C/LIA
# Version:	0.1
# Class:	NOI0-RNN-42-44
# Variables:	
#		Main: 42
#		Auxiliary: 0
# Constraints:	
#		Linear: 13
#		Non-linear: 31
#===============================================================================

#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
param Pi := 3.14159265358979323846;
param ZeroPlus := 1e-12;

#-------------------------------------------------------------------------------
# 10 parameters; Enter your values here
#-------------------------------------------------------------------------------
param E		:= 0.21e12;
param F_r	:= 0.4;
param G_m	:= 1500;
param G_q	:= 1.5;
param G_r	:= 1.1;
param L_sc	:= 4000;
param M_FH	:= 3.5;
param Q_r	:= 5000;
param d_o	:= 0.5e-1;
param f_y	:= 2.35e8;


#-------------------------------------------------------------------------------
# 42 variables
#-------------------------------------------------------------------------------
var A_1		>=1e-5	,<=0.16	;	#8.36e-3 chord area
var A_1w	>=0	,<=0.08	;	#1.6e-3 web area of chord
var A_2		>=0	,<=1.2	;	#6.24e-3 vertical x-sectional area
var A_b		>=85	,<=700	;	#360 floor area
var B_V		>=0	,<=7000	;	#72.8 Building volume
var F_H		>=0	,<=5	;	#3.64 Total floor height
var I_y		>=1e-5	,<=0.14	;	#8.28e-4 Moment of inertia
var L		>=12	,<=35	;	#15 beam span
var M		>=0	,<=3.125e6;	#7.79e4 applied moment
var N_1		>=0	,<=3e5	;	#1.76e5 applied flange force
var N_1pl	>=0	,<=7e7	;	#1.97e6 plastic flange strength
var S		>=0	,<=0.2	;	#3.69e-3 static moment
var T_d		>=0	,<=2e8	;	#1.13e7 tangent forces
var V_1		>=0	,<=6e5	;	#1.81e4 shear force
var V_1pl	>=0	,<=2e7	;	#2.17e5 streer strength
var V_2		>=0	,<=3.125e8;	#1.4e5 shear force
var V_2pl	>=0	,<=3e8	;	#8.47e5 strear strength
var V_f		>=0	,<=3	;	#2.36e-2 ventilation flow
var Z_1		>=1e-7	,<=0.06	;	#1.44e-4 plastic modulus
var a		>=0	,<=0.75	;	#0.145 distacne between outer fibre and hole edge
var a_N		>=0	,<=1.5	;	#0.442 distance between flange forces
var b		>=0	,<=1.5	;	#0.467 beam depth
var b_s		>=1e-5	,<=20	;	#1.0 beam spacing
var c		>=0	,<=0.4	;	#0.15 half flange width
var c_V		>=8.5e-4,<=15e-4;	#0.0015 constant depending on ventilation system chosen
var c_n		>=1e-5	,<=0.75	;	#0.133 Neutral axis of plastification
var d		>=0	,<=1	;	#0.2 hole diameter
var d_v		>=0	,<=1.2	;	#0.1 diameter needed for ventilation
var e		>=0.7	,<=1.2	;	#0.72 hole spacing
var h		>=0.1	,<=1	;	#0.55 beam depth
var n_d		>=1	,<=5	;	#2 number of ducts
var n_h		>=0	,<=20	;	#7 number of holes
var q		>=0	,<=65000;	#16001 linear loading
var q_sc	>=0	,<=40000;	#8400 short term live linear load
var t		>=0	,<=0.016;	#0.012 flange thickness
var t_fs	>=0.03	,<=0.5	;	#0.03 floor surface thickness
var t_s		>=0.12	,<=0.5	;	#0.12 slab thickness
var t_w		>=0	,<=0.014;	#0.012 web thickness
var w_l		>=0	,<=0.5	;	#0.001 beam deflection due to loads
var x		>=0.8	,<=1.2	;	#0.85 distance to first hole
var y_1		>=0	,<=18	;	#0.86 distance from support to center of 1st hole
var y_2		>=0	,<=30	;	#1.58 distance from support to center of 2nd hole


#-------------------------------------------------------------------------------
# constraints
#-------------------------------------------------------------------------------

subject to

# Constraints by architect
ArchitectC00:	L = 2*x + d + (n_h - 1)*e;
ArchitectC01:	F_H = M_FH + h + t_s + t_fs;
ArchitectC02:	B_V = F_H*A_b;
ArchitectC03:	h = 2*a + d;
ArchitectC04:	h = b + t;
ArchitectC05:	n_h >= n_d;
#ArchitectC06:	L = 9;
#ArchitectC08:	n_d = 3;
#ArchitectC09:	A_b = 360;

# Constraints by contractor
ContractorC00:	x >= 1.5*d + ZeroPlus;
ContractorC01:	e >= 2.5*d + ZeroPlus;
ContractorC02:	d <= 0.75*h - ZeroPlus;

# Constraints by civil engineer
CivilC00:	n_h >= 3;
CivilC01:	N_1 <= N_1pl/G_r - ZeroPlus;
#CivilC02:	N_1 = M/a_N;
CivilC02:	N_1*a_N = M; # to avoid "0/0"
CivilC03:	M = (q*L^2)/8;
CivilC04:	q = (1.3*G_m + G_q*Q_r)*b_s;
CivilC05:	a_N = S/A_1;
CivilC06:	S = 2*c*t*b + t_w*(a - t)*(b - a);
CivilC07:	A_1 = 2*c*t + t_w*(a - t);
CivilC08:	N_1pl = f_y*A_1;
CivilC09:	N_1/A_1 + V_1*d/4/Z_1 <= sqrt(f_y^2 - 3*T_d^2)/G_r - ZeroPlus;
CivilC10:	T_d = V_1/A_1w;
CivilC11:	A_1w = (a - t/2)*t_w;
CivilC12:	V_1 = q/2 * (L/2 - y_1);
CivilC13:	V_1 <= V_1pl/G_r - ZeroPlus;
CivilC14:	V_2 <= V_2pl/G_r - ZeroPlus;
CivilC15:	2*V_2*a_N - (y_2^2 - y_1^2 + L*e)*q = 0;
CivilC16:	y_1 = x + d/2;
CivilC17:	y_2 = y_1 + e;
CivilC18:	V_1pl = f_y*A_1w/sqrt(3);
CivilC19:	V_2pl = f_y*A_2/sqrt(3);
CivilC20:	A_2 = (e - d)*t_w;
CivilC21:	(a - t/2)/t_w <= 11 - ZeroPlus;
CivilC22:	1.2*w_l <= L/350 - ZeroPlus;
CivilC23:	w_l = 5/384*q_sc*L^4/(E*I_y);
CivilC24:	q_sc = L_sc*b_s;
CivilC25:	I_y = 4*c*t*(b/2)^2 + t_w*(a - t) * (b - a)^2/2;
CivilC26:	t_w*(a - t) >= 2*c*t + ZeroPlus;
CivilC27:	c_n = A_1/(2*t_w);
CivilC28:	Z_1 = c_n^2*t_w/2 + (a - c_n - t_w)^2/ 2*t_w + 2*(a - c_n - t/2)*c*t;
#additional constants
CivilC29:	t_fs = 0.03;
CivilC30:	t_s = 0.12;
CivilC31:	t_w = 0.012;

# Constraints by Ventilation expert
VentilC00:	d >= d_v + d_o + ZeroPlus;
VentilC01:	V_f >= c_V*B_V + ZeroPlus;
VentilC02:	V_f = n_d*F_r*Pi*d_v^2/4;


#-------------------------------------------------------------------------------
#solve;

#display A_1;
#display A_1w;
#display A_2;
#display A_b;
#display B_V;
#display F_H;
#display I_y;
#display L;
#display M;
#display N_1;
#display N_1pl;
#display S;
#display T_d;
#display V_1;
#display V_1pl;
#display V_2;
#display V_2pl;
#display V_f;
#display Z_1;
#display a;
#display a_N;
#display b;
#display b_s;
#display c;
#display c_V;
#display c_n;
#display d;
#display d_v;
#display e;
#display h;
#display n_d;
#display n_h;
#display q;
#display q_sc;
#display t;
#display t_fs;
#display t_s;
#display t_w;
#display w_l;
#display x;
#display y_1;
#display y_2;
