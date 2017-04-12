# **************************************************************************
# TITLE : optimal multi-dimensional quadrature formulas
# System given by Rabinowitz (1977)
# 
# References :
# @unpublished{bini-mourrain-poly,
# author  = "D. Bini and B. Mourrain",
# title   = "{Handbook} of {Polynomial} {Systems}",
# month   = "November",
# year    = 1996
# }
# 
# Ramon E. Moore:
# `Methods and applications of interval analysis',
# chapter 6, page 64, SIAM Philadelphia, 1979.
# 
# Sandie T. Jones:
# `Locating safe starting regions for iterative methods:
#  a heuristic algorithm', in Interval Mathematics 1980, pages 377-386,
# Academic Press 1980, Editor Karl L.E. Nickel,
# Proceedings of an International Symposium on Interval Mathematics,
# May 27-31, 1980.
# 
# 
# Derivation of optimal multi-dimensional integration formulae
# 16 finite solutions
# 
# Solution by Verschelde in 2510.9s (IBM RS/6000)
# ***************************************************************************



# Domains
var x1 >= -1.0e8, <= 1.0e8;
var x2 >= -1.0e8, <= 1.0e8;
var x3 >= -1.0e8, <= 1.0e8;
var x4 >= -1.0e8, <= 1.0e8;
var x5 >= -1.0e8, <= 1.0e8;
var x6 >= -1.0e8, <= 1.0e8;
var x7 >= -1.0e8, <= 1.0e8;
var x8 >= -1.0e8, <= 1.0e8;
var x9 >= -1.0e8, <= 1.0e8;
var w  >= -1.0e8, <= 1.0e8;
var v  >= -1.0e8, <= 1.0e8;
var u  >= -1.0e8, <= 1.0e8;
var t  >= -1.0e8, <= 1.0e8;
var r  >= -1.0e8, <= 1.0e8;
var s  >= -1.0e8, <= 1.0e8;
var q  >= -1.0e8, <= 1.0e8;

subject to
cons1 : x1 + x3 + x5 + 2*x7 = 1;

cons2 : u      + t      + 2*v      + 2*x7*s           = 2/3;
cons3 : u*x2   + t*x4   + 2*v*x6   + 2*x7*r           = 2/5;
cons4 : u*x2^2 + t*x4^2 + 2*v*x6^2 + 2*x7*(x8^3+x9^3) = 2/7;
cons5 : u*x2^3 + t*x4^3 + 2*q + 2*x7*(x8^4+x9^4) = 2/9;

cons6 : v*x6   + 2*w  = 1/9;
cons7 : q + 2*w*x8*x9 = 1/25;
cons8 : v*x6^2 + w*s  = 1/15;
cons9 : q + w*r       = 1/21;

cons10 : w = x7*x8*x9;
cons11 : v = x5*x6;
cons12 : u = x1*x2;
cons13 : t = x3*x4;
cons14 : r = x8^2+x9^2;
cons15 : s = x8+x9;
cons16 : q = v*x6^3;


#  ORIGINAL SYSTEM:
# x1+x3+x5+2*x7 = 1,
# x1*x2+x3*x4+2*x5*x6+2*x7*(x8+x9) = 2/3,
# x1*x2^2+x3*x4^2+2*x5*x6^2+2*x7*(x8^2+x9^2) = 2/5,
# x1*x2^3+x3*x4^3+2*x5*x6^3+2*x7*(x8^3+x9^3) = 2/7,
# x1*x2^4+x3*x4^4+2*x5*x6^4+2*x7*(x8^4+x9^4) = 2/9,
# x5*x6^2+2*x7*x8*x9 = 1/9,
# x5*x6^4+2*x7*x8^2*x9^2 = 1/25,
# x5*x6^3+x7*x8*x9^2+x7*x8^2*x9 = 1/15,
# x5*x6^4+x7*x8*x9^3+x7*x8^3*x9 = 1/21
# 

#solve;
#display q, r, s, t, u, v, w, x1, x2, x3, x4, x5, x6, x7, x8, x9;
