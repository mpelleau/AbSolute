# **************************************************************************
# File: Dipole
# Name: heart-dipole problem
# 
# Reference :
# 
# @unpublished{             bini-mourrain-poly,
# author          = "D. Bini and B. Mourrain",
# title           = "{Handbook} of {Polynomial} {Systems}",
# month           = "November",
# year            = 1996
# }
# Nelsen, C.V. and Hodgkin, B.C.:
# `Determination of magnitudes, directions, and locations of two independent
#  dipoles in a circular conducting region from boundary potential measurements'
# IEEE Trans. Biomed. Engrg. Vol. BME-28, No. 12, pages 817-823, 1981.
# 
# Morgan, A.P. and Sommese, A.J.:
# `Coefficient-Parameter Polynomial Continuation'
# Appl. Math. Comput. Vol. 29, No. 2, pages 123-160, 1989.
# Errata: Appl. Math. Comput. 51:207 (1992)
# 
# Morgan, A.P. and Sommese, A. and Watson, L.T.:
# `Mathematical reduction of a heart dipole model'
# J. Comput. Appl. Math. Vol. 27, pages 407-410, 1989.
# 
# ***************************************************************************



# Domains
var a >= -10, <= 10;
var b >= -10, <= 10;
var c >= -10, <= 10;
var d >= -10, <= 10;
var t >= -10, <= 10;
var u >= -10, <= 10;
var v >= -10, <= 10;
var w >= -10, <= 10;

var x >= -10, <= 10;
var y >= -10, <= 10;

subject to



cons1 : a + b = 0.63254;
cons2 : c + d = -1.34534;

cons3 : t*(0.63254-b) + u*b - v*(-1.34534-d) - w*d = -0.8365348;
cons4 : v*(0.63254-b) + w*b + t*(-1.34534-d) + u*d = 1.7345334;

cons5 : (0.63254-b)*x - 2*(-1.34534-d)*t*v + b*y - 2*d*u*w = 1.352352;
cons6 : (-1.34534-d)*x + 2*(0.63254-b)*t*v + d*y + 2*b*u*w = -0.843453;

cons7 : (0.63254-b)*t*(x - 2*v^2)+ (-1.34534-d)*v*(-x - 2*t^2)+ b*u*(y - 2*w^2)+ d*w*(-y -2*u^2) = -0.9563453;

cons8 : (-1.34534-d)*t*(x - 2*v^2)- (0.63254-b)*v*(-x -2*t^2)+ d*u*(y - 2*w^2)- b*w*(-y -2*u^2) = 1.2342523;

cons9 : x = t^2 - v^2;
cons10 : y = u^2 - w^2;


#  ORIGINAL SYSTEM:
# a + b = 0.63254,
# c + d = -1.34534,
# 
# t*a + u*b - v*c - w*d = -0.8365348,
# v*a + w*b + t*c + u*d = 1.7345334,
# 
# a*t**2 - a*v**2 - 2*c*t*v + b*u**2 - b*w**2 - 2*d*u*w = 1.352352,
# c*t**2 - c*v**2 + 2*a*t*v + d*u**2 - d*w**2 + 2*b*u*w = -0.843453,
# 
# a*t**3 - 3*a*t*v**2 + c*v**3 - 3*c*v*t**2 
#   + b*u**3 - 3*b*u*w**2 + d*w**3 - 3*d*w*u**2 = -0.9563453,
# 
# c*t**3 - 3*c*t*v**2 - a*v**3 + 3*a*v*t**2 
#   + d*u**3 - 3*d*u*w**2 - b*w**3 + 3*b*w*u**2 = 1.2342523
# 

#solve;
#display a, b, c, d, t, u, v, w, x, y;
