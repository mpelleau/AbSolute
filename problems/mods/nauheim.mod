# 
# TITLE : small system from constructive Galois theory, called s9_1
# 
# REFERENCE:
# 
# Raphael Nauheim:
#  "Systems of Algebraic Equations with Bad Reduction",
# Universitaet Heidelberg, Interdisziplinaeres Zentrum
# fuer wissenschaftliches Rechnen, Preprint 95-46, Dezember 1995.
# 
# There are four real and six complex conjugated solutions.
# 


# Domains
var a >= -1.0e8, <= 1.0e8;
var b >= -1.0e8, <= 1.0e8;
var c >= -1.0e8, <= 1.0e8;
var d >= -1.0e8, <= 1.0e8;
var e >= -1.0e8, <= 1.0e8;
var f >= -1.0e8, <= 1.0e8;
var g >= -1.0e8, <= 1.0e8;
var h >= -1.0e8, <= 1.0e8;

subject to
cons1 : e*g + 2*d*h = 0;
cons2 : 9*e + 4*b = 0;
cons3 : 4*c*h + 2*e*f + 3*d*g = 0;
cons4 : 7*c - 9*a + 8*f = 0;
cons5 : 4*d*f + 5*c*g + 6*h + 3*e = 0;
cons6 : 5*d + 6*c*f + 7*g - 9*b = 0;
cons7 : 9*d + 6*a = 5*b;
cons8 : 9*c = 7*a - 8;

#solve;
#display a, b, c, d, e, f, g, h;
