# **************************************************************************
# File: Winkler
#       Test for GB
# 
# Reference :
# 
# @book{                    winkler-poly,
#    author       = "F. Winkler",
#    title        = "{P}olynomial {A}lgorithms in {C}omputer {A}lgebra",
#    publisher    = "Springer-Verlag",
#    year         = 1996
# }
# 
# ***************************************************************************





# Domains
var x >= -1.0e5, <= 1.0e5;
var y >= -1.0e5, <= 1.0e5;
var z >= -1.0e5, <= 1.0e5;


subject to
cons1 : 4*x*z - 4*x*y^2 - 16*x^2 - 1 = 0;
cons2 : 2*y^2*z + 4*x + 1 = 0;
cons3 : 2*x^2*z + 2*y^2 + x = 0;

# 
# , z^6 + (38/55)*z^5 - (47/55)*z^4 - (316/55)*z^3 - (862/55)*z^2 + (336/11)*z - (120/11) = 0,
# 
# 
# y^2 - (1445/1388)*z^4 - (3017/1388)*z^3 - (2405/1388)*z^2 + (6295/1388)*z + (16567/694) = 0
# 

#solve;
#display x, y, z;
