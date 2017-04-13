# -----------------------------------------------------------------
#  Reference : Numerical Methods for Mathematical, Science and Engineering
#              John H. Mathews
#              Prentice Hall International Editions - 1992 - 2nd edition
# 
#  exercice 13 page 120
# -----------------------------------------------------------------


# Domains
var x >= -1000, <= 1000;
var y >= -1000, <= 1000;
var z >= -1000, <= 1000;

subject to
cons1 : x^2 - x + y^2 + z^2 = 5;
cons2 : x^2 + y^2 - y + z^2 = 4;
cons3 : x^2 + y^2 + z^2 + z = 6;


# 
# x = y - 1,
# y + z = 2,
# z^2 -(5/3)*z = 1/3
# 

#solve;
#display x, y, z;
