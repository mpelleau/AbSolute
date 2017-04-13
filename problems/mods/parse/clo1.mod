# 
# TITLE: test for Groebner bases
# 
# Reference:
# David Cox, John Little, Donal O'Shea.
# Ideals, Varieties and Algorithms: An introduction to computational
# algebraic geometry and commutative algebra.
# Springer Verlag, 1992.
# 



# Domains
var x >= -10, <= 10;
var y >= -10, <= 10;
var z >= -10, <= 10;

subject to
cons1 : x + y + z^2 = 1;
cons2 : x + y^2 + z = 1;
cons3 : x^2 + y + z = 1;

# 
# , z^2*(z^4 - 4*z^2 + 4*z - 1) = 0,
# y^2 - y - z^2 + z = 0
# 



#solve;
#display x, y, z;
