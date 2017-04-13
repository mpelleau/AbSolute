
# Domains
var x >= -1.0e8, <= 1.0e8;
var y >= -1.0e8, <= 1.0e8;
var z >= -1.0e8, <= 1.0e8;

subject to
cons1 : 13 + y^2*(1+z^2) + z*(z - 24*y)  = 0;

cons2 : 13 + z^2*(1+x^2) + x*(x - 24*z)  = 0;

cons3 : 13 + x^2*(1+y^2) + y*(y - 24*x)  = 0;


#  ,z^8 - 140*z^6 + 2622*z^4 - 1820*z^2 + 169 = 0




#solve;
#display x, y, z;
