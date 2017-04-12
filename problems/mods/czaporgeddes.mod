

# Domains
var x >= -1.0e8, <= 1.0e8;
var y >= -1.0e8, <= 1.0e8;
var z >= -1.0e8, <= 1.0e8;


subject to
cons1 : 8*x^2 - 2*x*y - 6*x*z + 3*x + 3*y^2 - 7*y*z + 10*y + 10*z^2 - 8*z = 4;
cons2 : 10*x^2 - 2*x*y + 6*x*z - 6*x + 9*y^2 - y*z - 4*y - 2*z^2 + 5*z = 9;
cons3 : 5*x^2 + 8*x*z + 4*x*z + 8*x + 9*y^2 - 6*y*z + 2*y - z^2 - 7*z + 5 = 0;

#solve;
#display x, y, z;
