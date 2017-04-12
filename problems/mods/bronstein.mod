# Domains
var x >= -1.0e8, <= 1.0e8;
var y >= -1.0e8, <= 1.0e8;
var z >= -1.0e8, <= 1.0e8;


subject to
cons1 : x^2 + y^2 + z^2 = 36;
cons2 : x+y = z;
cons3 : x*y + z^2 = 1;

#solve;
#display x, y, z;
