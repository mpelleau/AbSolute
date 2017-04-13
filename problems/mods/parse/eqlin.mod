# Domains
var x >= -1.0e8, <= 1.0e8;
var y >= -1.0e8, <= 1.0e8;
var z >= -1.0e8, <= 1.0e8;

subject to

cons1 : 3*x + 5*y = 3*z - 1;
cons2 : x   = 2*y - z;
cons3 : 2*x + 4*y + 7*z = 18;


#solve;
#display x, y, z;
