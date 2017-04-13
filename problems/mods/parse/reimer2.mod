# Domains
var x >= -1.0e8, <= 1.0e8;
var y >= -1.0e8, <= 1.0e8;

subject to
cons1 : x^2 - y^2  = 0.5;
cons2 : x^3 - y^3  = 0.5;

#solve;
#display x, y;
