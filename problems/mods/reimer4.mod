
# Domains
var x >= -1.0e8, <= 1.0e8;
var y >= -1.0e8, <= 1.0e8;
var z >= -1.0e8, <= 1.0e8;
var t >= -1.0e8, <= 1.0e8;

subject to
cons1 : x^2 - y^2 + z^2 - t^2 = 0.5;
cons2 : x^3 - y^3 + z^3 - t^3 = 0.5;
cons3 : x^4 - y^4 + z^4 - t^4 = 0.5;
cons4 : x^5 - y^5 + z^5 - t^5 = 0.5;


# 
# 2*x^2 - 2*y^2 + 2*z^2 - 2*t^2 = 1,
# 2*x^3 - 2*y^3 + 2*z^3 - 2*t^3 = 1,
# 2*x^4 - 2*y^4 + 2*z^4 - 2*t^4 = 1,
# 2*x^5 - 2*y^5 + 2*z^5 - 2*t^5 = 1
# 


#solve;
#display t, x, y, z;
