


# Domains
var x >= -1000, <= 1000;
var y >= -1000, <= 1000;
var z >= -1000, <= 1000;

subject to
cons1 : x^2 + y^2 + z^2 = 1;
cons2 : x^2 + z^2 = y;
cons3 : x = z;



# 
# x = z,
# z^2 = 0.5*y,
# y^2 + y = 1
# 

#solve;
#display x, y, z;
