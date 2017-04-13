# Domains
var x >= -1000, <= 1000;
var y >= -1000, <= 1000;
var z >= -1000, <= 1000;
var a >= -1000, <= 1000;
var b >= -1000, <= 1000;
var l >= -1000, <= 1000;
var p >= -1000, <= 1000;


subject to
cons1 : a^2 + y^2 = p^2;
cons2 : b^2 + z^2 = l^2;
cons3 : a^2 - l^2 + 60*l = 800;
cons4 : b^2 - p^2 + 40*p = 300;
cons5 : y*(y + 20) + x^2 = 300;
cons6 : z*(z + 20) + x^2 = 800;
cons7 : x = a + b;
cons8 : 30*b = l*x;
cons9 : 20*a = p*x;
cons10 : p*(10+y) = 20*y;
cons11 : l*(10+z) = 30*z;

cons12 : 10*p + y*(p-20) = 0;
cons13 : 10*l + z*(l-30) = 0;


#solve;
#display a, b, l, p, x, y, z;
