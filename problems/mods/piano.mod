# Domains
var x >= -100000, <= 100000;
var y >= -100000, <= 100000;
var l >= -100000, <= 100000;
var t >= -100000, <= 100000;
var L >= -100000, <= 100000;
var b >= -100000, <= 100000;
var r >= -100000, <= 100000;
var a >= -100000, <= 100000;
var w >= 0, <= 100000;


subject to
cons1 : x - l*t^3 - L*w = 0;
cons2 : y - L*t - l*w^3 = 0;
cons3 : L - 1           = 0;
cons4 : l - 2           = 0;
cons5 : x - a           = 0;
cons6 : 2*a - 3         = 0;
cons7 : y - b           = 0;
cons8 : b - r*t         = 0;
cons9 : w^2 - 1 + t^2   = 0;

#solve;
#display L, a, b, l, r, t, w, x, y;
