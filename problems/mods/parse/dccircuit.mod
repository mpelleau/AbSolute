
# Domains
var Is >= -100, <= 100;
var I1 >= -100, <= 100;
var I2 >= -100, <= 100;
var I3 >= -100, <= 100;
var I4 >= -100, <= 100;
var I5 >= -100, <= 100;
var I6 >= -100, <= 100;
var I7 >= -100, <= 100;
var I8 >= -100, <= 100;
var I9 >= -100, <= 100;

subject to
cons1 : Is - I1 - I2 - I8 = 0;
cons2 : -Is + I1 + I7 = 0;
cons3 : I2 + I3 - I5 = 0;
cons4 : -I3 - I4 + I8 - I9 = 0;
cons5 : I4 + I6 - I7 = 0;
cons6 : I5 - I6 + I9 = 0;
cons7 : I1 = 10;
cons8 : 2*I2 - 3*I3 - 8*I8 = 0;
cons9 : 3*I3 + 5*I5 - 9*I9 = 0;
cons10 : -4*I4 + 6*I6 + 9*I9 = 0;
cons11 : -I1 + 4*I4 + 7*I7 + 8*I8 = 0;

#solve;
#display I1, I2, I3, I4, I5, I6, I7, I8, I9, Is;
