# **************************************************************************
# File: Czapor
# Name: 
# 
# Reference :
# 
# @inproceedings{           monfroy-gb,
#    author       = "Eric Monfroy",
#    title        = "{G}r{\"{o}}bner {B}ases: {S}trategies and {A}pplications",
#    booktitle    = "Proceedings of AISMC'92",
#    publisher    = "Springer-Verlag",
#    series       = "LNCS",
#    year         = 1992,
#    volume       = 737,
#    pages        = "133--151"
# }
# 
# ***************************************************************************

# Domains
var x >= -1000000000, <= 1000000000;
var y >= -1000000000, <= 1000000000;
var z >= -1000000000, <= 1000000000;

#subject to

cons1 : z*x^2 - 0.5*x = y^2;
cons2 : z*y^2 + 2*x + 0.5 = 0;
cons3 : z - 16*x^2 - 4*x*y^2 = 1;

# 
# , z^7 - 2*z^6 + z^5 + 13*z^4 + 9*z^3 - 102*z^2 + 170*z - 60 = 0,
# 
# y^2 - (1071/3092)*z^6 + (1623/3092)*z^5 - (291/3092)*z^4 - (7045/1546)*z^3 -
# (4153/773)*z^2 + (101337/3092)*z - (66647/1546) = 0
# 


#solve;
#display x, y, z;
