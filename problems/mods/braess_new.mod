# Objective: convex quadratic
# Constraints: linear

set INTERSECTIONS;

param origin      symbolic in INTERSECTIONS;
param destination symbolic in INTERSECTIONS, <> origin;

param demand >= 0;                      # Number of travelers


set ROADS within (INTERSECTIONS diff {destination}) cross 
                 (INTERSECTIONS diff {origin});



param fixed_time {ROADS} >= 0;          # Fixed travel time on a link 
param congestion_factor {ROADS} >= 0;   # Flow-dependent effects


var Flow {(i,j) in ROADS} >= 0;
#var Total_Cost;

param best_val_found := 399;
param eps := 4;		# = max (1, 1% x best_val_found)

subject to BeckmannObj:
     sum {(i,j) in ROADS} ((fixed_time[i,j] * Flow[i,j]) +
                          (1/2 * congestion_factor[i,j] * Flow[i,j]^2)) <= best_val_found + eps;

subject to Leave:
     sum {(origin,j) in ROADS} Flow[origin,j] = demand;


subject to Arrive:
     sum {(i,destination) in ROADS} Flow[i,destination] = demand;


subject to Balance {k in INTERSECTIONS diff {origin,destination}}:
     sum {(i,k) in ROADS} Flow[i,k] = sum {(k,j) in ROADS} Flow[k,j];

/*
printf "\n\n A DEMONSTRATION OF BRAESS' PARADOX \n\n";


printf "Case 1: Before the new road is built open\n\n";

data;
set INTERSECTIONS := a b c d;

param origin      := a;
param destination := d;

param demand := 6;

param: ROADS: fixed_time congestion_factor :=
     a b    50      1,
     a c     0     10,
     b d     0     10,
     c d    50      1;


#solve;

printf "   The total travel time is: %6.2f.\n\n",
     sum {(i,j) in ROADS} (fixed_time[i,j]*Flow[i,j] + 
                           congestion_factor[i,j]*Flow[i,j]^2);

printf "   The flows are:\n";

#display Flow;
*/


/*
printf "\n\n\n";
printf "Case 2: After the new road (c,b) is built\n\n";

reset data;
*/
data;
set INTERSECTIONS := a b c d;

param origin      := a;
param destination := d;

param demand := 6;

param: ROADS: fixed_time congestion_factor :=
     a b    50      1,
     a c     0     10,
     b d     0     10,
     c d    50      1,
     c b    10      1;

#solve;
/*
printf "   The total travel time is: %6.2f.\n\n",
     sum {(i,j) in ROADS} (fixed_time[i,j]*Flow[i,j] + 
                           congestion_factor[i,j]*Flow[i,j]^2);

printf "   The flows are:\n";
*/
#display Flow;

