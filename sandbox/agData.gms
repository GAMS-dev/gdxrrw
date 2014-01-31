sets
  r 'regions' /
    usa
    eu
  /
  c 'commodities' /
    crn  ''
    mlk  'raw milk'
    wht
  /
  rc(r,c) 'not a cola' /
    usa.mlk "America's Dairyland"
    eu.wht
  /
  jjjj(r) / eu /
  ;

$ontext
parameters
  exp(c,r) 'exports' /
    crn.usa    6
    crn.eu     5
    wht.usa    8
    wht.eu     4
  /
  expTotal(c) 'worldwide export totals'
  ;
$offtext
parameter expTotal(c) 'worldwide export totals';

table exp(c,r) 'exports'
        usa     eu
crn      6      5
mlk      0      0
wht      8      4 ;
    
expTotal(c) = sum {r, exp(c,r)};

execute_unload 'agData';
