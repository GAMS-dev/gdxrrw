sets
  s 'states to map' /
    California
    Oregon
    Washington
    Nevada
    Utah
    Montana
    Idaho
    Wyoming
    Arizona
    Colorado
   "New Mexico" 
  /
  c 'commodities consumed' / wht, crn, ric /
  ;

parameters
  rPrd(c,s) 'raw production'
  sPrd(c,s) 'scaled production'
  w(s)      'scale factors based on total production'
  t(s)      'tmp'
  ;
scalar m;

rPrd(c,s) = uniform(0.2,2);
rPrd('wht','Wyoming') = 0;
t(s) = uniform(1,5);
t('California') = 5;
rPrd('ric','California') = smax{s, rPrd('ric',s)};
rPrd(c,s) = rPrd(c,s) * t(s);

t(s) = sum {c, rPrd(c,s)};
sPrd(c,s) = rPrd(c,s) / t(s);
m = smax{s, t(s)};
t(s) = t(s) / m;
w(s) = sqrt(t(s));


execute_unload 'stateRes', s, c, w, sPrd;
