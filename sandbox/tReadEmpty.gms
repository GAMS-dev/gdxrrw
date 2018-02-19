sets
  d1 / 'orange', 'pear', 'banana' /
  d2 / 'VA', 'MD' /
  d3 / '2018', '2019' /
  bb 'empty set' / system.empty /
  basket(d1,d2,d3)
  ;
parameters
  e1(d1) 'empty parameter' / orange 0 /
  e3(d1,d2,d3) 'empty parameter' / orange.VA.2018 0 /
  ;
execute_unload 'tReadEmpty';

