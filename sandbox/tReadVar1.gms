sets
  k     / k1 * k4 /
  kk(k) / k1 * k3 /
  ;
integer  variable u(k);
negative variable v(k);

u.up(kk) = 15;
u.fx('k1') = 5;
u.m('k2') = 1.5;

v.up('k1') = INF;
v.fx('k2') = -2;
v.m('k2') = -20;

execute_unload 'tReadVar1';
