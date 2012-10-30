sets
  i  / i1 * i2 /
  j  / j1 * j2 /
  k  / k1 * k2 /
  ;
positive variable x(i,j,k);

x.l(i,j,k) = 100 * (ord(i)-1) + 10 * (ord(j)-1) + (ord(k)-1);

x.lo('i1','j2','k1') = -INF;
x.lo('i1','j2','k2') = 100;

x.up('i1','j1','k1') = 525;
x.up('i2','j1','k1') = 0;

x.fx('i2','j2','k2') = 6;

* x.l('i2','j1','k2') = 20;

x.m('i1',j,'k2') = .25;

x.scale("i2","j2","k1") = 10;

execute_unload 'tReadVar3';
