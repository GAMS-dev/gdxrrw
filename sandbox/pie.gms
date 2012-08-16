sets
  i / i1 * i3 /
  j / j1 * j3 /
  k / k1 * k9 /
  ijk(i,j,k)  'map i-j space to singly-indexed'
  c           'commodities consumed'           / wht, crn, ric /
  ;
ijk(i,j,k) = [ord(k) eq (ord(i) + (ord(j)-1)*card(i))];

parameters
  x0(i,j)   'x coordinates'
  x(k)      'x coordinates'
  y0(i,j)   'y coordinates'
  y(k)      'y coordinates'
  rPrd(c,i,j) 'raw production at each node i-j'
  sPrd(c,k)   'scaled production'
  w(k)      'scale factors based on total production'
  tij(i,j), tk(k)
  ;
scalar m;

rPrd(c,i,j) = uniform(0.2,2);
tij(i,j) = uniform(1,5);
rPrd(c,i,j) = rPrd(c,i,j) * tij(i,j);
x0(i,j) = ord(i);
y0(i,j) = ord(j);
x(k) = sum {ijk(i,j,k), x0(i,j)};
y(k) = sum {ijk(i,j,k), y0(i,j)};

sPrd(c,k) = sum {ijk(i,j,k), rPrd(c,i,j)};
tk(k) = sum {c, sPrd(c,k)};
sPrd(c,k) = sPrd(c,k) / tk(k);
m = smax{k, tk(k)};
tk(k) = tk(k) / m;
w(k) = sqrt(tk(k));


execute_unload 'results', k, c, x, y, w, sPrd;
