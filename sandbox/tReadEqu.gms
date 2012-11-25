$ontext
A simple model to generate a GDX file with some equations that we can
use to test gdxrrw.
$offtext

sets
  I / i1 /
  J / j1 * j2 /
  K / k1 /
  ;
positive variable u;
free variables v, z;
equations
  e0         'Objective Definition'
  e1(K)      '1-dim equation'
  e3(I,J,K)  '3-dim equation'
  ;

e0.. z =E= sqr(u) + v;
e1(K) .. 2 =G= u;
e3(I,J,K) .. u + ord(J)*v =L= 4;

model m / all /;

u.l = 1;
e1.scale(K) = 2;
e1.m(K) = 3.5;
e3.m(i,'j2',k) = 0.5;
u.l = 2;
v.l = 1;
z.l = 5;
option nlp = pathnlp;
solve m using nlp max z;
