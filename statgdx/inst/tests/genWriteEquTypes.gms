$ontext
GAMS code to generate GDX "target" for wgdx test of equation types:
GMS_EQUTYPE <- list(E=0L,
                    G=1L,
                    L=2L,
                    N=3L,
                    X=4L,
                    C=5L,
                    MAX=6L)
$offtext

sets
  i / i1 * i10 /
  ii(i) / i1 * i5 /
  ;
equation
  equ_e_0(i)    '=e= equation'
  equ_g_1(i)    '=g= equation'
  equ_l_2(i)    '=l= equation'
  equ_n_3(i)    '=n= equation'
  o
  ;
variables
  x(i)
  y(i)
  z
  ;

o            .. sum{ii, power(-1,ord(ii)) * x(ii)} =e= z;
equ_e_0(ii)  .. x(ii) =e= y(ii);
equ_g_1(ii)  .. x(ii) =g= 2;
equ_l_2(ii)  .. y(ii) =l= 8;
equ_n_3(ii)  .. sqr(x(ii)) =n= y(ii);

model m / all /;
solve m using nlp min z;
execute_unload 'tEquTypes';
