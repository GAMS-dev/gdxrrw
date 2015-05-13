$Title  A Transportation Problem Example

$if NOT set INPUT  $set INPUT  inputs.gdx
$if NOT set OUTPUT $set OUTPUT outputs.gdx

SETS
  i   'canning plants'
  j   'markets';

PARAMETERS
  a(i)   'capacity of plant i'
  b(j)   'demand at market j'
  c(i,j) 'transport cost';

$gdxin %INPUT%
$load i j a b c
$gdxin

POSITIVE VARIABLE  x(i,j)  'shipment quantities';
VARIABLE           z       'total transportation costs';

EQUATIONS
   cost        define objective function
   supply(i)   observe supply limit at plant i
   demand(j)   satisfy demand at market j ;

cost ..        z  =e=  sum((i,j), c(i,j)*x(i,j)) ;

supply(i) ..   sum(j, x(i,j))  =l=  a(i) ;

demand(j) ..   sum(i, x(i,j))  =g=  b(j) ;

Model transport /all/ ;

Solve transport using lp minimizing z ;

scalars modelStat, solveStat;
modelStat = transport.modelStat;
solveStat = transport.solveStat;
execute_unload '%OUTPUT%';
