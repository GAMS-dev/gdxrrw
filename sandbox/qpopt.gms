$onempty
$include matglobs.gms

$if %ncstr% == 0 $goto unconstr
set i /1*%ncstr%/,
    ieq(i),
    ilt(i),
    j /1*%nvars%/;

ieq(i) = yes$(ord(i) le %neqcstr%);
ilt(i) = not ieq(i);
$goto probdef

$label unconstr
set i / dummy /,
    ieq(i) //,
    ilt(i) //,
    j /1*%nvars%/;

$label probdef
parameter H(j,j) //,
          f(j) //,
	  A(i,j) //,
	  b(i) //,
	  vlb(j) //,
	  vub(j) //,
	  x0(j) //;

$include matdata.gms

alias(j1,j);

variables x(j), obj;
equations cost, equality(i), leineq(i);

cost..
  obj =e= 0.5*sum(j,x(j)*sum(j1,H(j,j1)*x(j1)))
	     + sum(j,f(j)*x(j));

equality(ieq)..
  sum(j,A(ieq,j)*x(j)) =e= b(ieq);

leineq(ilt)..
  sum(j,A(ilt,j)*x(j)) =l= b(ilt);

x.lo(j) = vlb(j);
x.up(j) = vub(j);
x.l(j) = x0(j);

model qp /all/;
solve qp using nlp minimizing obj;

parameter lambda(i);
lambda(ieq) = equality.m(ieq);
lambda(ilt) = leineq.m(ilt);

$libinclude matout x.l j
$libinclude matout lambda i
