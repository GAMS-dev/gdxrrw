$ontext
GAMS code to generate GDX input for option(gdx.domainNames) test
$offtext

sets
  s / s1 * s4 /
  t / t1 * t3 /
  ;
alias(s,ss);
alias(u,*);
set p(s,ss) 'identity s -> ss';
p(s,ss) = sameas(s,ss);
parameters
  a(t,s)  / t1.s4 3.14159 /
  b(ss,u) / s2.s2 1.5 /
  ;

