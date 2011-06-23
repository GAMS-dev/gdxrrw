$TITLE Test LS (Least Squares) utility - lower difficulty (LS01,SEQ=394)

$ontext

Run the LS solver on the Norris test problem (Level of Difficulty:
Lower) from NIST and verify that the results are those certified to
be correct.  The problem and results were both taken from the NIST Web
site:

  http://www.itl.nist.gov/div898/strd/lls/data/Norris.shtml

Contributor: Erwin Kalvelagen and Steve Dirkse, July 2008.

$offtext


$ontext
Model: y = beta(0) + beta(1)x + error

Certified Regression Statistics
                                             Standard Deviation
Parameter        Estimate                     of Estimate
beta(0)         -0.262323073774029           0.232818234301152
beta(1)          1.00211681802045            0.429796848199937E-03

Residual
Standard Deviation      0.884796396144373
R-Squared               0.999993745883712

Certified Analysis of Variance Table

Source of
Variation       Degrees     Sums of Squares     Mean Squares        F Statistic
                of Freedom
Regression      1           4255954.13232369    4255954.13232369    5436385.54079785
Residual        34          26.6173985294224    0.782864662630069

$offtext

set i 'cases' /i1*i36/;

table data(i,*)
            y          x
  i1       0.1        0.2
  i2     338.8      337.4
  i3     118.1      118.2
  i4     888.0      884.6
  i5       9.2       10.1
  i6     228.1      226.5
  i7     668.5      666.3
  i8     998.5      996.3
  i9     449.1      448.6
  i10    778.9      777.0
  i11    559.2      558.2
  i12      0.3        0.4
  i13      0.1        0.6
  i14    778.1      775.5
  i15    668.8      666.9
  i16    339.3      338.0
  i17    448.9      447.5
  i18     10.8       11.6
  i19    557.7      556.0
  i20    228.3      228.1
  i21    998.0      995.8
  i22    888.8      887.6
  i23    119.6      120.2
  i24      0.3        0.3
  i25      0.6        0.3
  i26    557.6      556.8
  i27    339.3      339.1
  i28    888.0      887.2
  i29    998.5      999.0
  i30    778.9      779.0
  i31     10.2       11.1
  i32    117.6      118.3
  i33    228.9      229.2
  i34    668.4      669.1
  i35    449.2      448.9
  i36      0.2        0.5
;

set p  'coefficients' /
  b0   'beta(0)',
  b1   'beta(1)'
/;

parameter  x(p) 'estimated coefficients';
execute_unload 'ls01', i, p, data;

$exit

* The values with _ appended are the certified values:
* we test that we reproduce these

Scalars
  d
  tol             / 1e-6 /
  sigma_  'Standard error'     / 0.884796396144373 /
  sigma   'Standard error'
  r2_ 'R-Squared' / 0.999993745883712 /
  r2  'R-Squared'
  df_  'Degrees of freedom' / 34 /
  df   'Degrees of freedom'
  rss_ 'Residual sum of squares' / 26.6173985294224 /
  rss  'Residual sum of squares'
  resvar_  'Residual variance' / 0.782864662630069 /
  resvar   'Residual variance'
  ;

parameters
  estimate_(p) 'Estimated coefficients' /
   'b0' -0.262323073774029
   'b1'  1.00211681802045
  /
  estimate(p) 'Estimated coefficients'
  se_(p)  'Standard errors' /
   'b0' 0.232818234301152
   'b1' 0.429796848199937E-03
  /
  se(p)  'Standard errors'

  tval_(p)  't values' /
    'b0' -1.12672907498579,
    'b1' 2331.60578589043
  /
  tval(p)  't values'
  pval_(p)  'p values' /
    'b0' 0.267746835947037
    'b1' 0
  /
  pval(p)  'p values'
  ;


d = smax{p, abs(x.l(p)-estimate_(p))};
abort$[d > tol] 'bad solution x.l', x.l, estimate_, d;

execute_load 'ls', estimate, se, sigma, r2, df, rss, resvar, tval, pval;
abort$[execerror > 0] 'Could not load statistics from GDX';

d = abs(sigma_ - sigma);
abort$[d > tol] 'bad standard error', sigma_, sigma, d;

d = abs(r2_ - r2);
abort$[d > tol] 'bad R-Squared', r2_, r2, d;

d = abs(df_ - df);
abort$[d > tol] 'bad degrees of freedom', df_, df, d;

d = abs(rss_ - rss);
abort$[d > tol] 'bad residual sum of squares', rss_, rss, d;

d = abs(resvar_ - resvar);
abort$[d > tol] 'bad residual variance', resvar_, resvar, d;

d = smax{p, abs(estimate_(p) - estimate(p))};
abort$[d > tol] 'bad estimate', estimate_, estimate, d;

d = smax{p, abs(se_(p) - se(p))};
abort$[d > tol] 'bad standard error', se_, se, d;

d = smax{p, abs(tval_(p) - tval(p))};
abort$[d > tol] 'bad t values', tval_, tval, d;

d = smax{p, abs(pval_(p) - pval(p))};
abort$[d > tol] 'bad p values', pval_, pval, d;
