$ontext
GAMS code to generate GDX "target" for wgdx test of 20-dim variable
$offtext

$setenv GDXCOMPRESS 1

sets
  d1 / d1_1, d1_2 /
  d2 / d2_1, d2_2 /
  d3 / d3_1, d3_2 /
  d4 / d4_1, d4_2 /
  d5 / d5_1, d5_2 /
  d6 / d6_1, d6_2 /
  d7 / d7_1, d7_2 /
  d8 / d8_1, d8_2 /
  d9 / d9_1, d9_2 /
  da / da_1, da_2 /
  db / db_1, db_2 /
  dc / dc_1, dc_2 /
  dd / dd_1, dd_2 /
  de / de_1, de_2 /
  df / df_1, df_2 /
  dg / dg_1, dg_2 /
  dh / dh_1, dh_2 /
  di / di_1, di_2 /
  dj / dj_1, dj_2 /
  dk / dk_1, dk_2 /
  ;
free variable big(d1,d2,d3,d4,d5,d6,d7,d8,d9,da,
                  db,dc,dd,de,df,dg,dh,di,dj,dk) '20-dim var';

big.L(d1,d2,d3,d4,d5,d6,d7,d8,d9,da,
      db,dc,dd,de,df,dg,dh,di,dj,dk) = 0.75;

* change in d2
big.l    ('d1_1','d2_2','d3_1','d4_1','d5_1','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0;
big.m    ('d1_1','d2_2','d3_1','d4_1','d5_1','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0.5;

* change in d3
big.l    ('d1_1','d2_1','d3_2','d4_1','d5_1','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0;
big.lo   ('d1_1','d2_1','d3_2','d4_1','d5_1','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0;

* change in d4
big.l    ('d1_1','d2_1','d3_1','d4_2','d5_1','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0;
big.up   ('d1_1','d2_1','d3_1','d4_2','d5_1','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0;

* change in d5
big.l    ('d1_1','d2_1','d3_1','d4_1','d5_2','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 0;
big.scale('d1_1','d2_1','d3_1','d4_1','d5_2','d6_1','d7_1','d8_1','d9_1','da_1',
          'db_1','dc_1','dd_1','de_1','df_1','dg_1','dh_1','di_1','dj_1','dk_1') = 100;

execute_unload 'tVar20';

scalar c;
c = card(big);
file log / '' /;
putclose log / 'Card(big) = ', c /;
