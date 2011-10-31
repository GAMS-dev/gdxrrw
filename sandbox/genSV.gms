set I / 'dummy', 'dbl', 'tiny'
        'sv_eps'
        'sv_undef', 'sv_na'
        'sv_pinf', 'sv_ninf' /;

alias(I,J);

scalars dt / 1e-20 /;
scalar tiny;
tiny = dt * dt;
tiny = tiny * dt;
tiny = tiny * dt;
tiny = tiny * dt;
* 1e-100
tiny = tiny * dt;
tiny = tiny * dt;
tiny = tiny * dt;
tiny = tiny * dt;
tiny = tiny * dt;
* 1e-200
tiny = tiny * dt;
* tiny = tiny * dt;
* tiny = tiny * dt;
* tiny = tiny * dt;

parameter sv(I,I);

sv(I,'dummy') = 1;
sv('dummy',I) = 1;
sv('dbl','dbl') = pi;
sv('tiny','tiny') = tiny;
sv('sv_eps','sv_eps') = eps;
sv('sv_undef','sv_undef') = 0/0;
sv('sv_na','sv_na') = NA;
sv('sv_pinf','sv_pinf') = +inf;
sv('sv_ninf','sv_ninf') = -inf;

execerror = 0;

execute_unload 'svGAMS', I, sv;
