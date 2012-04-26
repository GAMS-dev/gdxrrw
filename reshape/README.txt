One weak spot in gdxrrw is the lack of support for data frames that
result from calls to read.csv.  For example, the data file sample1.dat
looks like this:

comm	region	y2010	y2011	y2012	y2013	y2014
wht	usa	1.1	1.11	1.12	1.13	1.14
wht	can	2.1	2.11	2.12	2.13	2.14
wht	rus	3.1	3.11	3.12	3.13	3.14
crn	usa	5.1	5.11	5.12	5.13	5.14
crn	can	6.1	6.11	6.12	6.13	6.14
crn	rus	7.1	7.11	7.12	7.13	7.14

We would like to conveniently make it look like something ready to
send to wgdx.df as a 3-dim parm prd(comm,region,time) with this
content:

parameter prd(comm, region, time) 'production' /
wht.usa.y2010 1.1
wht.usa.y2011 1.11
... blah blah blah ...
crn.rus.y2013 7.13
crn.rus.y2014 7.14
/;

Optionally, we could make time the first or second index but it would
default to the rightmost one.

Of course, we'll also want to go in the opposite direction:  starting
with the GAMS parameter prd as given above, produce data frames
suitable for output via write.csv with any of the three indices
(commodity, region, or time) on the column.

The idea is to do this at first using a script.  We can stay with the
script or move to doing this in C code if there is reason to do so.

For writing a GDX, experiment with reshaping and writing the data
frame all in one shot, so we can also write the sets, etc.  Would be
great if we could write domain information for all GDX files we
create.
