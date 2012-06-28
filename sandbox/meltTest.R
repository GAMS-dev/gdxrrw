# how does melt() work?

# start with the data in toMelt.dat
df <- read.delim ("toMelt.dat")
str(df)
# note that:
#  1. column A is clearly index data: character data converted to a
#     factor by read.delim
#  2. columns D and E are doubles: probably data columns, if not we
#     should force them to be read as character data
#  3. columns B & C are integers: they could go either way

# by default, melt will choose id={factor or character cols}.
# Numeric (e.g. double or integer) cols are assumed to be data.  In
# this case, we'll get a 2-dim parameter result (i.e. a DF output with
# two factor variables/columns)

outDefault <- melt(df)
str(outDefault)

# we can convert columns B and/or C to factors, in which case melt
# will treat them as index columns
df2 <- df
df2[[2]] <- as.factor(df[[2]])
df2[[3]] <- as.factor(df[[3]])
str(df2)
out2Default <- melt(df2)
str(out2Default)

# we can get the same results by specifying the id variables
# explicitly
outExp <- melt(df,id=c(1:3))
str(outExp)

# we don't have to take the left-most columns as indices
out13 <- melt(df,id=c(1,3))
str(out13)

# what about taking them out of order?
out31 <- melt(df,id=c(3,1))
str(out31)
