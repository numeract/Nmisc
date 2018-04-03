# Unless needed, add only one example for each warning / error.

# known bug: warning regardless of how many spaces for missing columns
# Commas should never have a space before.
df[rows, , drop = FALSE]
# Commas should always have a space after.
df[rows,, drop = FALSE]

# Use <-, not =, for assigment.
a = 3

# Put space arround all infix operators.
a<-3
