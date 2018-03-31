# simulated trends

x=1:100

# simple sine, +/-50, mean 100
ss = sin(x*pi/12)*50 + 100 
plot( ss )

# simple sine, addititive trend
ssat = sin(x*pi/12)*50 + 100 + x 
plot( ssat )

# simple sine, multiplicative trend
ssmt = (sin(x*pi/12)*50 + 100)*(1+.01*x)
plot( ssmt )

# Arima


