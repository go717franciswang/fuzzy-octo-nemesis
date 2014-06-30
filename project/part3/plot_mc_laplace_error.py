from pylab import *

n, approx, error = loadtxt('laplace_mc_error.txt', unpack=True)

figure(1)
clf()
loglog(n, error, '-o', label='Monte-Carlo')
loglog([1,1e6], [1, sqrt(1e-6)], 'k', label='1 / sqrt(N)')
legend()
xlabel('number of MC random walks used')
ylabel('abs(error)')
title('Log-log plot of relative error in MC random walk')
savefig('laplace_mc_error.png')
