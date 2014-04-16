import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(-10, 10, 1000)
y1 = x * np.cos(np.pi * x)
y2 = 1 - 0.6 * (x * x)

plt.figure(1)
plt.clf()
plt.plot(x, y1, 'b-')
plt.plot(x, y2, 'r-')
plt.savefig('intersections.png')
