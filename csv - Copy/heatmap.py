import numpy as np
import matplotlib.pyplot as plt

data = np.genfromtxt("grid.csv", delimiter=",")
plt.imshow(data, cmap='hot', interpolation='nearest')
plt.show()