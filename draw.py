import matplotlib.pyplot as plt
import numpy as np

import stats as s
import brutestats as b

fig, ax = plt.subplots()

# ax.plot(np.array(s.sizes), (10**3) * np.array(s.timings))
# plt.show()

ax.plot(np.array(b.sizes), (10**3)*np.array(b.timings))
plt.show()
