import numpy as np
import matplotlib.pyplot as plt
from itertools import product

class Maze:
    def __init__(self, n=5):
        self.n = n
        self.GAMMA = 0.9
        self.ACTIONS = np.array([[-1, 0], [1, 0], [0, -1], [0, 1]]) # Actions consist of up, down, left, right, stay
        self.STATES = [s for s in product(range(n), repeat=2)]

        self.maze = np.zeros((n, n))
        self.s0 = 0
        self.goal = n*n-1
        self.policy_table = np.random.choice([0, 1, 2, 3], n**2)
        self.state_value = np.zeros(n*n)

        '''Actions are encoded using 0, 1, 2 and 3 and states are encoded using 0,...,24.
        There corresponding action/state in 2D can be obtained using ACTIONS and STATES.'''

    def get_new_state(self, s, a):
        sprime = tuple(np.array(self.STATES[s]) + self.ACTIONS[a])
        try:
            idx = self.STATES.index(sprime)
        except ValueError:
            idx = -1
        return idx

    def transition_prob(self, sprime, s, a):
        if sprime == self.get_new_state(s, a):
            return 1
        return 0

    def reward(self, s):
        if s == self.goal:
            return 10
        return -0.5

    def action_reward(self, s, a):
        total = 0
        for sprime in range(self.n**2):
            prob = self.transition_prob(sprime, s, a)
            r = self.reward(sprime)
            vs = self.state_value[sprime]
            total += prob*(r + self.GAMMA*vs)
        return total

    def policy_evaluation(self, theta=1e-3):
        delta = theta + 1
        while delta >= theta:
            delta = 0
            for s in range(self.n**2):
                v = self.state_value[s]
                self.state_value[s] = self.action_reward(s, self.policy_table[s])
                delta = max(delta, np.abs(v - self.state_value[s]))
        return True

    def policy_iteration(self, theta=1e-3):
        stable = False
        while not stable:
            self.policy_evaluation(theta=theta)
            stable = True
            for s in range(self.n**2):
                a = self.policy_table[s]
                p = np.argmax([self.action_reward(s, a) for a in range(len(self.ACTIONS))])
                self.policy_table[s] = p
                if a != p:
                    stable = False

            print(self.policy_table, flush=True)
        return True

    def translate(self):
        D = {0: "Up", 1: "Down", 2: "Left", 3: "Right"}
        return [D[self.policy_table[i]] for i in range(self.n**2)]





M = Maze()
M.policy_iteration()


plt.imshow()
plt.colorbar(ticks=[0, 1, 2, 3])


def discrete_matshow(data):
    #get discrete colormap
    cmap = plt.get_cmap('RdBu', np.max(data)-np.min(data)+1)
    mat = plt.matshow(data, cmap=cmap, vmin=np.min(data)-.5, vmax=np.max(data)+.5)
    cbar = plt.colorbar(mat, ticks=np.arange(np.min(data), np.max(data)+1))
    cbar.ax.get_yaxis().set_ticks([])
    for j, lab in enumerate(['Up', 'Down', 'Left', 'Right']):
        cbar.ax.text(1.0, (4*j + 1)/4, lab, ha='center', va='center', rotation=90)


discrete_matshow(M.policy_table.reshape((5,5)))
