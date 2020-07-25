import numpy as np
import matplotlib.pyplot as plt
from itertools import product


class Maze:
    def __init__(self):
        self.GAMMA = 0.9
        self.ACTIONS = np.array([[-1, 0], [1, 0], [0, -1], [0, 1]]) # Actions consist of up, down, left, right
        self.STATES = [np.array(s) for s in product([0, 1, 2, 3, 4], repeat=2)]

        self.maze = self.initialize_maze()
        self.s0 = 0
        self.goal = 24
        self.policy_table = np.ones((4, 25))/4
        self.state_value = np.zeros(25)

        '''Actions are encoded using 0, 1, 2 and 3 and states are encoded using 0,...,24.
        There corresponding action/state in 2D can be obtained using ACTIONS and STATES.'''

    def get_new_state(self, state, action):
        return self.STATES[state] + self.ACTIONS[action]

    def get_action_space(self, state):
        '''Definiere welche Aktionen gemacht werden können:
        up, down, left, right'''
        s = self.STATES[state]
        return np.array([(s[0] > 0), (s[0] < 4), (s[1] > 0), (s[1] < 4)])

    def transition_prob(self, state_prime, state, action):
        if (self.get_new_state(state, action) == self.STATES[state_prime]).all():
            return 1
        return 0

    def reward(self, state, action):
        '''Evaluate reward the agent gets for taking action at state.
        Here the goal is located at [4, 4].'''
        if (self.get_new_state(state, action) == self.STATES[self.goal]).all():
            return 10
        else:
            return -0.5
        pass

    def policy(self, state):
        '''Among all possible actions take the one with maximal probability;
        Greedy strategy for choosing the action based on state.'''
        possible_actions = self.get_action_space(state)
        best_action = np.argmax(self.policy_table[..., state] * possible_actions)
        return best_action

    def get_action_reward(self, state, action):
        total = 0
        for s in range(len(self.STATES)):
            vs = self.state_value[s]
            prob = self.transition_prob(s, state, action)
            r = self.reward(state, action)
            total += prob*(r + self.GAMMA*vs)
        return total

    #############################
    def prediction(self, state):
        '''Calculate the prediction'''
        action_space = self.get_action_space(state)
        total = 0
        A = np.where(action_space == True)[0]
        for action in A:
            total += self.get_action_reward(state, action)*self.policy_table[action, state]
        return total

    def policy_evaluation(self, theta=1e-3):
        delta = theta + 1
        while delta >= theta:
            delta = 0
            for s in range(len(self.STATES)):
                v = self.state_value[s]
                self.state_value[s] = self.prediction(state=s)
                delta = max(delta, np.abs(v - self.state_value[s]))
        return True

    def policy_iteration(self, theta=1e-3):
        stable = False
        while not stable:
            self.policy_evaluation(theta=theta)
            stable = True
            for s in range(len(self.STATES)):
                a = self.policy(s)
                self.policy_table[a, s] = np.argmax([self.get_action_reward(s, a) for a in range(len(self.ACTIONS))])
                if a != self.policy_table[a, s]:
                    stable = False
        return True

    def initialize_maze(self):
        maze = np.zeros((5, 5))
        return maze




M = Maze()

M.policy_evaluation()

plt.imshow(M.state_value.reshape((5, 5)))
