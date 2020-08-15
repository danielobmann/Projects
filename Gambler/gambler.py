import matplotlib.pyplot as plt


class Gambler:
    def __init__(self, prob=0.5, maximum=100):
        self.prob = prob
        self.maximum = maximum
        self.GAMMA = 0.9
        self.values = [0]*(self.maximum + 1)
        self.policy = [0]*(self.maximum + 1)

    def evaluate_state(self, state):
        actions = [0]*(min([state, self.maximum - state]) + 1)
        for action in range(len(actions)):
            r = (1 if (state + action) == self.maximum else 0)
            actions[action] = self.prob*(r + self.GAMMA*self.values[state + action]) + (1 - self.prob)*self.GAMMA*self.values[state - action]
        return actions

    def value_iteration(self, theta=1e-8):
        delta = theta+1
        while delta > theta:
            delta = 0
            for state in range(self.maximum+1):
                v = self.values[state]
                self.values[state] = max(self.evaluate_state(state))
                delta = max([delta, abs(v - self.values[state])])
        self.update_policy()
        pass

    def update_policy(self):
        for state in range(self.maximum+1):
            eval = self.evaluate_state(state)
            self.policy[state] = eval.index(max(eval))
        pass


G = Gambler(prob=0.8)
G.value_iteration(theta=1e-10)

G.policy
G.values

plt.plot(G.policy)