import matplotlib.pyplot as plt


class Gambler:
    def __init__(self, prob=0.4, maximum=100):
        self.prob = prob
        self.maximum = maximum
        self.values = [0]*self.maximum + [1]

    def evaluate_state(self, state):
        N = min([state, self.maximum - state]) + 1
        actions = [i for i in range(N)]
        action_returns = []
        for action in actions:
            action_returns.append(self.prob*self.values[state + action] + (1 - self.prob)*self.values[state - action])
        return action_returns

    def value_iteration(self, theta=1e-8):
        delta = theta+1
        while delta > theta:
            delta = 0
            for state in range(1, self.maximum+1):
                v = self.values[state]
                self.values[state] = max(self.evaluate_state(state))
                delta = max([delta, abs(v - self.values[state])])
        pass

    def get_policy(self):
        policy = [0]*(self.maximum+1)
        for state in range(1, self.maximum):
            eval = self.evaluate_state(state)[1:]
            eval = [round(x, 5) for x in eval]
            idx = eval.index(max(eval)) + 1
            policy[state] = idx
        return policy


G = Gambler(prob=0.4)
G.value_iteration(theta=1e-10)

plt.plot(G.get_policy())

G.evaluate_state(5)