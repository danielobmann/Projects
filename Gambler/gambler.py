import matplotlib.pyplot as plt
import numpy as np


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

    def play_game(self, s0, policy):
        hist = [s0]
        while s0 < self.maximum and s0 > 0:

            bet = policy[s0]
            coin = np.random.choice(2, p=[self.prob, 1 - self.prob])

            if coin == 0:
                s0 += bet
            else:
                s0 -= bet
            hist.append(s0)

        if s0 == self.maximum:
            outcome = 1
        else:
            outcome = 0

        return outcome, hist




G = Gambler(prob=0.4)
G.value_iteration(theta=1e-10)

plt.plot(G.get_policy(), '.')
plt.savefig("Gambler/policy.pdf", format='pdf')

G.evaluate_state(5)

G.play_game(40, G.get_policy())

N = 10000
policy = G.get_policy()

averages = []

for budget in range(1, 100):
    outcomes = []
    for _ in range(N):
        s0 = budget
        out, _ = G.play_game(s0, policy=policy)
        outcomes.append(out)

    averages.append(np.mean(outcomes))

print(averages)

plt.plot(averages, label="Win probability")
plt.plot(G.values, label="Values of each state")
plt.legend()
plt.savefig("Gambler/comparison.pdf", format='pdf')


