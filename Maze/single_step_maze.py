import numpy as np
import imageio


class Maze:
    '''The maze instance is eht environment on which the agent acts.
    It is able to give the current state, give reward based on an action taken
    and keeps track of where the agent was and is.'''
    def __init__(self, size=5):
        self.size = size
        self.maze, self.position_x, self.position_y = self.get_initial_position()
        self.finished = False
        self._history = {'maze': [self.maze], 'position': [[self.position_x, self.position_y]]}

    def get_initial_position(self):
        maze = np.zeros((self.size, self.size))
        i0, j0 = np.random.choice(self.size), np.random.choice(self.size)
        maze[i0, j0] = -1
        return maze, i0, j0

    def take_action(self, action):
        self.position_x = (self.position_x + action[0]) % self.size
        self.position_y = (self.position_y + action[1]) % self.size
        reward = self.maze[self.position_x, self.position_y]
        self.maze[self.position_x, self.position_y] -= 1
        self._history['maze'].append(self.maze)
        self._history['position'].append([self.position_x, self.position_y])

        if not (self.maze == 0).any():
            reward = 5
            self.finished = True

        return reward

    def get_current_state(self):
        return self.maze, self.position_x, self.position_y

    def plot_solution(self, path):
        imageio.mimsave(path, self._history['maze'], fps=1)


class Agent:
    '''The agent acts in the maze defined above.
    Its goal is to visit each space exactly once.'''
    def __init__(self):
        self.actions = [[1, 0], [0, 1], [-1, 0], [0, -1]]

    def get_action(self, state, position_x, position_y):
        # Select action based on state and position
        i = np.random.choice(len(self.actions))
        return self.actions[i]


size = 2
M = Maze(size=size)
A = Agent()

while not M.finished:
    state, position_x, position_y = M.get_current_state()
    action = A.get_action(state, position_x, position_y)
    M.take_action(action=action)


M.plot_solution("Maze/test.gif")