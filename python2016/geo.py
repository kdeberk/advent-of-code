
class Point:
    def __init__(self, x, y):
        self.x, self.y = x, y

    def dist(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y)

    def neighbors(self):
        return [Point(self.x+dx, self.y+dy)
                for (dx, dy) in [(0, -1), (0, 1), (1, 0), (-1, 0)]]

    def __eq__(self, o):
        return self.x == o.x and self.y == o.y

    def __hash__(self):
        return hash((self.x, self.y))

    def __str__(self):
        return f'<{self.x}, {self.y}>'

    def __repr__(self):
        return f'<{self.x}, {self.y}>'
