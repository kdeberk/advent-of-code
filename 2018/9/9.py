
class Node(object):
    @classmethod
    def initial(cls):
        node = cls(0)
        node.next = node
        node.prev = node
        return node
    
    def __init__(self, value):
        self.value = value
        self.next = None
        self.prev = None


def part_1(n_players, highest_marble):
    scores = [0] * n_players

    current = Node.initial()
    for marble in range(1, highest_marble + 1):
        if 0 != marble % 23:
            current = current.next

            new_node = Node(marble)
            new_node.next = current.next
            new_node.prev = current
            current.next.prev = new_node
            current.next = new_node
            current = new_node
        else:
            for _ in range(0, 7):
                current = current.prev
            current.prev.next = current.next
            current.next.prev = current.prev

            scores[(marble - 1) % n_players] += marble
            scores[(marble - 1) % n_players] += current.value
            current = current.next

    print(max(scores))

part_1(429, 70901)
part_1(429, 70901 * 100)
