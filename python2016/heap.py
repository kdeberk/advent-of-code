class MinHeap:
    def __init__(self):
        self.items = []

    def any(self):
        return 0 < len(self.items)

    def push(self, el, score):
        self.items.append((score, el))
        idx = len(self.items)-1
        while 0 < idx:
            pIdx = idx//2
            if self.items[pIdx][0] < self.items[idx][0]:
                return
            self.swap(idx, pIdx)
            idx = pIdx

    def pop(self): # TODO: this heap impl is very wrong
        self.swap(0, len(self.items)-1)

        idx = 0
        while idx < len(self.items):
            cIdx = 2*idx + 1
            if len(self.items) <= cIdx:
                break
            if self.items[cIdx][0] < self.items[idx][0]:
                self.swap(idx, cIdx)
                idx = cIdx
                continue

            cIdx = 2*idx + 2
            if len(self.items) <= cIdx:
                break
            if self.items[cIdx][0] < self.items[idx][0]:
                self.swap(idx, cIdx)
                idx = cIdx
                continue

            break
        return self.items.pop()[1]

    def peek(self):
        return self.items[0][1]

    def swap(self, a, b):
        self.items[a], self.items[b] = self.items[b], self.items[a]
