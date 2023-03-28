from hashlib import md5 as rawMD5

class MD5Generator(object):
    # List value [] is default so that all instances of Generator will share it.
    def __init__(self, prefix, fn, rep=1, l=[]):
        self.prefix = prefix
        self.fn = fn
        self.rep = rep

        self.n = 0

        self.idx = 0
        self.l = l

    def next(self):
        if self.idx < len(self.l):
            self.idx += 1
            return self.l[self.idx-1], 0 # TODO: fix n

        while True:
            h = md5(self.prefix + str(self.n))
            for _ in range(0, self.rep-1):
                h = md5(h)

            self.n += 1

            if self.fn(h):
                self.idx += 1
                self.l.append(h)
                return h, self.n - 1

def md5(str):
    return rawMD5(str.encode('utf-8')).hexdigest()
