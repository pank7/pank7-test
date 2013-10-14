#!/usr/bin/env python

import collections

def fi(area):
    R = len(area)
    C = len(area[0])
    count = 0
    island = collections.deque()
    for r, c in ((r, c) for r, row in enumerate(area) for c, cell in enumerate(row) if cell == '1'):
        count += 1
        area[r][c] = count
        island.append((r, c))
        while len(island) > 0:
            rr, cc = island.popleft()
            for nr, nc in ((i, j) for i, j in ((rr - 1, cc), (rr + 1, cc), (rr, cc - 1), (rr, cc + 1)) if i >= 0 and i < R and j >= 0 and j < C):
                if area[nr][nc] == '1':
                    island.append((nr, nc))
                    area[nr][nc] = count
    return count

if __name__ == "__main__":
    C, R = map(int, raw_input().rstrip().split(' '))
    print fi([raw_input().rstrip().split(' ') for _ in xrange(R)])
