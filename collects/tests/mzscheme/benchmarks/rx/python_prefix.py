
import re
import time

def test(rx, input, iterations):
    crx = re.compile(rx)
    start = time.time()
    for i in range(0, iterations):
        re.search(crx, input)
    print(time.time() - start)
