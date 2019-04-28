#!/bin/python3

import sys
from random import randint

def printRandPixel(maxpos):
	point = [randint(0, maxpos) for i in range(2)]
	color = [randint(0, 255) for i in range(3)]

	print("(%d,%d) (%d,%d,%d)" % (point[0], point[1], color[0], color[1], color[2]))


def main(argv):
	maxpos = 1000
	nbPixels = 100

	if len(argv) >= 2:
		maxpos = int(argv[1])
	if len(argv) >= 1:
		nbPixels = int(argv[0])

	for i in range(0, nbPixels):
		printRandPixel(maxpos)


if __name__ == '__main__':
	main(sys.argv[1:])
