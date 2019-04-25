##
## EPITECH PROJECT, 2019
## cpp Rush3
## File description:
## makefile for rush3 MyGKrellm
##

all:	imageCompressor

imageCompressor:
	stack build
	cp `stack path --local-install-root`/bin/imageCompressor .

.PHONY:		all imageCompressor
