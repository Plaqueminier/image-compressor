##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

all:
	./path.sh

clean:
		stack clean

fclean: 	clean
			rm -f imageCompressor

re: fclean all
