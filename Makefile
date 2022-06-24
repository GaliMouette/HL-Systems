NAME = L-Systems

.PHONY: all clean fclean re tests_run

.SILENT:
all: $(NAME)

$(NAME): app/Main.hs $(shell find src -type f -name "*.hs")
	$(info Building...)
	stack install --local-bin-path .

clean:
	$(info Cleaning...)
	stack clean

fclean: clean
	$(info Removing...)
	stack purge
	$(RM) $(NAME)

tests_run:
	$(info Running tests...)
	stack test

re: fclean all
