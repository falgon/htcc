DIST_DIR=dist

define compile
	mkdir -p $(DIST_DIR)
	stack exec htcc -- $1 | gcc -xassembler -no-pie -o $(DIST_DIR)/$2 -
endef

all: knapsack shuffle_and_sort merge_sort_linked_list lifegame

knapsack:
	@$(call compile,"./knapsack.c","knapsack")

shuffle_and_sort:
	@$(call compile,"./shuffle_and_sort.c","shuffle_and_sort")

merge_sort_linked_list:
	@$(call compile,"./merge_sorting_linked_list.c","merge_sorting_linked_list")

lifegame:
	@$(call compile,"./lifegame.c","lifegame")

clean:
	$(RM) -r $(DIST_DIR)

.PHONY: knapsack shuffle_and_sort merge_sort_linked_list lifegame clean
