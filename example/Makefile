DIST_DIR=dist

define compile
	mkdir -p $(DIST_DIR)
	stack exec htcc -- $1 | gcc -xassembler -no-pie -o $(DIST_DIR)/$2 -
endef

define compile_docker_and_run
	mkdir -p /tmp/htcc
	stack exec htcc -- $1 > /tmp/htcc/spec.s
	dhall-to-yaml < ../docker/example.dhall | docker-compose -f - up
	rm -r /tmp/htcc
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

docker: docker_knapsack docker_shuffle_and_sort docker_merge_sort_linked_list

docker_knapsack:
	@$(call compile_docker_and_run,"./knapsack.c")

docker_shuffle_and_sort:
	@$(call compile_docker_and_run,"./shuffle_and_sort.c")

docker_merge_sort_linked_list:
	@$(call compile_docker_and_run,"./merge_sorting_linked_list.c")

clean:
	$(RM) -r $(DIST_DIR)

clean_docker:
	dhall-to-yaml < ../docker/example.dhall | docker-compose -f - down --rmi all

.PHONY: knapsack shuffle_and_sort merge_sort_linked_list lifegame docker_knapsack docker_shuffle_and_sort docker_merge_sort_linked_list docker clean
