/*
 *
 *
 * Perform merge sort on linked list and sort in ascending order.
 *
 *
 */

typedef long time_t;

time_t time(time_t);
void srand(int);
int rand();
void* calloc(int, int);
void free(void*);
int printf();

struct node_type {
    int val;
    struct node_type* next;
};

typedef struct node_type node;

void split(node* src, node** front, node** back)
{
    node* slow = *front = src;

    for (node* fast = src->next; fast;) {
        if (fast = fast->next) {
            slow = slow->next;
            fast = fast->next;
        }
    }
    *back = slow->next;
    slow->next = 0;
}

node* merge(node* lhs, node* rhs)
{
    node* result = 0;
    if (!lhs) return rhs;
    else if (!rhs) return lhs;

    if (lhs->val <= rhs->val) {
        result = lhs;
        result->next = merge(lhs->next, rhs);
    } else {
        result = rhs;
        result->next = merge(lhs, rhs->next);
    }
    return result;
}

void merge_sort(node** head_ref)
{
    node* head = *head_ref;
    
    if (!head || !head->next) return;
    
    node* lhs = 0;
    node* rhs = 0;
    split(head, &lhs, &rhs);
    merge_sort(&lhs);
    merge_sort(&rhs);
    *head_ref = merge(lhs, rhs);
}

void push(node** head_ref, int val)
{
    node* n = calloc(1, sizeof(node));
    n->val = val;
    n->next = *head_ref;
    *head_ref = n;
}

void print(node* n)
{
    for (; n; n = n->next) printf("%d ", n->val);
    printf("\n");
}

void free_list(node* n)
{
    for (node* t; n; n = t) {
        t = n->next;
        free(n);
        n = 0;
    }
}

node* rand_list(int s, int min, int max)
{
    srand(time(0));
    node* list = 0;
    for (int i = 0; i < s; ++i) push(&list, rand() % max + min);
    return list;
}

int main()
{
    int n = 10;
    node* list = rand_list(n, 1, 10);
    printf("Before sorting: ");
    print(list);

    merge_sort(&list);

    printf("After sotring: ");
    print(list);

    free_list(list);

    return 0;
}
