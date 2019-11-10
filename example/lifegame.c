/*
 *
 * The simulator of Conway's Game of Life (See also: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
 * Rules:
 *  1. The cell has one life or not. In this, cells with life are '@', and cells without life are space.
 *     There are eight neighbors, including diagonally.
 *  2. Each life will survive in the next generation if the number of lives in adjacent cells is 2 or 3. 
 *     Otherwise it will die.
 *  3. A cell without life will be born in the next generation if the number of lives in adjacent cells is 3.
 *
 */

typedef long time_t;

void srand(int);
int rand(void);
time_t time(time_t);
void sleep(int);
void* calloc(int, int);
void free(void*);
int printf();
int system(char*);

_Bool*** init_map(int n)
{
    _Bool*** map = calloc(2, sizeof(_Bool***));
    for (int i = 0; i < 2; ++i) {
        map[i] = calloc(n + 2, sizeof(_Bool*));
        for (int j = 0; j < n + 2; ++j) {
            map [i][j] = calloc(n + 2, sizeof(_Bool));
        }
    }

    srand(time(0));

    for (int i = 1; i <= n; ++i)
        for (int j = 1; j <= n; ++j)
            map[0][i][j] = rand() % 2;

    return map;
}

void free_map(_Bool*** map, int n)
{
    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < n + 2; ++j) {
            free(map[i][j]);
            map[i][j] = 0;
        }
        free(map[i]);
        map[i] = 0;
    }
    free(map);
    map = 0;
}

void lifegame(_Bool*** m, int cell_size)
{
    int st = 0;
    for (int g = 0;; ++g) {
        system("clear");
        printf("Generation: %d\n", g);
        for (int i = 1; i <= cell_size; ++i) {
            for (int j = 1; j <= cell_size; ++j)
                printf("%c", m[st][i][j] ? '@' : ' ');
            printf("\n");
        }
        printf("\n");
        sleep(1);

        int t = 0;
        for (int i = 1; i <= cell_size; ++i) {
            for (int j = 1; j <= cell_size; ++j) {
                t = m[st][i + 1 > cell_size ? 1 : i + 1][j] +
					m[st][i - 1 ?: cell_size][j] +
                  	m[st][i][j + 1 > cell_size ? 1 : j + 1] +
                  	m[st][i][j - 1 ?: cell_size] +
                  	m[st][i + 1 > cell_size ? 1 : i + 1][j + 1 > cell_size ? 1 : j + 1] +
                  	m[st][i + 1 > cell_size ? 1 : i + 1][j - 1 ?: cell_size] +
                  	m[st][i - 1 ?: cell_size][j + 1 > cell_size ? 1 : j + 1] +
                  	m[st][i - 1 ?: cell_size][j - 1 ?: cell_size];
                m[st ^ 1][i][j] = m[st][i][j] ? t == 2 || t == 3 : t == 3;
            }
        }
        st ^= 1;
    }
}

int main()
{
    int n = 30;
    _Bool*** m = init_map(n);
    lifegame(m, n);
    free_map(m, n);
}
