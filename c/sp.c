#include        <stdio.h>
#include        <stdlib.h>

int
find_neighbors(int R, int C, int r, int c, int neighbors[4][2])
{
  int   count = 0;
  if (r - 1 >= 0 && r - 1 < R) {
    neighbors[count][0] = r - 1;
    neighbors[count][1] = c;
    ++count;
  }
  if (r + 1 >= 0 && r + 1 < R) {
    neighbors[count][0] = r + 1;
    neighbors[count][1] = c;
    ++count;
  }
  if (c - 1 >= 0 && c - 1 < C) {
    neighbors[count][0] = r;
    neighbors[count][1] = c - 1;
    ++count;
  }
  if (c + 1 >= 0 && c + 1 < C) {
    neighbors[count][0] = r;
    neighbors[count][1] = c + 1;
    ++count;
  }

  return count;
}

int
main(int argc, char *argv[])
{
  int   i = 0, j = 0, k = 0, retval = 0, rows = 0, columns = 0;
  int   sp = -1;
  char  **map = NULL;
  int   **spmap = NULL;
  int   neighbors[4][2], num_of_neighbors = 0;

  retval = scanf("%d %d", &columns, &rows);
  // printf("%d %d\n", columns, rows);

  map = (char **)malloc(sizeof(char *) * rows);
  spmap = (int **)malloc(sizeof(int *) * rows);
  for (i = 0; i < rows; ++i) {
    map[i] = (char *)malloc(sizeof(char) * columns);
    spmap[i] = (int *)malloc(sizeof(int) * columns);
    for (j = 0; j < columns; ++j) {
      scanf("%s", map[i] + j);
      spmap[i][j] = -1;
      if (map[i][j] == 'g') {
        spmap[i][j] = 0;
      }
    }
  }

  while (1) {
    int         updated_flag = 0, found_flag = 0;
    for (i = 0; i < rows; ++i) {
      for (j = 0; j < columns; ++j) {
        int     currentsp = spmap[i][j];
        if (currentsp < 0) continue;
        num_of_neighbors = find_neighbors(rows, columns, i, j, neighbors);
        for (k = 0; k < num_of_neighbors; ++k) {
          int   nr = neighbors[k][0], nc = neighbors[k][1];
          char  n = map[nr][nc];
          int   nn = spmap[nr][nc];
          switch (n) {
          case '0':
            if (nn == -1 || nn > currentsp + 1) {
              spmap[nr][nc] = currentsp + 1;
              ++updated_flag;
            }
            break;
          case 's':
            sp = currentsp + 1;
            found_flag = 1;
            break;
          default:
            break;
          }                     // switch
          if (found_flag == 1) break;
        }                       // for (k...
        if (found_flag == 1) break;
      }                         // for (j...
      if (found_flag == 1) break;
    }                           // for (i...
    if (updated_flag == 0) break;
    if (found_flag == 1) break;
  }                             // while

  if (sp == -1) printf("Fail\n");
  else printf("%d\n", sp);

  for (i = 0; i < rows; ++i) {
    free((void *)map[i]);
    free((void *)spmap[i]);
  }
  free((void *)map);
  free((void *)spmap);

  return 0;
}
