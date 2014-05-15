#include        <iostream>

template<class T>
void permutation(T a[], int t, int n)
{
    if (t == n) {
        for (int i = 0; i < n; ++i) {
            std::cout << a[i] << " ";
        }
        std::cout << std::endl;
        return;
    }

    for (int j = t; j < n; ++j) {
        std::swap(a[j], a[t]);
        permutation(a, t + 1, n);
        std::swap(a[j], a[t]);
    }
}

template<class T>
void fullpermutation(T a[], int t, int n){
    if (t == n) {
        for (int i = 0; i < n; ++i) {
            std::cout << a[i] << " ";
        }
        std::cout << std::endl;
        return;
    }

    for (int j = t; j < n; ++j) {
        int flag = 1;
        for (int r = t; r < j; ++r) {
            if (a[r] == a[j]) {
                flag = 0;
                break;
            }
        }
        if (flag == 0){
            continue;
        }
        std::swap(a[j], a[t]);
        fullpermutation(a, t + 1, n);
        std::swap(a[j], a[t]);
    }
}

int
main()
{
    char        a[] = "abcdefg";

    permutation(a, 0, 6);

    return 0;
}
