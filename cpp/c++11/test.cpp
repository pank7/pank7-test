#include        <iostream>
#include        <array>

int
main(int argc, char *argv[])
{
    std::array<int, 10>         a({1,2,3,4,5,6,7,8,9,0});

    for (auto i = 0; i < 10; ++i) {
        a[i] = i * 10;
    }

    for (auto &e : a) {
        e += 1;
    }

    for (auto it = a.begin(); it != a.end(); ++it) {
        std::cout << *it << std::endl;
    }

    return 0;
}
