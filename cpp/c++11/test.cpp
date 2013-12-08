#include        <iostream>
#include        <array>

constexpr int
get_size(bool foo)
{
    return foo ? 10 : 20;
}

int
main(int argc, char *argv[])
{
    constexpr int               c = get_size(false);
    std::array<int, c>          a({1,2,3,4,5,6,7,8,9,0});

    for (auto i = 0; i < c; ++i) {
        a[i] = i * 10;
    }

    for (auto &e : a) {
        e += 1;
    }

    for (auto it = a.cbegin(); it != a.cend(); ++it) {
        std::cout << *it << std::endl;
    }

    return 0;
}
