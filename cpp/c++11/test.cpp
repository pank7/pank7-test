#include        <iostream>
#include        <array>
#include        <random>

#include        <ctime>

#include        "selection.hpp"

constexpr int
get_size(bool foo)
{
    return foo ? 10 : 20;
}

int
main(int argc, char *argv[])
{
    constexpr int               c = get_size(false);
    std::array<int, c>          a;

    std::uniform_int_distribution<int>  u(0, 1000);
    std::default_random_engine  re(time(NULL));

    u(re);

    for (auto &e : a) {
        e = u(re);
        std::cout << e << " ";
    }
    std::cout << std::endl;

    std::cout << "selection(a, 7) = "
              << *selection(a, 7)
              << std::endl;

    return 0;
}
