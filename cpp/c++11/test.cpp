#include        <iostream>
#include        <fstream>
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
    time_t                      seed = time(NULL);
    std::ifstream               urandom("/dev/urandom", std::fstream::in | std::fstream::binary);

    if (urandom.is_open()) {
        urandom.read(reinterpret_cast<char *>(&seed), sizeof(seed));
        std::cout << "Seed: " << seed << std::endl;
    }

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
