#include        <iostream>
#include        <fstream>
#include        <functional>
#include        <array>
#include        <algorithm>
#include        <random>

#include        <ctime>

void
foo(std::function<void()> &&f)
{
    f();
    return;
}

int
main(int argc, char *argv[])
{
    time_t              seed = time(NULL);
    std::ifstream       urandom("/dev/urandom",
                                std::fstream::in | std::fstream::binary);
    if (urandom.is_open()) {
        urandom.read(reinterpret_cast<char *>(&seed), sizeof(seed));
        std::cout << "Seed: " << seed << std::endl;
    }
    std::uniform_int_distribution<int>  u(0, 1000);
    std::default_random_engine          re(seed);
    u(re);

    int         i = 0;
    auto        f = [&] { std::cout << i++ << std::endl; };
    foo(f);
    foo(f);

    constexpr const int         len = 50;
    std::array<int, len>        a;

    std::cout << "Array: " << std::endl;
    for (auto &elem : a) {
        elem = u(re);
        std::cout << elem << " ";
    }
    std::cout << std::endl;

    std::cout << "Even elements: " << std::endl;
    std::for_each(a.begin(), a.end(), [] (const int &i) {
            if (i % 2 == 0) std::cout << i << " ";
        });
    std::cout << std::endl;

    return 0;
}
