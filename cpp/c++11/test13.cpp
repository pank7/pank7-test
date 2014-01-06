#include        <iostream>
#include        <fstream>
#include        <string>
#include        <functional>
#include        <utility>
#include        <algorithm>
#include        <memory>
#include        <unordered_map>
#include        <vector>
#include        <tuple>
#include        <valarray>
#include        <bitset>
#include        <random>
#include        <limits>
#include        <cctype>
#include        <ctime>

int
main(int argc, char *argv[], char *env[])
{
    std::size_t         i = 0;

    while (env[i]) {
        std::cout << env[i++] << std::endl;
    }

    time_t                      seed = time(NULL);
    std::ifstream               urandom("/dev/urandom", std::fstream::in | std::fstream::binary);
    if (urandom.is_open()) {
        urandom.read(reinterpret_cast<char *>(&seed), sizeof(seed));
        std::cout << "Seed: " << seed << std::endl;
    }
    std::uniform_int_distribution<unsigned long long>   u(0, std::numeric_limits<unsigned long long>::max());
    std::default_random_engine                          re(seed);
    unsigned long long                                  num = u(re);
    std::bitset<8 * sizeof(unsigned long long)>         bits = num;

    std::cout << num << std::endl;
    std::cout << bits.to_string('-', '*') << std::endl;
    std::cout << bits.flip().to_ullong() << std::endl;
    std::cout << bits.count() << std::endl;
    std::cout << bits.size() << std::endl;
    std::cout << bits.set().to_ullong() << std::endl;
    std::cout << bits.reset() << std::endl;
    bits.set(0);
    std::cout << bits << std::endl;

    std::random_device  rd;
    std::cout << "max: " << rd.max() << ", min: " << rd.min() << std::endl;

    return 0;
}
