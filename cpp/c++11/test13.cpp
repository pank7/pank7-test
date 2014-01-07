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

    std::random_device                                  rd;
    std::uniform_int_distribution<unsigned long long>   u(0, std::numeric_limits<unsigned long long>::max());
    unsigned long long                                  num = u(rd);
    std::bitset<8 * sizeof(unsigned long long)>         bits = num;

    std::cout << "rd max: " << rd.max() << ", rd min: " << rd.min() << std::endl;
    std::cout << num << std::endl;
    std::cout << bits.to_string('-', '*') << std::endl;
    std::cout << bits.flip().to_ullong() << std::endl;
    std::cout << bits.count() << std::endl;
    std::cout << bits.size() << std::endl;
    std::cout << bits.set().to_ullong() << std::endl;
    std::cout << bits.reset() << std::endl;
    bits.set(0);
    std::cout << bits << std::endl;


    return 0;
}
