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
    constexpr const std::size_t         s = 10;
    std::random_device                  rd;
    std::mt19937                        gen(rd());
    std::uniform_real_distribution<>    dis(-1, 1);
    std::valarray<double>               va;

    va.resize(s);
    for (auto &e : va) e = dis(gen);
    va += 1;
    std::cout << "va(" << va.size() << "):";
    for (const auto &e : va) std::cout << ' ' << e;
    std::cout << std::endl;

    std::valarray<double>               vaa = va.cshift(1) * (double)10;
    std::cout << "vaa(" << vaa.size() << "):";
    for (const auto &e : vaa) std::cout << ' ' << e;
    std::cout << std::endl;

    return 0;
}
