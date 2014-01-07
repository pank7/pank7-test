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
#include        <cstdarg>

void
foo(size_t count, ...)
{
    va_list     args;
    std::cout << "foo start..." << std::endl;
    va_start(args, count);
    for (size_t i = 0; i < count; ++i) {
        double  num = va_arg(args, double);
        std::cout << "arg " << i + 1 << ": " << num << std::endl;
    }
    std::cout << "foo end..." << std::endl;
    return;
}

int
main(int argc, char *argv[], char *env[])
{
    foo(0);
    foo(1, 1.1);
    foo(2, 2.2, 3.3);
    foo(3, 2.2, 3.3, 4.4);

    return 0;
}
