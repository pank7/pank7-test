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

void
foo(int &x)
{
    std::cout << "lvalue: " << x << std::endl;
}

void
foo(int &&x)
{
    std::cout << "rvalue: " << x << std::endl;
}

int
main(int argc, char *argv[])
{
    int         x = 0, &y = x, &&z = 7;

    foo(x);
    foo(y);
    foo(z);
    foo(7);
    foo(std::forward<int>(x));
    foo(std::forward<int>(y));
    foo(std::forward<int>(z));

    return 0;
}
