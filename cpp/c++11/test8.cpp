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
bar(int &x)
{
    std::cout << "bar(int &x): " << x << std::endl;

    return;
}

void
bar(int &&x)
{
    std::cout << "bar(int &&x): " << x << std::endl;

    return;
}

void
foo(int &x)
{
    std::cout << "foo(int &x): " << x << std::endl;

    bar(x);

    return;
}

void
foo(int &&x)
{
    std::cout << "foo(int &&x): " << x << std::endl;

    bar(std::forward<int>(x));

    return;
}

int     i = 0;

int &&
fuga()
{
    return std::move(i);
}

int &&
hoge()
{
    return fuga();
}

class C
{
public:
    C() : value(0)
        {
            std::cout << "[" << this << "] Constructor" << std::endl;
        }
    void
    hoge()
        {
            std::cout << "[" << this << "] " << this->value++ << std::endl;
        }
private:
    int         value;
};

int
main(int argc, char *argv[])
{
    foo(i);
    foo(7);

    int         &&ii = hoge();
    int         &&iii = std::forward<int>(ii);

    iii = 8;

    foo(std::forward<int>(iii));
    foo(fuga());

    auto        p1 = new C();
    p1->hoge();
    p1->hoge();

    auto        p2 = new (p1) C;
    p2->hoge();

    return 0;
}
