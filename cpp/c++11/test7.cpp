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

class C
{
public:
    C()
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Normal Constructor" << std::endl;
        }
    C(C &o)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Copy Constructor" << std::endl;
        }
    C(C &&o)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Move Constructor" << std::endl;
        }
    ~C()
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Destructor" << std::endl;
        }
    void
    hoge()
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: hoge" << std::endl;
        }
};

C &&
foobar1()
{
    return C();
}

int
main(int argc, char *argv[])
{
    auto        c1 = foobar1();
//    auto        c2 = (c1);
//    auto        c3 = C(std::forward<C>(foobar1()));
    auto        c3 = C(foobar1());

    c1.hoge();
//    c2.hoge();
    c3.hoge();

    return 0;
}
