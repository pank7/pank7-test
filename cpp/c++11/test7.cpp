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
    C() : value(7)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Normal Constructor" << std::endl;
        }
    C(C &o) : value(o.value)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Copy Constructor" << std::endl;
        }
    C(C &&o) : value(o.value)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Move Constructor" << std::endl;
            o.value = 0;
        }
    ~C()
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Destructor" << std::endl;
        }
    C &
    operator=(C &o)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Operator = &" << std::endl;
            return *this;
        }
    C &
    operator=(C &&o)
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: Operator = &&" << std::endl;
            return *this;
        }
    void
    hoge()
        {
            std::cout << "[" << this << "|" << __LINE__ << "]: " << this->value << std::endl;
        }
private:
    int         value;
};

C
foobar()
{
    return C();
}

int
main(int argc, char *argv[])
{
    auto        c1 = std::move(C());
    auto        c2 = foobar();
    auto        c3 = c1;
    auto        c4 = C(std::forward<C>(foobar()));
    auto        c5 = C(foobar());

    c5 = c4;
    c5 = foobar();

    c1.hoge();
    c2.hoge();
    c3.hoge();
    c4.hoge();
    c5.hoge();

    return 0;
}
