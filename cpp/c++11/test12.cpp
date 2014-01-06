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
#include        <cctype>

class C
{
public:
    C() : value(10)
        {
        }
    void
    hoge()
        {
            if (value > 0) this->f();
        }
private:
    int                         value;
    std::function<void()>       f = [this] {
        std::cout << this->value-- << std::endl;
        this->hoge();
    };
};

int
main(int argc, char *argv[])
{
    C().hoge();

    return 0;
}
