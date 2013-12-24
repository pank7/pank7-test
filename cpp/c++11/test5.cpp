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

typedef std::unordered_map<std::string,std::string> stringmap;

// function with lvalue and rvalue reference overloads:
void overloaded(const int &x) {std::cout << "[lvalue:" << x << "]";}
void overloaded(int &&x) {std::cout << "[rvalue:" << x << "]";}

// function template taking rvalue reference to deduced type:
template <class T>
void
foobar(T &&x)
{
    overloaded(x);                   // always an lvalue
    overloaded(std::forward<T>(x));  // rvalue if argument is rvalue
}

int
main(int argc, char *argv[])
{
    stringmap a = {{"AAPL","Apple"}, {"MSFT","Microsoft"}, {"GOOG","Google"}};
    stringmap b = {{"MSFT","Microsoft"}, {"GOOG","Google"}, {"AAPL","Apple"}};
    stringmap c = {{"MSFT","Microsoft Corp."}, {"GOOG","Google Inc."}, {"AAPL","Apple Inc."}};

    if (a == b) std::cout << "a and b are equal\n";
    if (b != c) std::cout << "b and c are not equal\n";

    for (const auto &e : c) {
        std::cout << e.first << " = " << e.second << std::endl;
    }

    auto        fn = [] (stringmap::value_type &e, std::string sadd) {
        e.second += sadd;
    };
    for_each(std::begin(c), std::end(c), std::bind(fn, std::placeholders::_1, " hoge"));

    for (auto p = std::begin(c); p != std::end(c); ++p) {
        std::cout << p->first << " = " << p->second << std::endl;
    }

    auto        mytuple = std::make_tuple(7, 7.7, 'A');
    int         myint;
    char        mychar;
    std::tie(myint, std::ignore, mychar) = mytuple;
    auto        mydouble = std::get<1>(mytuple);
    std::cout << "<" << myint << ", " << mydouble << ", " << mychar << ">" << std::endl;

    int                 init[] = {10, 20, 30, 40, 50};
    std::valarray<int>  foo(init,5);
    std::valarray<int>  &&bar = foo.apply([] (int x) {return ++x;}).cshift(2);

    std::cout << "foo contains:";
    for (const auto &elem : bar) std::cout << ' ' << elem;
    std::cout << std::endl;

    std::function<int(int,int)> fns[] = {
        std::plus<int>(),
        std::minus<int>(),
        std::multiplies<int>()
    };
    for (auto &x : fns) std::cout << x(10, 5) << std::endl;

    std::string                 s1 = "s1-string", s2 = "s2-string";
    std::vector<std::string>    myvec;

    myvec.push_back(s1);
    myvec.push_back(std::move(s2));

    std::cout << "myvec:";
    for (auto &elem : myvec) {
        std::cout << ' ' << elem;
        elem += "hoge";
    }
    std::cout << std::endl;
    std::cout << "s1: " << s1 << std::endl
              << "s2: " << s2 << std::endl;

    std::cout << "calling foobar with lvalue: ";
    foobar(myint);
    std::cout << std::endl;
    std::cout << "calling foobar with rvalue: ";
    foobar(0);
    std::cout << std::endl;

    return 0;
}
