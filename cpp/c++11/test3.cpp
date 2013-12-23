#include        <iostream>
#include        <memory>
#include        <string>

std::string
return_a_string()
{
    return std::string("pank7");
}

void
print_ref(std::string &str)
{
    std::cout << std::addressof(str) << "(lvalue): " << str << std::endl;
    return;
}

void
print_ref(std::string &&str)
{
    std::cout << std::addressof(str) << "(rvalue): " << str << std::endl;
    return;
}

int
bar()
{
    return 777;
}

void
foo(int &&i)
{
    std::cout << std::addressof(i) << ": " << i << std::endl;

    return;
}

class C
{
public:
    C() : m(0), n("default name")
        {
            std::cout << "Default constructor" << std::endl;
        }
    C(int i, const std::string &n) : m(i), n(n)
        {
            std::cout << "Normal constructor" << std::endl;
        }
    C(C &o) : m(o.m), n(o.n + " copy")
        {
            std::cout << "Copy constructor" << std::endl;
        }
    C(C &&o) noexcept : m(o.m), n(o.n + " move")
        {
            std::cout << "Move constructor" << std::endl;
            o.m = 0;
        }
    ~C()
        {
            std::cout << "Destructor: \"" << this->n
                      << "\" = " << this->m << std::endl;
        }
private:
    int         m;
    std::string n;
};

int
main(int argc, char *argv[])
{
    std::string &&astr = return_a_string();

    std::cout << std::addressof(astr) << ": " << astr << std::endl;

    print_ref(astr);
    print_ref(return_a_string());

    foo(bar());

    int         hoge = 777;
    int         &&fuga = std::move(hoge);

    fuga = 7777;

    std::cout << std::addressof(hoge) << ": " << hoge << std::endl;
    std::cout << std::addressof(fuga) << ": " << fuga << std::endl;

    C           a;
    C           b(777, "b");
    C           c(b);
    C           d(std::move(b));

    return 0;
}

