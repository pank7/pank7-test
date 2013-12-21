#include        <iostream>
#include        <memory>
#include        <string>

#include        <ctime>

void
foo(std::shared_ptr<int> i)
{
    std::cout << typeid(i).name() << ": " << (*i)++ << ": "
              << i.use_count() << std::endl;

    return;
}

int
main(int argc, char *argv[])
{
    auto                        ptr1 = std::make_shared<int>(777);
    auto                        ptr2 = ptr1;
    std::shared_ptr<int>        ip(new int(777)), ip2;
    {
        std::weak_ptr<int>          wip = ip;
        ip2 = wip.lock();
    }

    foo(ptr2);

    std::cout << typeid(ptr1).name() << ": " << *ptr1 << ": "
              << ptr1.use_count() << std::endl;
    std::cout << typeid(ip2).name() << ": " << *ip2 << ": "
              << ip2.use_count() << std::endl;

    std::unique_ptr<std::string>        strp(new std::string("pank7"));
    std::cout << typeid(strp).name() << ": " << *strp << std::endl;

    return 0;
}

