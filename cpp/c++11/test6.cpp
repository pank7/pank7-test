#include        <iostream>
#include        <thread>
#include        <mutex>
#include        <chrono>
#include        <functional>

void foo(std::mutex &mtx)
{
    mtx.lock();
    for (int i = 0; i < 10; ++i) {
        std::cout << "[" << std::this_thread::get_id() << "]"
                  << "foo..." << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
    mtx.unlock();
    return;
}


void bar(int x, std::mutex &mtx)
{
    mtx.lock();
    for (int i = 0; i < 10; ++i) {
        std::cout << "[" << std::this_thread::get_id() << "]"
                  << "bar (" << x << ")..." << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
    mtx.unlock();
    return;
}


int main()
{
    std::mutex          mtx;
    std::thread         first(foo, std::ref(mtx)); // spawn new thread that calls foo()
    std::thread         second(bar, 0, std::ref(mtx)); // spawn new thread that calls bar(0)

    std::cout << "main, foo and bar now execute concurrently...\n";

    // synchronize threads:
    first.join();
    // pauses until first finishes
    second.join();
    // pauses until second finishes

    std::cout << "foo and bar completed.\n";

    return 0;
}
