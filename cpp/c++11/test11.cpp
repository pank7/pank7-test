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

#include        <openssl/md5.h>

static std::size_t __string_to_uint(const std::string &k)
{
    std::size_t         h = 0, i = 1;
    for_each(std::begin(k), std::end(k), [&] (const char &c) {
            if (std::isdigit(c)) {
                h += ((c - '0') * i);
                i *= 10;
            }
        });
    return h;
}

struct shf
{
    std::size_t operator() (std::string k) const
        {
            k.erase(std::remove_if(k.begin(), k.end(), [] (const char &x) {
                        return not std::isdigit(x);
                    }), k.end());
            return std::stoul(k);
        };
};

struct shf1
{
    std::size_t operator() (const std::string &k) const
        {
            std::size_t         h = __string_to_uint(k);
            return (h * 2654435761) / 0xFFFFFFFFUL;
        };
};

struct shf2
{
    std::size_t operator() (const std::string &k) const
        {
            std::size_t         h = __string_to_uint(k);
            h = (h + 0x7ed55d16) + (h << 12);
            h = (h ^ 0xc761c23c) ^ (h >> 19);
            h = (h + 0x165667b1) + (h << 5);
            h = (h + 0xd3a2646c) ^ (h << 9);
            h = (h + 0xfd7046c5) + (h << 3);
            h = (h ^ 0xb55a4f09) ^ (h >> 16);
            return h;
        };
};

struct shf3
{
    std::size_t operator() (const std::string &k) const
        {
            std::size_t         h = __string_to_uint(k);
            h = ~h + (h << 15);
            h = h ^ (h >> 12);
            h = h + (h << 2);
            h = h ^ (h >> 4);
            h = h * 2057;
            h = h ^ (h >> 16);
            return h;
        };
};

struct shf4
{
    std::size_t operator() (const std::string &k) const
        {
            std::size_t         h = __string_to_uint(k);
            h = ((h >> 16) ^ h) * 0x45d9f3b;
            h = ((h >> 16) ^ h) * 0x45d9f3b;
            h = ((h >> 16) ^ h);
            return h;
        };
};

struct shf5
{
    std::size_t operator() (const std::string &k) const
        {
            unsigned char       *md5 = ::MD5(reinterpret_cast<const unsigned char *>(k.c_str()), k.length(), NULL);
            std::size_t         h = 0;
            unsigned char       *p = reinterpret_cast<unsigned char *>(&h);
            for (int i = 0; i < 16; ++i) {
                p[i % (sizeof(h))] ^= md5[i];
            }
            return h;
        };
};

typedef std::unordered_map<std::string, int>            strdict;
typedef std::unordered_map<std::string, int, shf5>      strdict2;

template <class T>
void
print_strdict_details(const T &d)
{
    std::cout << "size: " << d.size() << std::endl;
    // std::cout << "max_size: " << d.max_size() << std::endl;
    std::cout << "bucket_count: " << d.bucket_count() << std::endl;
    // std::cout << "max_bucket_count: " << d.max_bucket_count() << std::endl;
    std::cout << "load_factor: " << d.load_factor() << std::endl;
    // std::cout << "max_load_factor: " << d.max_load_factor() << std::endl;

    return;
}

template <class T>
void
print_strdict_stat(const T &d)
{
    std::unordered_map<std::size_t, unsigned int>       c;
    strdict::size_type          b, bs;
    for (const auto &kv : d) {
        b = d.bucket(kv.first);
        bs = d.bucket_size(b);
        if (c.count(bs) > 0) ++c[bs];
        else c[bs] = 1;
    }
    for (const auto &kv : c)
        std::cout << kv.first << " => " << kv.second / kv.first << std::endl;

    return;
}

int
main(int argc, char *argv[])
{

    strdict             d;
    std::ifstream       infile("input.tsv");
    std::string         p, u, dsp;
    int                 win;

    if (not infile.is_open()) {
        std::cerr << "Can not open file!" << std::endl;
        return 1;
    }

    std::cout << "now read file..." << std::endl;
    while (infile >> p >> u >> dsp >> win) {
        d[p + "-" + u + "-" + dsp] = win;
    }
    print_strdict_details(d);
    print_strdict_stat(d);
    std::cout << "now rehash..." << std::endl;
    d.rehash(d.size() * 3);
    print_strdict_details(d);
    print_strdict_stat(d);

    std::cout << "now d2..." << std::endl;
    strdict2            d2;
    for (auto &kv : d) d2[kv.first] = kv.second;
    print_strdict_details(d2);
    print_strdict_stat(d2);
    std::cout << "now rehash..." << std::endl;
    d2.rehash(d2.size() * 3);
    print_strdict_details(d2);
    print_strdict_stat(d2);

    return 0;
}
