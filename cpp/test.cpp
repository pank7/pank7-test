#include        <iostream>
#include        <vector>
#include        <algorithm>
#include        <cmath>
#include        <cctype>

int
main(int argc, char *argv[])
{
    std::vector<double>     score;
    double                  s;

    std::cout << std::ispunct(',') << std::endl;

    while (std::cin >> s) {
        score.push_back(s);
    }

    std::sort(score.begin(), score.end());

    while (*score.begin() < 60) {
        for (auto &s: score) {
            s = sqrt(s) * 10;
        }
    }

    for (auto s: score) {
        std::cout << s << std::endl;
    }

    return 0;
}
