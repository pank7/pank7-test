#include    <iostream>
#include    <vector>
#include    <algorithm>
#include    <cmath>

int
main(int argc, char *argv[])
{
    std::vector<double>     score;
    double                  s;

    while (std::cin >> s) {
        score.push_back(s);
    }

    std::sort(score.begin(), score.end());

    while (*score.begin() < 60) {
        for (int i = 0; i <= score.size(); ++i) {
            score[i] = sqrt(score[i]) * 10;
        }
    }

    for (std::vector<double>::iterator i = score.begin();
            i < score.end(); ++i) {
        std::cout << *i << std::endl;
    }

    return 0;
}
