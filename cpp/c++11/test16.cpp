// A naive solution of:
// https://oj.leetcode.com/problems/find-minimum-in-rotated-sorted-array/
#include    <iostream>
#include    <vector>
#include    <algorithm>

using std::vector;

class Solution {
public:
    int findMin(vector<int> &num) {
        auto left = num.begin();
        auto right = num.begin() + num.size() - 1;
        auto mid = num.begin() + num.size() / 2;
        if (*left > *right) {
            while (*left > *right) {
                    if (*left < *mid) {
                        left = mid;
                    } else if (*left > *mid) {
                        right = mid;
                    } else {
                        return *(left + 1);
                    }
                    mid = left + (right - left) / 2;
            }
        } else {
            while (*left < *right) {
                    if (*left < *mid) {
                        right = mid;
                    } else if (*left > *mid) {
                        left = mid;
                    } else {
                        return *left;
                    }
                    mid = left + (right - left) / 2;
            }
        }
        
        return *left;
    }
};

int
main(int argc, char *argv[])
{
    Solution s;
    vector<int> num;
    for (auto i = 0; i < 1; ++i) {
        num.push_back(i);
    }
    // std::rotate(num.begin(), num.begin() + 40, num.end());
    // std::reverse(num.begin(), num.end());

    std::cout << s.findMin(num) << std::endl;

    return 0;
}
