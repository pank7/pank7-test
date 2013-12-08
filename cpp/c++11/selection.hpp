//
// select k algorithm
// header file
// author: pank7 <liyi84@gmail.com>
//

#ifndef __SELECTION_HPP__
#define __SELECTION_HPP__

#include        <algorithm>

template<typename C>
typename C::const_iterator
selection(C &A, const unsigned int &k)
{
    std::sort(A.begin(), A.end());
    return A.begin() + k - 1;
}

#endif  // __SELECTION_HPP__
