/**
 * @file   %f
 * @author %U %a
 * @date   %d 
 * Time-stamp: < >
 *
 * Copyright (c) %y %U %a
 *               %o
 *
 * @version 0.1 $Rev$
 * $Id$ 
 *
 * @brief  A set of toolbox functions
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __TOOLS_H
#define __TOOLS_H

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <iterator>
#include <boost/random.hpp>
#include <ctime>

/** 
 * Helpful function to print directly the content of a vector in an 
 * output stream 
 */
template<class T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    copy(v.begin(), v.end(), std::ostream_iterator<T>(os, " ")); 
    return os;
}

/** 
 * FromStringTo convert a string to a value of type T 
 * Example : int i = FromStringTo<int>(s)
 * @param s string to convert 
 * @return  value of type T corresponding to the conversion of s to this type
 */
template<class T> 
T FromStringTo(const std::string & s) {
    std::istringstream is(s);
    T t;
    is >> t;
    return t;
}

/** 
 * ToStringFrom convert any value v of type T to string 
 * Exemple: string s = ToStringFrom(14)
 * @param v value to convert
 * @return  a string corresponding to v
 */
template<class T> 
std::string ToStringFrom(const T & t) {
    std::ostringstream s;
    s << t;
    return s.str();
}

namespace Random {
    // ==========================================
    /**
     * Generator. Used to generate random objects of type T constructed from 
     * integers of type IntType which are uniformly chosen between min and max. 
     * It therefore assume that T has a constructor tacking one IntType as 
     * argument. 
     */
    template<class T, 
	     class IntType = int, 
	     class Engine = boost::mt19937>
    class Generator {
	typedef boost::uniform_int<IntType>   distribution_t;
	distribution_t  _dist;                    /**< type of distribution */

    public:
	Generator(const IntType min = IntType(0), 
		  const IntType max = IntType(10)) : _dist(min,max) {} 
	virtual ~Generator() {}

	IntType min() const { return _dist.min(); } 
	IntType max() const { return _dist.max(); } 
	void reset(const IntType min = 0, const IntType max = 10) { 
	    _dist = distribution_t(min, max); 
	}

	T operator()() { 
	    static Engine rng(static_cast<unsigned> (std::time(0)));
	    return T(_dist(rng)); 
	}
    };	// ============ end class Generator =================

    /**
     * RealGenerator. Used to generate random objects of type T constructed 
     * from reals of type RealType which are uniformly chosen between min 
     * and max. It therefore assume that T has a constructor tacking one 
     * RealType as argument. 
     */
    template<class T, 
	     class RealType = double, 
	     class Engine = boost::mt19937>
    class RealGenerator {
	typedef boost::uniform_real<RealType>       distribution_t;

    protected: 
	distribution_t  _dist;                    /**< type of distribution */

    public:
	RealGenerator(const RealType min = RealType(0.0), 
		      const RealType max = RealType(1.0)) : _dist(min,max) {} 
	virtual ~RealGenerator() {}

	RealType min() const { return _dist.min(); } 
	RealType max() const { return _dist.max(); } 
	void reset(const RealType min = 0.0, const RealType max = 1.0) { 
	    _dist = distribution_t(min, max); 
	}

	virtual T operator()() { 
	    static Engine rng(static_cast<unsigned> (std::time(0)));
	    return T(_dist(rng)); 
	}
    };	// ============ end class RealGenerator ===================

}; // namespace Random

#endif   // __TOOLS_H
