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
 * @brief  see %b.h
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

#include <sys/time.h>
#include <iomanip>
#include <iostream>
#include <string>

#include "%b.h"

namespace Util {

    std::string dateToString() {
	time_t rawtime;
	time ( &rawtime );
	return std::string(ctime (&rawtime) );
    }
    
    std::ostream& log() {
	std::cout << "[" << Util::dateToString() << "]: ";
	return std::cout;
    }

    std::ostream& verbose(const short verboselevel) {
	std::string tabs = "";
	for (short i=0; i < verboselevel; i++) tabs += "\t";
	std::cout << tabs << "[VL" << verboselevel << "] ";
	return std::cout;
    }

    std::ostream& error() {
	std::cerr << "--- ERROR -- [" << dateToString() << "]" ;
	return std::cerr;
    }

} // end namespace Util
