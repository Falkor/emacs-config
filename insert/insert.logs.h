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
 * @brief  Management of different macros for logs, assertions etc. 
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
#ifndef __%B_H
#define __%B_H

#include <exception>

/**
 * @namespace Util
 * @brief  a set of useful element for logging/debugging typically
 */
namespace Util {

    /**
     * @def PACKAGENAME_XVALTOSTR(msg)
     * @ingroup Util
     * @brief Helper for macro PACKAGENAME_VALTOSTR
     */
#define PACKAGENAME_XVALTOSTR(msg) \
    #msg

    /**
     * @def PACKAGENAME_VALTOSTR(msg)
     * @ingroup Util
     * @brief Macro to convert value to a string
     */
#define PACKAGENAME_VALTOSTR(msg) \
    PACKAGENAME_XVALTOSTR(msg)

    /**
     * @def PACKAGENAME_LOCINFO
     * @ingroup Util
     * @brief Prepend file name and line number 
     */
#define PACKAGENAME_LOCINFO       \
    __FILE__ "(" PACKAGENAME_VALTOSTR(__LINE__) ")"

    /**
     * @def PACKAGENAME_OSTREAM_FUNCTION_INFO
     * @ingroup Util
     * @brief Prepend file name with line number and function name 
     *        inside brackets on a stream 
     */
#define PACKAGENAME_OSTREAM_FUNCTION_INFO  \
    "[" << PACKAGENAME_LOCINFO << ":" << __FUNCTION__ << "] "

    /**
     * @def PACKAGENAME_SUFX_M(msg)
     * @ingroup Util
     * @brief Append file name and line number to the argument string
     */
#define PACKAGENAME_SUFX          \
    "\t in " PACKAGENAME_LOCINFO
#define PACKAGENAME_SUFX_M(msg)   \
    msg PACKAGENAME_SUFX

    /** @def PACKAGENAME_ASSERT_M(cond,msg)
     * @ingroup Util
     * @brief Assertion which display message before throwing std::runtime_error 
     *        if cond is failed
     */
#define PACKAGENAME_ASSERT_M(cond,msg)					\
    if (!(cond)) {							\
        Util::error() << PACKAGENAME_OSTREAM_FUNCTION_INFO << "Assertion (" << PACKAGENAME_VALTOSTR(cond) << ") failed! : " << msg << ::std::endl; \
	throw std::runtime_error("");					\
    }

    /** @def PACKAGENAME_ASSERT(cond)
     * @ingroup Util
     * @brief Assertion which throw std::runtime_error() if cond is failed
     */
#define PACKAGENAME_ASSERT(cond)						\
    if (!(cond)) {							\
	Util::error() << PACKAGENAME_OSTREAM_FUNCTION_INFO << "Assertion (" << PACKAGENAME_VALTOSTR(cond) << ") failed!\n"; \
	throw std::runtime_error("");					\
    }

    /** @def PACKAGENAME_DO_ON_DEBUG(expr)
     * @ingroup Util
     * @brief execute expr only if the DEBUG macro is activated
     */
    /** @def PACKAGENAME_DEBUG(msg)
     * @ingroup Util
     * @brief print msg only if the DEBUG macro is activated. 
     *        msg can be written in "stream" format i.e you can write 
     *        \code PACKAGENAME_DEBUG("an " << object << "is put in a stream");
     *        \endcode
     */
#ifndef DEBUG
#  define PACKAGENAME_DO_ON_DEBUG(expr)
#  define PACKAGENAME_DEBUG(msg)
#else /* DEBUG */
#  define PACKAGENAME_DO_ON_DEBUG(expr)	\
    ::std::cout << "--- DEBUG --- " << PACKAGENAME_OSTREAM_FUNCTION_INFO; \
    expr;       
#  define PACKAGENAME_DEBUG(msg) \
    ::std::cout << "--- DEBUG --- " << PACKAGENAME_OSTREAM_FUNCTION_INFO << msg << ::std::endl;
#endif

    /** 
     * Stream to add some log entries. 
     * By default, it output to std::cout yet it will be possible to change the target 
     * @return output stream (std::cout by default) 
     */
    ::std::ostream& log();
    
    /** 
     * Stream to add some verbosity entries. 
     * By default, it output to std::cout yet it will be possible to change the target 
     * @return output stream (std::cout by default) 
     */
    ::std::ostream& verbose(const short verboselevel = 0);
    
    /** 
     * Stream to print errors. 
     * By default, it output to std::cerr yet it will be possible to change the target 
     * @return output stream (std::cerr by default) 
     */
    ::std::ostream& error();


} // end namespace Util


#endif  // __%B_H




