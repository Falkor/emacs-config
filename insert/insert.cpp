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

#include <string>
#include <iostream>
#include <sstream>
#include <getopt.h>
#include <stdlib.h>

//#include "%b.h"

using namespace std;

/* Useful functions for the main - definitions after */
template<class T> T FromStringTo(const std::string & s);
template<class T> std::string ToStringFrom(const T & v);
void printFormatErrorAndExit(char * command);
void printHelp(char * command);

/** 
 * Entry point where the program begins its execution.
 * @param argc number of arguments of the command line
 * @param argv multi-array containing the command line arguments 
 *             (argv[0] is the name of the program, argv[argc]=NULL)
 * @return status of the execution (0: correct execution)
 */
int main(int argc, char * argv[]){
    string arg = "Default";
    bool VERBOSE = false;

    // Command line management 
    extern char *optarg;  // specific to getopt
    extern int optind;    //, opterr, optopt;  // id.
    char c;

    while ((c = getopt(argc, argv, "hd:v")) != -1) {
        switch (c) {
        case 'h': printHelp(argv[0]); exit(0); break;
        case 'd': arg = string(optarg);        break;
        case 'v': VERBOSE=true;                break;
        default:  printFormatErrorAndExit(argv[0]);
        }
    }
    if ( (arg[0]=='-') || ((argc-optind)>0) ) 
        printFormatErrorAndExit(argv[0]);

    // Now ready to proceed

    // f(FromStringTo<int>(arg))
    return 0;
} 

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
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
std::string ToStringFrom(const T & v) {
    std::ostringstream s;
    s << v;
    return s.str();
}

/** 
 * function called to print on cerr a "bad format" message then exit 
 * @param command command used (argv[0])
 */
void printFormatErrorAndExit(char * command) {
    cerr << "Bad Format, use '" << command << " -h' for help\n";
    exit(1);
}

/** 
 * Print the help message  
 * @param command command used (argv[0])
 */
void printHelp(char * command) {
    cout <<
        "NAME\n"                                                             << 
    "      " << command << " - \n"                                           <<
    "\nSYNOPSIS\n"                                                           <<
    "      " << command << " [-h] [-d depth] [-v]\n"                         <<
    "\nDESCRIPTION\n"                                                        <<
    "      -h : print help and exit\n"                                       <<
    "      -d : value of the depth \n"                                       <<
    "           Default value : \n"                                          <<
    "      -v : verbose mode\n"                                              <<
    "\nAUTHOR\n"                                                             <<
    "      %U %a\n"                <<
    "      Web page : http://www-id.imag.fr/~svarrett/\n"                    <<
    "\nREPORTING BUGS\n"                                                     <<
    "      Please report bugs to %a\n"             <<
    "\nCOPYRIGHT\n"                                                          <<
    "      This  is free software; see the source for copying conditions.\n" <<
    "      There is NO warranty; not even for MERCHANTABILITY or FITNESS\n"  <<
    "      FOR A PARTICULAR PURPOSE."                                        <<
    "\nSEE ALSO\n"                                                           <<
    "      %o \n";
}

