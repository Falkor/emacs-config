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

#include <stdio.h>     /* for printf */
#include <stdlib.h>    /* for exit  */
#include <assert.h>    /* for assert */
#include <string.h> 
#include <getopt.h> 

//#include "%b.h"

#define VERSION  0.1   // source version
#define MAX_SIZE 256   // maximal size of strings, including '\0'

void printHelp(char * command);                        // print help message
void printError(char * error_message, char * command); // print error message
void printVersion(char * command);                     // print version

/**
 * Entry point where the program begins its execution.
 * @param argc number of arguments of the command line
 * @param argv multi-array containing the command line arguments
 *             (argv[0] is the name of the program, argv[argc]=NULL)
 * @return status of the execution (0: correct execution)
 */
int main(int argc, char * argv[]){

    // Command line management 
    extern char *optarg;  // specific to getopt
    extern int optind;    //, opterr, optopt;  // id.
    char c;

    // to deal with options
    char opt_i[MAX_SIZE] = "";
    char opt_f[MAX_SIZE] = "";
    long arg_i = 14;
    double arg_f = 1.5; 
    char arg_s[MAX_SIZE] = "On est super content!";
    char arg_file[MAX_SIZE] = "";

 
    // Management of parameters in command line
    while ((c = getopt(argc, argv, "i:f:s:hV")) != -1) {
        switch(c) {
        case 'h': printHelp(argv[0]);    return EXIT_SUCCESS; 
        case 'V': printVersion(argv[0]); return EXIT_SUCCESS;
        case 'i':   
            assert(strlen(optarg) < MAX_SIZE); // check size
            strncpy(opt_i,optarg,MAX_SIZE);    // copy current arg to opt_i
            // Minimal check for validity (opt_i isn't a command line option)
            if (opt_i[0] == '-') printError("Bad Format for arg_i!",argv[0]);
            arg_i = strtol(opt_i, NULL, 10);   // last arg: base for convertion   
            break;
        case 'f':   
            assert(strlen(optarg) < MAX_SIZE); // check size
            strncpy(opt_f,optarg,MAX_SIZE);    // copy current arg to opt_f
            // Minimal check for validity (opt_f isn't a command line option)
            if (opt_f[0] == '-') printError("Bad Format for arg_f!",argv[0]);
            arg_f = strtod(opt_f, NULL);       // conversion   
            break;
        case 's':   
            assert(strlen(optarg) < MAX_SIZE); // check size
            strncpy(arg_s,optarg,MAX_SIZE);    // copy current arg to arg_s
            if (arg_s[0] == '-') printError("Bad Format for arg_i!",argv[0]);
            break;
        default: printError("Bad Format!",argv[0]);
        }
    }
    
    // Now proceed to detect errors and/or set the values
    
    // parameter arg_file is required. If not, print error and exit
    // argc - optind == 0 : no arg_file
    // argc - optind > 1 : to many parameters or error not detected
    if ((argc - optind) != 1) printError("Bad Format",argv[0]);

    // required parameter: arg_file
    assert(strlen(argv[optind]) < MAX_SIZE);
    strncpy(arg_file,argv[optind],MAX_SIZE);
    
    //Print values
    printf("arg_i = %%ld\n",arg_i);
    printf("arg_f = %%f\n",arg_f);
    printf("Valeur de arg_s: %%s\n",arg_s);
    printf("Valeur de arg_file: %%s\n",arg_file);
    
    return EXIT_SUCCESS;
}


/** 
 * Print the help message  
 * @param command used (argv[0])
 */
void printHelp(char * command) {
    printf("NAME\n"                                                               );
    printf("      %%s\n",command                                                   );
    printf("\nSYNOPSIS\n"                                                         ); 
    printf("      %%s [-h] [-V]\n",command                                         );
    printf("      %%s [-i arg_i] [-f arg_f] [-s arg_s] arg_file\n",command         );
    printf("\nDESCRIPTION\n"                                                      ); 
    printf("      -h : print help and exit\n"                                     );
    printf("      -V : print version and exit\n"                                  );
    printf("      -i arg_i : set arg_i (long)\n"                                  );
    printf("           Default value : 14\n"                                      );
    printf("      -f arg_f : set arg_f (float)\n"                                 );
    printf("           Default value : 1.5\n"                                     );
    printf("      -s arg_s : set arg_s (char *), a string with at most MAX_SIZE\n");
    printf("           characters. Default value : \"Toto\"\n"                    );
    printf("\nAUTHOR\n"                                                           );
    printf("      %U %a\n"              );
    printf("      Web page : http://www-id.imag.fr/~svarrett/\n"                  );
    printf("\nREPORTING BUGS\n"                                                   );
    printf("      Please report bugs to %a\n"           );
    printf("\nCOPYRIGHT\n"                                                        );
    printf("      This  is free software; see the source for copying conditions.\n");
    printf("      There is NO warranty; not even for MERCHANTABILITY or FITNESS\n");
    printf("      FOR A PARTICULAR PURPOSE."                                      );
    printf("\nSEE ALSO\n"                                                         );
    printf("      http://www-id.imag.fr/~svarrett/perso.html\n"                   );
}

/**
 * print error message on stderr 
 * @param error_message Error message to print 
 * @param command       command used (argv[0])
 */
void printError(char * error_message, char * command) {
    fprintf(stderr, "[ERROR] %%s\n",error_message);
    fprintf(stderr, "Use '%%s -h' for help\n", command);
    exit(EXIT_FAILURE);
}
/**
 * print version
 * @param command used (argv[0])
 */
void printVersion(char * command) {
    fprintf(stderr, "This is %%s  version %%f\n",command,VERSION);
    fprintf(stderr, "Please type '%%s -h' for help\n", command);    
}



