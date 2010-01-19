#!/usr/bin/perl -w

##############################################################################
# File      : %f 
# Creation  : %D %m %y
# Time-stamp: < >
#
# Copyright (c) %y %U %a
#               %o
# $Id$ 
#
# Description : %@
#		See the man page for more information.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>. 
##############################################################################
use strict;
use warnings;

# Used packages
use Getopt::Long;             # For command line management (long version)
use Term::ANSIColor;          # To send the ANSI color-change sequences to the user's terminal
use Pod::Usage;
#use Data::Dumper;

# Generic variables
my $VERSION = '0.1';          # Script version
my $VERBOSE = 0;              # option variable for verbose mode with default value (false)
my $DEBUG   = 0;              # option variable for debug mode with default value (false)
my $QUIET   = 0;              # By default, display all informations
my $numargs = scalar(@ARGV);  # Number of arguments
my $command = `basename $0`;  # base command
chomp($command);

# Specific variables
my $SIMULATION_MODE = 0;      # By default, don't simulate
# YOUR JOB

# Parse command line
my @arg;                      # Example for option 

my $getoptRes = GetOptions('arg=s'     => \@arg,                                # Add option
			   'dry-run|n'       => \$SIMULATION_MODE,              # Simulation mode
			   'verbose|v' => \$VERBOSE,                            # Verbose mode
			   'quiet|q'   => \$QUIET,                              # Quiet mode
			   'debug'     => sub { $DEBUG = 1; $VERBOSE = 1; },    # Debug mode
			   'help|h'    => sub { pod2usage(-exitval => 1,
							  -verbose => 2); },    # Show help
			   'version'   => sub { VERSION_MESSAGE(); exit(0); }   # Show version
			  );

PRINT_ERROR_THEN_EXIT("Please check the format of the command-line $!") unless ($getoptRes);

# I allow comma-separated args: --arg=toto,tutu is equivalent to --arg=toto --arg=tutu
#debug(Dumper \@arg);
@arg = split(/,/,join(',',@arg));

# Basic Error processing
PRINT_ERROR_THEN_EXIT() unless ($numargs); # At least a file or an option should be mentioned

# Now ready for your job :-)

# Example: show the parameters entered with --arg option
foreach my $a (@arg) { print $a." "; }
print "\n";



#################################################################################
############## ------------------ Sub routines  ------------------ ##############
#################################################################################

######
# Print information in the following form: '[$2] $1' ($2='=>' if not submitted)
# usage: info(text [,title])
##
sub info {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing text argument') unless @_;
    my $prefix = $_[1] ? $_[1] : '=>';
    print "$prefix $_[0]" unless $QUIET;
}

######
# Print verbose information (i.e print only if $VERBOSE is set)
# usage: verbose(text)
##
sub verbose {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing text argument') unless @_;
    print @_ if ${VERBOSE};
}

######
# Print debug information (i.e print only if $DEBUG is set)
# usage: debug(text)
##
sub debug {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing text argument') unless @_;
    info(@_, '['. color("yellow") . 'DEBUG' . color("reset") . ']') if ${DEBUG};
}

######
# Print error message 
# usage: error(text)
##
sub error {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing text argument') unless @_;
    info(@_, '['. color("red") . 'ERROR' . color("reset") . ']');
}

######
# Print warning message 
# usage: warning(text)
##
sub warning {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing text argument') unless @_;
    info(@_, '['. color("magenta") . 'WARNING' . color("reset") . ']');
}

####
# Print error message then exit with error status
# Optionnal parameter $_[0] : error message ('Bad format' by defaut) 
##
sub PRINT_ERROR_THEN_EXIT {
    my $msg = $_[0] ? $_[0] : 'Bad format';
    error "$msg\n";
    exit(1);
}

######
# Ask the user wish to continue. 
##
sub really_continue {
    print "Are you sure you want to continue? [yN] ";
    chomp(my $ans = <STDIN>);
    exit(0) unless ($ans && ($ans =~ /y|yes|1/i));
}

#####
# execute a local command 
# usage: execute(command)
###
sub execute {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing command argument') unless (@_);
    debug('[' . (caller(0))[3] . "] @_\n");
    $SIMULATION_MODE ? 
      print '(', color('bold'), 'simulation', color('reset'), ") @_\n" : system("@_");  
    my $exit_status = $?;
    debug('[' . (caller(0))[3] . "] exit status : $exit_status\n");
    return $exit_status;
}

#####
# execute a command on a remote server by ssh 
# usage: ssh_remote_execute("user@host", port, "command")
###
sub ssh_remote_execute {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] malformed arguments') unless (scalar(@_) >= 3);
    my ($host, $port, @command) = @_;
    my $cmd = "ssh -p $port $host @command 1>/dev/null";
    return execute($cmd);
}

####
# check the presence of the binaries @_ on the local system using 'which'
# usage:  check_binary(bin1 [, bin2 ...]);
##
sub check_binary {
    PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . '] missing argument') unless (@_);
    my $which = "$ENV{'PATH'} which";
    foreach my $app (@_) {
	verbose("=> check availability of the command '$app' on the local system...");
	verbose("\n") if (($DEBUG) || ($SIMULATION_MODE));
       	my $exit_status = execute("which $app 1>/dev/null");
	PRINT_ERROR_THEN_EXIT( '[' . (caller(0))[3] . "] unable to find the application $app on your system") if ($exit_status);
	verbose("\tOK\n");
    }
}

####
# Print script version
##
sub VERSION_MESSAGE {
    print <<EOF;
This is $command v$VERSION.
Copyright (c) %y %U  (http://www-id.imag.fr/~svarrett/)
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
EOF
}

######################## POD documentation ########################
=pod

=head1 NAME

I<%f>, a nice script in perl (no joke ;) )

=head1 SYNOPSIS

      ./%f [options] 

=head1 DESCRIPTION

I<%f> do whatever you implement -- see L<SYNOPSIS>

=head1 OPTIONS

The following options are available:

=over 12

=item B<--debug>

Debug mode. Display debugging information probably only relevant to me ;)

=item B<--dry-run   -n>

Simulate the operations to show what would have been done and/or transferred but do 
not perform any backend actions.

=item B<--help  -h>

Display a help screen and quit.

=item B<--quiet>

Quiet mode. Minimize the number of printed messages and don't ask questions. 
Very useful for invoking this script in a crontab yet use with caution has all 
operations will be performed without your interaction.

=item B<--verbose  -v>

Verbose mode. Display more information

=item B<--version>

Display the version number then quit. 

=back

=head1 BUGS

Please report bugs to %U %a

=head1 AUTHOR

%U -- L<http://varrette.gforge.uni.lu/>

=head1 COPYRIGHT

This  is a free software. There is NO warranty; not even for
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=cut

# POD documentation to be done (Suggested by Damian Conway in Perl Best Practices.)
#Name
#Usage
#Description
#Required arguments
#Options
#Exit status
#Diagnostics
#Configuration
#Dependencies
#Incompatibilities
#Bugs and limitations
#Author
#License and copyright
#Disclaimer of warranty













