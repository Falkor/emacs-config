##############################################################################
# File      : %f 
#
# Copyright (c) %y %U %a
#               %o
# $Id$ 
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
package %b;

use strict;
use warnings;

=head1 NAME

%b - The great new %b!

=cut

BEGIN {
        use Exporter   ();
        our ($VERSION, @ISA, @EXPORT, @EXPORT_OK);
        # Module version
        $VERSION = sprintf "1.%%03d", '$Rev$' =~ /(\d+)/g;

	# Module export list
        @ISA         = qw(Exporter);
	# Items to export into callers namespace by default. 
	# /!\ DO NOT export names by default without a very good reason. 
	#     Use EXPORT_OK instead.
        @EXPORT      = qw(func1);
	@EXPORT_OK   = qw($Var1);
}

=head1 SYNOPSIS

    use %b;
    func1();

    use %b qw('Var1')
    $Var1

=head1 DESCRIPTION

Short description of what %b does.

=head1 GLOBAL VARIABLES

=head2 $Var1

=cut

our $Var1 = "value1";

=head1 FUNCTIONS

=head2 func1

=cut

sub func1() {
     print "hello\n"; 
}

=head1 AUTHOR

%U C<< %a >>

=head1 COPYRIGHT & LICENSE

Copyright (c) %y %U, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of the module %b
