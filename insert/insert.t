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
# Description : Part of the test suite %@
#               'perldoc Test::More' for more details
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

use Test::More qw( no_plan );

use File::Basename;
use Cwd             'abs_path'

# Retrieve the absolute path to the test directory
my $testdir = dirname(abs_path($0));







