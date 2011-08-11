# File::      %f 
# Author::    %U (%a)
# Copyright:: Copyright (c) %y %U (www[%o])
# License::   GPLv3
# 
# Time-stamp: < >
# ------------------------------------------------------------------------------
# = Class: %b
#
# %@
#
# == Parameters:
#
# $param1 (Default: val):: description
#
# == Actions: 
#
# Install and configure %b 
#
# == Requires: 
#
# n/a
#
# == Sample usage:
#
#     import %b
#
# == Warnings
#
# /!\ Always respect the style guide available
# here[http://docs.puppetlabs.com/guides/style_guide]
#
# [Remember: No empty lines between comments and class definition]
#
class %b {

    case $operatingsystem {
        debian, ubuntu:         { include %b::debian }
        redhat, fedora, centos: { include %b::redhat }
        default: {
            fail("Module $module_name is not supported on $operatingsystem")
        }
    }
}

# Class: %b::common
#
# Base class to be inherited by the other %b classes
#
# Note: respect the Naming standard provided here[http://projects.puppetlabs.com/projects/puppet/wiki/Module_Standards]
class %b::common {

}

# Class: %b::debian
#
# Specialization class for Debian systems
class %b::debian { }

# Class: %b::redhat
#
# Specialization class for Redhat systems
class %b::redhat { }







