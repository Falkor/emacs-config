# File::      <tt>%f</tt> 
# Author::    %U (%a)
# Copyright:: Copyright (c) %y %U (www[%o])
# License::   GPLv3
# 
# ------------------------------------------------------------------------------
# = Class: %b
#
# %@
#
# == Parameters:
#
# $ensure:: *Default*: 'present'. Ensure the presence (or absence) of %b
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
# You can then specialize the various aspects of the configuration,
# for instance:
#
#      class { '%b':
#          ensure => 'present'
#      }
#
# == Warnings
#
# /!\ Always respect the style guide available
# here[http://docs.puppetlabs.com/guides/style_guide]
#
# [Remember: No empty lines between comments and class definition]
#
class %b ( $ensure = $%b::params::ensure ) inherits %b::params {

    info ("Configuring %b (with ensure = ${ensure})")

    if ! ($ensure in [ 'present', 'absent' ]) {
        fail("%b 'ensure' parameter must be set to either 'absent' or 'present'")
    }

    case $::operatingsystem {
        debian, ubuntu:         { include %b::debian }
        redhat, fedora, centos: { include %b::redhat }
        default: {
            fail("Module $module_name is not supported on $operatingsystem")
        }
    }
}

# ------------------------------------------------------------------------------
# = Class: %b::common
#
# Base class to be inherited by the other %b classes
#
# Note: respect the Naming standard provided 
# here[http://projects.puppetlabs.com/projects/puppet/wiki/Module_Standards]
class %b::common {

    # Load the variables used in this module. Check the infiniband-params.pp file
    require %b::params


}

# ------------------------------------------------------------------------------
# = Class: %b::debian
#
# Specialization class for Debian systems
class %b::debian inherits %b::common { }

# ------------------------------------------------------------------------------
# = Class: %b::redhat
#
# Specialization class for Redhat systems
class %b::redhat inherits %b::common { }







