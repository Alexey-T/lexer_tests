#!/usr/bin/env ruby
#--
# Copyright 2006 by Chad Fowler, Rich Kilmer, Jim Weirich and others.
# All rights reserved.
# See LICENSE.txt for permissions.
#++

# The prelude in 1.9.1 injects rubygems.rb into $LOADED_FEATURES
# which prevents the `require 'rubygems'` from actually loading
# the site's version of rubygems. So we have to use it's API
# to get it's prelude out of the way.
#
if RUBY_VERSION =~ /^1\.9\.1/ && defined?(Gem::QuickLoader)
  Gem::QuickLoader.load_full_rubygems_library
end

require 'rubygems'
require 'rubygems/gem_runner'
require 'rubygems/exceptions'

required_version = Gem::Requirement.new ">= 1.8.7"

unless required_version.satisfied_by? Gem.ruby_version then
  abort "Expected Ruby Version #{required_version}, is #{Gem.ruby_version}"
end

args = ARGV.clone

begin
  Gem::GemRunner.new.run args
rescue Gem::SystemExitException => e
  exit e.exit_code
end

