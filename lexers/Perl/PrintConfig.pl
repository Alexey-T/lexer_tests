#!perl
# ============================================================================
#
# Filename:  PrintConfig.pl
#
# Copyright: 2011, BSK Datentechnik GmbH
# Author:    Hans Schmidts
#
# Abstract:
#   Print config info (config for which perl was build)
#
# $Revision: 1.1 $
# $Date: 2011/08/02 15:25:58 $
# $Source: /BSK/BSK_PRJ/scripts/perl/perl_examples/PrintConfig.pl,v $
#
# Modifications:
# 2011-08-02  BSK/HS
# - Initial revision
#
# ============================================================================

use strict;
use warnings;
use Config;
use English; # for $OSNAME; $^O is always available

sub vDumpConfig() {  # debug
  my ($sKey);
  printf "vDumpConfig:\n";
  foreach $sKey (keys(%Config)) {
    print "  \%Config[$sKey] = $Config{$sKey}\n";
  }
} # vDumpConfig


if (@ARGV != 0) {
  print "Print config info (config for which perl was build)\n";
  print "Usage: PrintConfig.pl\n";
  exit 1;
} # wrong arguments


print "\$^O = $^O\n";
print "\$OSNAME = $OSNAME\n";
print "Config.osname = $Config{osname}\n";
# e.g. "cygwin",  "MSWin32"
print "Config.archname = $Config{archname}\n";
print "Config.myarchname = $Config{myarchname}\n";
print "Config.sh = $Config{sh}\n";
print "Config.cf_email = $Config{cf_email}\n";

# vDumpConfig(); # debug
#...

exit 0;

# eof
