#!/usr/bin/perl
use strict;
use warnings;

use Config::Any;

my @files = (
        'path/to/config_file.json',
        'path/to/config.pl',
        'path/to/config.xml'
);

my $config = Config::Any->load_files( { files => \@files } );