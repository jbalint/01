#!/usr/bin/env perl
#===================================================================================
#
#         FILE:  ManaUtil.pm
#
#        USAGE:  ./ManaUtil.pm 
#
#     SYNOPSIS:  
#
#  DESCRIPTION:  
#      OPTIONS:  ---
# REQUIREMENTS:  ---
#         BUGS:  ---
#        NOTES:  ---
#       AUTHOR:  Jess Balint (), jbalint@improvedideas.com
#      COMPANY:  ImprovedIdeas
#      VERSION:  1.0
#      CREATED:  02/12/06 13:45:10 EST
#     REVISION:  $Id: perl-file-header,v 1.2 2005/04/15 22:48:02 jbalint Exp $
#===================================================================================

use strict;
use warnings;

use vars qw(@EXPORT);

use IO::File;

require Exporter;

@EXPORT = qw(
parse_coord
parse_coord_pair

log_init
log_destroy

log_trace
log_debug
log_info
log_error
);

################################################################
#
# Coordinate encoding/decoding Functions
#
################################################################
# See messagein.cpp:readCoordinates for details
# Unpacks the x, y, direction coordinates packed in 3 bytes
sub parse_coord
{
	my @data = @_;
	my($cx, $cy, $cdir);
	my $temp = ($data[1] & 0xc0) + (($data[0] & 0xff) << 8);
	$cx = $temp >> 6;
	$temp = ($data[2] & 0xf0) + (($data[1] & 0x3f) << 8);
	$cy = $temp >> 4;
	$cdir = $data[2] & 0xf;
	return($cx, $cy, $cdir);
}

sub parse_coord_pair
{
	my @data = @_;
	my($dstx, $dsty, $srcx, $srcy);
	my $temp = $data[3] + (($data[2] & 0x0f) << 8);
	$dstx = $temp >> 2;
	$dsty = $data[4] + (($data[3] & 0x03) << 8);
	$temp = $data[1] + ($data[0] << 8);
	$srcx = $temp >> 6;
	$temp = $data[2] + (($data[1] & 0x3f) << 8);
	$srcy = $temp >> 4;
	return($dstx, $dsty, $srcx, $srcy);
}

sub pack_coords
{
	my($x, $y, $dir) = @_;
	my @bytes;
	my $temp = $x << 6;
	$bytes[0] = ($temp >> 8) & 0xff;
	$bytes[1] = $temp & 0xff;
	$temp = $y << 4;
	$bytes[1] |= ($temp >> 8) & 0xff;
	$bytes[2] = $temp & 0xff;
	$bytes[2] |= $dir;
	return @bytes;
}

################################################################
#
# Logging Functions
#
################################################################
my $logfile;
my $opts;

sub log_init
{
	my $logname = shift;
	$opts = shift;
	$logfile = new IO::File(">> $logname")
		or die("Cannot open log file: $!");
	$logfile->autoflush(1);
}

sub log_destroy
{
	$logfile->close();
	undef $logfile;
}

sub log_bottom
{
	my $msg = strftime("%F %H:%M:%S ", localtime) . shift() . "\n";
	print {$logfile} $msg;
	if($opts->{logConsole})
	{
		print $msg;
	}
}

sub log_trace
{
	if($opts->{logTrace})
	{
		log_bottom("TRACE: " . shift);
	}
}

sub log_debug
{
	if($opts->{logDebug})
	{
		log_bottom("DEBUG: " . shift);
	}
}

sub log_info
{
	if($opts->{logInfo})
	{
		log_bottom("INFO: " . shift);
	}
}

sub log_error
{
	log_bottom("ERROR: " . shift);
}

1;

