#!/usr/bin/env perl
#===================================================================================
#
#         FILE:  ManaMessageReader.pm
#
#        USAGE:  ./ManaMessageReader.pm 
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
#      CREATED:  02/12/06 13:20:52 EST
#     REVISION:  $Id: perl-file-header,v 1.2 2005/04/15 22:48:02 jbalint Exp $
#===================================================================================

use strict;
use warnings;
use vars qw(@EXPORT);

use Data::Dumper;

use ManaConstants;
use ManaUtil;

require Exporter;

@EXPORT = qw(
dcd_player_move
dcd_being_move
dcd_item_visible
dcd_chat_incoming
dcd_warp
);

sub dcd_player_move
{
	my $id = shift;
	my $data = shift;
	my %parsed;
	my @fields = unpack("I S9", $data);
	$data = substr($data, 22); # pass the fields we just read
	if($id == ID_PLAYER_MOVE)
	{
		$parsed{tick} = unpack("I", $data);
		$data = substr($data, 4);
	}
	push(@fields, unpack("S5 I2 S C2", $data));
	$data = substr($data, 22);
	if($id == ID_PLAYER_MOVE)
	{
		my($dstx, $dsty, $srcx, $srcy) = parse_coord_pair(unpack("C5", $data));
		$parsed{x} = $dstx;
		$parsed{y} = $dsty;
		$parsed{srcx} = $srcx;
		$parsed{srcy} = $srcy;
		$data = substr($data, 5);
	}
	else
	{
		my($x, $y, $dir) = parse_coord(unpack("C3", $data));
		$parsed{x} = $x;
		$parsed{y} = $y;
		$parsed{dir} = $dir;
		$data = substr($data, 3);
	}
	push(@fields, unpack("C2", $data));
	$data = substr($data, 2);
	if($id == ID_PLAYER_UPDATE_1 || $id == ID_PLAYER_MOVE)
	{
		$data = substr($data, 1);
	}
	push(@fields, unpack("C2", $data));

	# I S9
	$parsed{id} = shift(@fields);
	$parsed{speed} = shift(@fields);
	$parsed{opt1} = shift(@fields);
	$parsed{opt2} = shift(@fields);
	$parsed{opt3} = shift(@fields);
	$parsed{job} = shift(@fields);
	$parsed{hair} = shift(@fields);
	$parsed{weaponId} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	# S5 I2 S C2
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{hairColor} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	# C2
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	# C2
	$parsed{lvl} = shift(@fields);
	$parsed{unused} = shift(@fields);

	log_trace("Parsed player move: " . Dumper(\%parsed));

	return \%parsed;
}

sub dcd_being_move
{
	my($id, $data) = @_;

	my %parsed;
	my @fields = unpack("I S8", $data);
	$data = substr($data, 20);
	if($id == ID_BEING_MOVE)
	{
		# server tick
		$data = substr($data, 4);
	}
	push(@fields, unpack("S11 C2", $data));
	$data = substr($data, 24);
	if($id == ID_BEING_MOVE)
	{
		my($dstx, $dsty, $srcx, $srcy) = parse_coord_pair(unpack("C5", $data));
		$parsed{x} = $dstx;
		$parsed{y} = $dsty;
		$parsed{srcx} = $srcx;
		$parsed{srcy} = $srcy;
		$data = substr($data, 5);
	}
	else
	{
		my($x, $y, $dir) = parse_coord(unpack("C3", $data));
		$parsed{x} = $x;
		$parsed{y} = $y;
		$parsed{dir} = $dir;
		$data = substr($data, 3);
	}

	push(@fields, unpack("C3", $data));

	# I S8
	$parsed{id} = shift(@fields);
	$parsed{speed} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{job} = shift(@fields);
	$parsed{hair} = shift(@fields);
	$parsed{weaponId} = shift(@fields);
	$parsed{unused} = shift(@fields);

	# S11 C2
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{hairColor} = shift(@fields);
	$parsed{unused} = shift(@fields);

	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	$parsed{unused} = shift(@fields);
	#
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	# C3
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	log_trace("Parsed being move: " . Dumper(\%parsed));

	return \%parsed;
}

sub dcd_item_visible
{
	my($id, $data) = @_;
	my %parsed;
	my @fields = unpack("I S C S2 C4", $data);

	$parsed{id} = shift(@fields);
	$parsed{itemid} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{x} = shift(@fields);
	$parsed{y} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);
	$parsed{unused} = shift(@fields);

	log_trace("Parsed item visible: " . Dumper(\%parsed));
	return \%parsed;
}

sub dcd_chat_incoming
{
	my($id, $data) = @_;
	my %parsed;
	my @fields = unpack("I Z*", $data);
	$parsed{id} = shift(@fields);
	$parsed{message} = shift(@fields);

	log_trace("Parsed incoming chat: " . Dumper(\%parsed));
	return \%parsed;
}

sub dcd_warp
{
	my($id, $data) = @_;
	my %parsed;
	my @fields = unpack("Z16 S S", $data);
	$parsed{map} = shift(@fields);
	$parsed{x} = shift(@fields);
	$parsed{y} = shift(@fields);

	log_trace("Parsed warp: " . Dumper(\%parsed));
	return \%parsed;
}

1;

