#!/usr/bin/env perl
#==============================================================================
#
#         FILE:  bot.pl
#
#        USAGE:  ./bot.pl 
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
#      CREATED:  02/12/06 00:36:58 EST
#     REVISION:  $Id: perl-file-header,v 1.2 2005/04/15 22:48:02 jbalint Exp $
#==============================================================================

use lib ".";
use strict;
use warnings;

use Data::Dumper;
use Event;
use IO::Socket::INET;
use POSIX;
use Term::ReadLine;

use ManaConstants;
use ManaMessageReader;

use constant MAX_USER_PASS_LEN => 24;

use constant SERVER_ADDRESS => "animesites.de:6901";

use constant DEBUG => 1;

$Data::Dumper::Indent = 0;
$|++;

# Gets the following options:
# mainServer
# mainPort
# mapPort
# logConsole
# logTrace
# logDebug
# logInfo
# charId
# TODO: add other session ids, etc
# char - reference to character hash, created upon login
# coordx
# coordy
# coorddir
# itemSteal - whether or not to steal remote items
# followId - player (or being?) id we are currently following
# players ->
# 	id ->
# 		lvl ->
# 		name ->
# 		x ->
# 		y ->
# beings ->
# 	id ->
# 		x ->
# 		y ->
# 		visible -> last time visible, or 0 if now
# sitting - whether or not player is sitting
my %globalOpts;
$globalOpts{logConsole} = 0;

$globalOpts{logTrace} = 0;
$globalOpts{logDebug} = 1;
$globalOpts{logInfo} = 1;

$globalOpts{itemSteal} = 1;
$globalOpts{followId} = 0;
$globalOpts{sitting} = 0;

#if(DEBUG)
#{
#	$globalOpts{logConsole} = 1;
#}

# from the beginning of src/net/network.cppp
my @packetLength = (
   10,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
# 0x0040
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0, 55, 17,  3, 37, 46, -1, 23, -1,  3,108,  3,  2,
    3, 28, 19, 11,  3, -1,  9,  5, 54, 53, 58, 60, 41,  2,  6,  6,
# 0x0080
    7,  3,  2,  2,  2,  5, 16, 12, 10,  7, 29, 23, -1, -1, -1,  0,
    7, 22, 28,  2,  6, 30, -1, -1,  3, -1, -1,  5,  9, 17, 17,  6,
   23,  6,  6, -1, -1, -1, -1,  8,  7,  6,  7,  4,  7,  0, -1,  6,
    8,  8,  3,  3, -1,  6,  6, -1,  7,  6,  2,  5,  6, 44,  5,  3,
# 0x00C0
    7,  2,  6,  8,  6,  7, -1, -1, -1, -1,  3,  3,  6,  6,  2, 27,
    3,  4,  4,  2, -1, -1,  3, -1,  6, 14,  3, -1, 28, 29, -1, -1,
   30, 30, 26,  2,  6, 26,  3,  3,  8, 19,  5,  2,  3,  2,  2,  2,
    3,  2,  6,  8, 21,  8,  8,  2,  2, 26,  3, -1,  6, 27, 30, 10,
# 0x0100
    2,  6,  6, 30, 79, 31, 10, 10, -1, -1,  4,  6,  6,  2, 11, -1,
   10, 39,  4, 10, 31, 35, 10, 18,  2, 13, 15, 20, 68,  2,  3, 16,
    6, 14, -1, -1, 21,  8,  8,  8,  8,  8,  2,  2,  3,  4,  2, -1,
    6, 86,  6, -1, -1,  7, -1,  6,  3, 16,  4,  4,  4,  6, 24, 26,
# 0x0140
   22, 14,  6, 10, 23, 19,  6, 39,  8,  9,  6, 27, -1,  2,  6,  6,
  110,  6, -1, -1, -1, -1, -1,  6, -1, 54, 66, 54, 90, 42,  6, 42,
   -1, -1, -1, -1, -1, 30, -1,  3, 14,  3, 30, 10, 43, 14,186,182,
   14, 30, 10,  3, -1,  6,106, -1,  4,  5,  4, -1,  6,  7, -1, -1,
# 0x0180
    6,  3,106, 10, 10, 34,  0,  6,  8,  4,  4,  4, 29, -1, 10,  6,
   90, 86, 24,  6, 30,102,  9,  4,  8,  4, 14, 10,  4,  6,  2,  6,
    3,  3, 35,  5, 11, 26, -1,  4,  4,  6, 10, 12,  6, -1,  4,  4,
   11,  7, -1, 67, 12, 18,114,  6,  3,  6, 26, 26, 26, 26,  2,  3,
# 0x01C0
    2, 14, 10, -1, 22, 22,  4,  2, 13, 97,  0,  9,  9, 29,  6, 28,
    8, 14, 10, 35,  6,  8,  4, 11, 54, 53, 60,  2, -1, 47, 33,  6,
   30,  8, 34, 14,  2,  6, 26,  2, 28, 81,  6, 10, 26,  2, -1, -1,
   -1, -1, 20, 10, 32,  9, 34, 14,  2,  6, 48, 56, -1,  4,  5, 10,
# 0x0200
   26,  0,  0,  0, 18,  0,  0,  0,  0,  0,  0, 19,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
);

log_init("TMW_Log", \%globalOpts);

my $mainConnection = login("hotstuff259", "4222");
#my $mainConnection = login("seamanor", "none");

Event->io(fd => \*STDIN, cb => \&cmd_line);
Event->io(fd => $mainConnection, cb => \&server_event);
Event->timer(interval => .5, cb => \&follow);

my $rc = Event::loop;

log_info("Done at $rc");

log_destroy();

if(defined($mainConnection))
{
	$mainConnection->close();
}

# this should be a worker, not a direct function call/loop
sub walk
{
	# stand to walk if we're sitting
	# TODO : func for this
	if($globalOpts{sitting})
	{
		my $req = pack("S I C", ID_BEING_ACTION_DO, 0, 3);
		print {$mainConnection} $req;
		$globalOpts{sitting} = 0;
	}

	my($x, $y) = @_;

	log_debug(sprintf("Walk dest set to %d/%d", $x, $y));

	## old, NORMAL way to do it
	#$globalOpts{coordx} = $x;
	#$globalOpts{coordy} = $y;
	#my $req = pack("S C3", ID_WALK,
	#	pack_coords($x, $y, $globalOpts{coorddir}));
	#print {$mainConnection} $req;
	while($globalOpts{coordx} != $x || $globalOpts{coordy} != $y)
	{
		if($x > $globalOpts{coordx})
		{
			$globalOpts{coordx}++;
		}
		elsif($x < $globalOpts{coordx})
		{
			$globalOpts{coordx}--;
		}

		if($y > $globalOpts{coordy})
		{
			$globalOpts{coordy}++;
		}
		elsif($y < $globalOpts{coordy})
		{
			$globalOpts{coordy}--;
		}

		# TODO: temporarily promoted to DEBUG, should be trace
		log_debug(sprintf("Walking to %d/%d",
			$globalOpts{coordx}, $globalOpts{coordy}));
		my $req = pack("S C3", ID_WALK,
			pack_coords($globalOpts{coordx}, $globalOpts{coordy},
				$globalOpts{coorddir}));
		print {$mainConnection} $req;
	}
}

sub follow
{
	if($globalOpts{followId})
	{
		my $pl = $globalOpts{players}{$globalOpts{followId}};

		if(!defined($pl))
		{
			log_error("Follow id $globalOpts{followId} not found in players");
			$globalOpts{followId} = 0;
			return;
		}

		if(abs($globalOpts{coordx} - $pl->{x}) > 2 or
			abs($globalOpts{coordy} - $pl->{y}) > 2)
		{
			my $newx = $pl->{x} - 2;
			my $newy = $pl->{y} - 2;
			walk($newx, $newy);
		}
	}
}

sub server_event
{
	my $ev = shift;
	my $sock = $ev->w->fd;
	my $msg = sock_read_message($sock);

	# TODO: need a standard way to send messages
	if($msg->{id} == ID_PLAYER_UPDATE_2 or
		$msg->{id} == ID_PLAYER_UPDATE_1 or
		$msg->{id} == ID_PLAYER_MOVE)
	{
		my $pmsg = dcd_player_move($msg->{id}, $msg->{data});
		# request to get the player name if we don't have it
		if(!defined($globalOpts{players}{$pmsg->{id}}))
		{
			my $req = pack("S I", ID_BEING_NAME_REQUEST, $pmsg->{id});
			print {$mainConnection} $req;
		}
		# keep these up to date
		$globalOpts{players}{$pmsg->{id}}{x} = $pmsg->{x};
		$globalOpts{players}{$pmsg->{id}}{y} = $pmsg->{y};
		$globalOpts{players}{$pmsg->{id}}{lvl} = $pmsg->{lvl};
	}
	elsif($msg->{id} == ID_BEING_MOVE or
		$msg->{id} == ID_BEING_VISIBLE)
	{
		my $pmsg = dcd_being_move($msg->{id}, $msg->{data});
		my $b = $globalOpts{beings}{$pmsg->{id}};
		if(!defined($b))
		{
			log_debug(sprintf("Being add %d at %d/%d",
				$pmsg->{id}, $pmsg->{x}, $pmsg->{y}));
			$b = {};
			$globalOpts{beings}{$pmsg->{id}} = $b;
		}
		$b->{x} = $pmsg->{x};
		$b->{y} = $pmsg->{y};
		$b->{visible} = 0;
	}
	elsif($msg->{id} == ID_BEING_REMOVE)
	{
		# TODO: decode for simple?
		my $pmsg = {};
		$pmsg->{id} = unpack("I", $msg->{data});
		my $b = $globalOpts{beings}{$pmsg->{id}};
		# this could be for monster or player
		if(!defined($b) && !defined($globalOpts{players}{$pmsg->{id}}))
		{
			log_error("Being removed, no record: " . $pmsg->{id});
		}
		else
		{
			$b->{visible} = time();
		}
	}
	elsif($msg->{id} == ID_BEING_NAME_RESPONSE)
	{
		my($id, $name) = unpack("I Z*", $msg->{data});
		log_info("Added player $name for id $id " .
			"(lvl=$globalOpts{players}{$id}{lvl})");
		$globalOpts{players}{$id}{name} = $name;
	}
	elsif($msg->{id} == ID_ITEM_VISIBLE or
		$msg->{id} == ID_ITEM_DROPPED)
	{
		my $pmsg = dcd_item_visible($msg->{id}, $msg->{data});
		log_info(sprintf("Retrivieving item at id=%d,x=%d,y=%d",
			$pmsg->{id}, $pmsg->{x}, $pmsg->{y}));
		# get item
		if($globalOpts{itemSteal})
		{
			my $req = pack("S I", ID_ITEM_PICKUP, $pmsg->{id});
			print {$mainConnection} $req;
		}
	}
	elsif($msg->{id} == ID_PLAYER_LOOK_CHANGE)
	{
		# don't really care
	}
	elsif($msg->{id} == ID_SERVER_MSG_UNKNOWN2)
	{
		# not sure why we get these all the time
	}
	elsif($msg->{id} == ID_ITEM_REMOVED)
	{
		# don't really care
	}
	elsif($msg->{id} == ID_BEING_ACTION_DONE)
	{
		# sit, stand, attack
		# nothing for now
	}
	elsif($msg->{id} == ID_BEING_STOP_WALKING)
	{
		# the main game has the handler code for this
		# commented out too
	}
	elsif($msg->{id} == ID_WALK_RESP)
	{
		# nothing significant
	}
	elsif($msg->{id} == ID_CHAT_GM)
	{
		# TODO: display it
	}
	elsif($msg->{id} == ID_CHAT_INCOMING)
	{
		my $pmsg = dcd_chat_incoming($msg->{id}, $msg->{data});

		if(!defined($globalOpts{players}{$pmsg->{id}}))
		{
			log_error("Unknown player sent message: " + $pmsg->{message});
		}

		# the player name is already part of the msg string
		log_info(sprintf("Incoming message, %s",
			$pmsg->{message}));
	}
	elsif($msg->{id} == ID_PLAYER_WARP)
	{
		my $pmsg = dcd_warp($msg->{id}, $msg->{data});
		$globalOpts{coordx} = $pmsg->{x};
		$globalOpts{coordy} = $pmsg->{y};

		log_info(sprintf("Changed map to %s coord %d/%d",
			$pmsg->{map}, $pmsg->{x}, $pmsg->{y}));

		# ACK
		my $req = pack("S", ID_MAP_LOADED);
		print {$mainConnection} $req;
	}
	else
	{
		log_info(sprintf("Unhandled server message id=%x", $msg->{id}));
	}
}

sub cmd_line
{
	my $ev = shift;
	my $fd = $ev->w->fd;
	my $cmd = <$fd>;
	return unless(defined($cmd));
	chomp($cmd);
	if($cmd eq "quit")
	{
		Event::unloop("User quit");
		return;
	}
	elsif($cmd =~ /say.*/)
	{
		my $chat = $cmd;
		$chat =~ s/^say\s+//;
		$chat = $globalOpts{char}{name} . " : " . $chat;
		my $len = length($chat) + 1;
		# TODO: make this somewhere else
		my $msg = pack("S S Z" . $len, ID_CHAT_MESSAGE, $len + 4, $chat);
		print {$mainConnection} $msg;
	}
	elsif($cmd eq "coord")
	{
		printf("Coordinates x=%d y=%d dir=%d\n",
			$globalOpts{coordx}, $globalOpts{coordy},
			$globalOpts{coorddir});
	}
	elsif($cmd =~ /^mv\s+/)
	{
		my $rest = $cmd;
		$rest =~ s/^mv\s+//;
		my($newx, $newy);
		if($rest =~ m/(\d+)\s+(\d+)/)
		{
			($newx, $newy) = ($1, $2);
		}
		elsif($rest eq "up")
		{
			$newx = $globalOpts{coordx};
			$newy = $globalOpts{coordy} - 1;
		}
		elsif($rest eq "dn")
		{
			$newx = $globalOpts{coordx};
			$newy = $globalOpts{coordy} + 1;
		}
		elsif($rest eq "lt")
		{
			$newx = $globalOpts{coordx} - 1;
			$newy = $globalOpts{coordy};
		}
		elsif($rest eq "rt")
		{
			$newx = $globalOpts{coordx} + 1;
			$newy = $globalOpts{coordy};
		}

		walk($newx, $newy);
	}
	elsif($cmd =~ /^being/)
	{
		if($cmd eq "being")
		{
			print "Beings\n";
			for(keys(%{$globalOpts{beings}}))
			{
				my $b = $globalOpts{beings}{$_};
				printf(" id = %d x = %2d y = %2d vis = %d\n",
					$_, $b->{x}, $b->{y}, $b->{visible});
			}
		}
	}
	elsif($cmd =~ /^player/)
	{
		# TODO: have a standard way to print players
		if($cmd eq "player")
		{
			print "Currently known players:\n";
			for(keys(%{$globalOpts{players}}))
			{
				my $p = $globalOpts{players}{$_};
				printf(" Id=%d Level=%3d Name=%s\n", $_, $p->{lvl}, $p->{name});
			}
		}
		elsif($cmd =~ m/^player\s+(\d+)$/)
		{
			my $p = $globalOpts{players}{$1};
			printf("Player Id=%d Level=%d Name=%s\n",
				$1, $p->{lvl}, $p->{name});
		}
		elsif($cmd =~ m/^player\s+(.*)$/)
		{
			my $name = $1;
			my $found = 0;
			for(keys(%{$globalOpts{players}}))
			{
				my $p = $globalOpts{players}{$_};
				if($p->{name} eq $name)
				{
					$found = 1;
					printf("Player Id=%d Level=%d Name=%s\n",
						$_, $p->{lvl}, $p->{name});
					last;
				}
			}

			if(!$found)
			{
				printf("Could not find details for player '%s'\n", $name);
			}
		}
	}
	elsif($cmd =~ /^attack\s+/)
	{
		my $id = $cmd;
		$id =~ s/^attack\s+//;
		my $b = $globalOpts{beings}{$id};
		if(!defined($b))
		{
			log_info("Being id " . $id . " not defined.");
		}
		elsif($b->{visible} > 0)
		{
			log_info("Being id " . $id . " not visible.");
		}
		else
		{
			# TODO: temporary clear follow id so it can go attack
			#my $fid = $globalOpts{followId};
			#Event->timer(at => time + 5,
			#	cb => sub { $globalOpts{followId} = $fid });
			$globalOpts{followId} = 0;
			walk($b->{x} - 2, $b->{y});
			sleep(1);
			my $req = pack("S I C", ID_BEING_ACTION_DO, $id, 0);
			print {$mainConnection} $req;
		}
	}
	elsif($cmd =~ /^set\s+/)
	{
		$cmd =~ s/^set\s+//;
		my $var;
		if($cmd =~ m/^(\w+)\s*/)
		{
			$var = $1;
		}

		if(!defined($var))
		{
			printf("Invalid command syntax\n");
		}
		elsif(!exists($globalOpts{$var}))
		{
			printf("Option '%s' unknown\n", $var);
		}
		elsif($cmd =~ m/\w+\s+(.*)\s*$/)
		{
			printf("Setting $var '%s'->'%s'\n", $globalOpts{$var}, $1);
			$globalOpts{$var} = $1;
		}
		else
		{
			printf("%s=%s\n", $var, $globalOpts{$var});
		}
	}
	elsif($cmd eq "respawn")
	{
		# TODO constants
		my $req = pack("S C", 0xb2, 0);
		print {$mainConnection} $req;
	}
	elsif($cmd =~ "sit")
	{
		my $type;
		# TODO: constant for type
		if(!$globalOpts{sitting})
		{
			$type = 2;
			$globalOpts{sitting} = 1;
		}
		else
		{
			$type = 3;
			$globalOpts{sitting} = 0;
		}
		my $req = pack("S I C", ID_BEING_ACTION_DO, 0, $type);
		print {$mainConnection} $req;
	}
	else
	{
		printf("Unrecognized command\n");
	}
}

sub login
{
	my $username = shift;
	my $password = shift;

	my $acctId;
	my $sessId1;
	my $sessId2;

	my $sex;

	# This is the first part of the login. we send the username
	# and password and obtain some session ids
	{
		log_debug("Sending login request ($username/$password)");
		my $loginrequest = pack("s i Z" . MAX_USER_PASS_LEN .
					" Z" . MAX_USER_PASS_LEN . " C", ID_LOGIN, 0,
				substr($username, 0, MAX_USER_PASS_LEN),
				substr($password, 0, MAX_USER_PASS_LEN), 0);

		log_trace(sprintf("MSG_OUT: Login stage1 request len=%d '%s'",
			length($loginrequest), join(" ", unpack("H*", $loginrequest))));

		my $sock = new IO::Socket::INET(
			PeerAddr => SERVER_ADDRESS,
			Proto => "tcp");

		if(!defined($sock))
		{
			log_error("Cannot connect for stage1 login: $!");
			die("Cannot connect for stage1 login: $!");
		}

		print {$sock} $loginrequest;

		my $resp = sock_read_message($sock);
		if($resp->{id} != ID_LOGIN_RESP)
		{
			log_error(sprintf("Login stage1 FAILED, " .
				"resp message id is 0x%x", $resp->{id}));
			$sock->close();
			return;
		}
		my @rest; # timestring, sex, srv addr, srv port,
				  # srv name, srv users, byte2
		($sessId1, $acctId, $sessId2, @rest) =
			unpack("I3 Z30 C i s Z20 i C2", $resp->{data});
		$sex = $rest[1];

		$globalOpts{mainServer} = join(".", unpack("C*", pack("i", $rest[2])));
		$globalOpts{mainPort} = $rest[3];
		log_debug("Main server address is " .
			"'$globalOpts{mainServer}:$globalOpts{mainPort}'");

		$sock->close();
		log_debug(sprintf("Login stage 1 complete, " .
			"sessids=(0x%x, 0x%x), acctid=0x%x", $sessId1, $sessId2, $acctId));
	}

	# login stage 2, we connect to the main server
	{
		my $loginrequest = pack("s I3 s C", ID_SERVER_SELECT,
			$acctId, $sessId1, $sessId2, 0, $sex);

		log_trace(sprintf("MSG_OUT: Login stage2 request len=%d '%s'",
			length($loginrequest), join(" ", unpack("H*", $loginrequest))));

		my $sock = new IO::Socket::INET(
			PeerAddr => $globalOpts{mainServer},
			PeerPort => $globalOpts{mainPort},
			Proto => "tcp");

		if(!defined($sock))
		{
			log_error("Cannot connect for stage2 login: $!");
			die("Cannot connect for stage2 login: $!");
		}

		print {$sock} $loginrequest;

		# read the famous "mysterious 4 bytes"
		read_from_socket($sock, 4);

		my $resp = sock_read_message($sock);

		if($resp->{id} != ID_SERVER_SELECT_RESP)
		{
			log_error(sprintf("Login stage2 FAILED, " .
				"resp message id is 0x%x", $resp->{id}));
			$sock->close();
			return;
		}

		my %char;
		my @res = unpack("Z20 I5 Z8 I3 Z2 S16 Z24 C8", $resp->{data});
		shift(@res); # remove the 20 unused bytes
		$char{id} = $res[0];
		$char{xp} = $res[1];
		$char{gp} = $res[2];
		$char{jobXp} = $res[3];
		$char{jobLvl} = $res[4];
		$char{hp} = $res[10];
		$char{maxHp} = $res[11];
		$char{mp} = $res[12];
		$char{maxMp} = $res[13];
		$char{speed} = $res[14];
		# we skipped some
		$char{lvl} = $res[18];
		$char{name} = $res[26];
		$char{str} = $res[27];
		$char{agi} = $res[28];
		$char{vit} = $res[29];
		$char{int} = $res[30];
		$char{dex} = $res[31];
		$char{luk} = $res[32];
		$char{num} = $res[33];

		$globalOpts{char} = \%char;

		log_debug("Login stage2 complete. " .
			"Character info: " . Dumper(\%char));

		# Stage 3
		$loginrequest = pack("s C", ID_CHARACTER_SELECT, 0);

		log_trace(sprintf("MSG_OUT: Login stage3 request len=%d '%s'",
			length($loginrequest), join(" ", unpack("H*", $loginrequest))));

		print {$sock} $loginrequest;

		$resp = sock_read_message($sock);

		if($resp->{id} != ID_CHARACTER_SELECT_RESP)
		{
			log_error(sprintf("Login stage3 FAILED, " .
				"resp message id is 0x%x", $resp->{id}));
			$sock->close();
			return;
		}

		my($char_id, $map_path, $map_address, $map_port) =
			unpack("I Z16 I S", $resp->{data});
		$map_address = join(".", unpack("C*", pack("i", $map_address)));
		log_debug(sprintf("Login stage3 complete cid=%d,mpath=%s,maddr=%s,mport=%d",
			$char_id, $map_path, $map_address, $map_port));
		$globalOpts{charId} = $char_id;
		$globalOpts{mapServer} = $map_address;
		$globalOpts{mapPort} = $map_port;

		$sock->close();
	}

	# Final stage, build the final request and log in
	my $mainSocket;
	{
		my $loginrequest = pack("S I4 C", ID_MAP_LOGIN,
			$acctId, $globalOpts{charId}, $sessId1, $sessId2, $sex);

		log_trace(sprintf("MSG_OUT: Login stageF request len=%d '%s'",
			length($loginrequest), join(" ", unpack("H*", $loginrequest))));

		$mainSocket = new IO::Socket::INET(
			PeerAddr => $globalOpts{mapServer},
			PeerPort => $globalOpts{mapPort},
			Proto => "tcp");

		if(!defined($mainSocket))
		{
			log_error("Cannot connect for stageF login: $!");
			die("Cannot connect for stageF login: $!");
		}

		print {$mainSocket} $loginrequest;

		# Don't know, check connection.cpp mapLogin()
		# read the famous "mysterious 4 bytes"
		read_from_socket($mainSocket, 4);

		my $resp = sock_read_message($mainSocket);

		if($resp->{id} != ID_MAP_LOGIN_RESP)
		{
			log_error(sprintf("Login stageF FAILED, " .
				"resp message id is 0x%x", $resp->{id}));
			$mainSocket->close();
			return;
		}

		my($serverTick, @rest) = unpack("I C3 Z2", $resp->{data});
		# grab the coordinates in the crazy way
		my($cx, $cy, $cdir);
		{
			my $temp = ($rest[1] & 0xc0) + (($rest[0] & 0xff) << 8);
			$cx = $temp >> 6;
			$temp = ($rest[2] & 0xf0) + (($rest[1] & 0x3f) << 8);
			$cy = $temp >> 4;
			$cdir = $rest[2] & 0xf;
		}
		log_debug("Login stageF complete " .
			"coords($cx, $cy, $cdir) tick=%serverTick");
		$globalOpts{coordx} = $cx;
		$globalOpts{coordy} = $cy;
		$globalOpts{coorddir} = $cdir;

		# acknowledge
		print {$mainSocket} pack("S", ID_MAP_LOADED);

		log_debug("StageF map login complete");
	}
	return $mainSocket;
}

sub read_from_socket
{
	my $sock = shift;
	my $bytes = shift;
	my $buffer;
	my $read;
	for(my $left = $bytes; $left > 0; $left -= $read)
	{
		$read = $sock->read($buffer, $left, $bytes - $left);
		if($read == 0)
		{
			log_error("Socket EOF");
			die("Socket EOF");
		}
		elsif($read < 0)
		{
			log_error("Socket error: $!");
			die("Socket error: $!");
		}
	}
	return $buffer;
}

sub sock_read_message
{
	my $sock = shift;
	my $msg;
	my $id = read_from_socket($sock, 2);

	$id = unpack("s", $id);

	my $len = get_defined_msg_len($id);

	if(!defined($len))
	{
		log_error(sprintf("Msg received with undefined len. " .
					"Msg id = 0x%x", $id));
		return;
	}
	elsif($len == -1)
	{
		$len = read_from_socket($sock, 2);
		# subtract 2 for the 2 length bytes
		$len = unpack("s", $len) - 2;
	}

	# subtract 2 for the 2 id bytes
	$len -= 2;

	$msg = read_from_socket($sock, $len);

	log_trace(sprintf("MSG_IN: id=0x%x len=%d '%s'",
		$id, $len, unpack("H*", $msg)));

	return({id => $id, data => $msg});
}

sub get_defined_msg_len
{
	my $msgid = shift;
	if($msgid > $#packetLength)
	{
		log_error(sprintf("Message id 0x%x not defined", $msgid));
		return;
	}
	my $len = $packetLength[$msgid];
	log_trace(sprintf("Defined len for msg id 0x%x is %d", $msgid, $len));
	return $len;
}

