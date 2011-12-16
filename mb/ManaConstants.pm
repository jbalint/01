#!/usr/bin/env perl
#===================================================================================
#
#         FILE:  ManaConstants.pm
#
#        USAGE:  ./ManaConstants.pm 
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
#      CREATED:  02/12/06 13:43:54 EST
#     REVISION:  $Id: perl-file-header,v 1.2 2005/04/15 22:48:02 jbalint Exp $
#===================================================================================

use strict;
use warnings;

use vars qw(@EXPORT);

use constant ID_LOGIN => 0x64;
use constant ID_LOGIN_RESP => 0x69;

use constant ID_SERVER_SELECT => 0x65;
use constant ID_SERVER_SELECT_RESP => 0x6b;

use constant ID_CHARACTER_SELECT => 0x66;
use constant ID_CHARACTER_SELECT_RESP => 0x71;

use constant ID_MAP_LOGIN => 0x72;
use constant ID_MAP_LOGIN_RESP => 0x73;
use constant ID_MAP_LOADED => 0x7d;

use constant ID_CHAT_MESSAGE => 0x8c;
use constant ID_CHAT_INCOMING => 0x8d;
use constant ID_CHAT_GM => 0x8e;

use constant ID_WALK => 0x85;
use constant ID_WALK_RESP => 0x87;

use constant ID_ITEM_VISIBLE => 0x9d;
use constant ID_ITEM_DROPPED => 0x9e;
use constant ID_ITEM_PICKUP => 0x9f;
use constant ID_ITEM_REMOVED => 0xa1;

use constant ID_BEING_VISIBLE => 0x78;
use constant ID_BEING_MOVE => 0x7b;
use constant ID_BEING_REMOVE => 0x80;
use constant ID_BEING_STOP_WALKING => 0x88;
use constant ID_BEING_ACTION_DO => 0x89;
use constant ID_BEING_ACTION_DONE => 0x8a;
use constant ID_BEING_NAME_REQUEST => 0x94;
use constant ID_BEING_NAME_RESPONSE => 0x95;

use constant ID_PLAYER_LOOK_CHANGE => 0x0119;

use constant ID_PLAYER_UPDATE_1 => 0x01d8;
use constant ID_PLAYER_UPDATE_2 => 0x01d9;
use constant ID_PLAYER_MOVE => 0x01da;
use constant ID_PLAYER_WARP => 0x91;

use constant ID_SERVER_MSG_UNKNOWN2 => 0x7c;

use constant STAT_STR => 0x0d;
use constant STAT_AGI => 0x0e;
use constant STAT_VIT => 0x0f;
use constant STAT_INT => 0x10;
use constant STAT_DEX => 0x11;
use constant STAT_LUK => 0x12;

require Exporter;

@EXPORT = qw(
);

1;

