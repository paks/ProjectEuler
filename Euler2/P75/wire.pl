#!/usr/bin/perl 
#===============================================================================
#
#         FILE:  wire.pl
#
#        USAGE:  ./wire.pl 
#
#  DESCRIPTION:  P75 Euler project
#
#      OPTIONS:  ---
# REQUIREMENTS:  ---
#         BUGS:  ---
#        NOTES:  ---
#       AUTHOR:  Cesar Mendoza (CM), <cesar.e.mendoza@pioneer.com>
#      COMPANY:  Pioneer HI-Bred INT.
#      VERSION:  1.0
#      CREATED:  08/19/08 09:28:34 CDT
#     REVISION:  ---
#===============================================================================

use strict;
use warnings;

my %hash = ();
my %hash2 = ();
my $limit = 2000000;

for (my $i = 1; $i <= $limit; $i ++) 
{
	$hash{$i} = $i ** 2;
	$hash2{$i ** 2} = $i;
}

my %hash3 = ();
for (my $a = 1; $a <= $limit/2; $a ++) 
{
	for (my $b = 1; $b <= $a; $b ++) 
	{
		my $c = $hash{$a} + $hash{$b};
		if(defined($hash2{$c})) 
		{
			if ($a + $b + $hash2{$c} <= $limit) 
			{
				$hash3{$a + $b + $hash2{$c}} += 1;
			}
		}
	}
}

my $single = 0;
foreach my $d (keys %hash3) 
{
	my $count = $hash3{$d};
	if ($count == 1) 
	{
		$single++;
	}
}

print "$single\n"; 

