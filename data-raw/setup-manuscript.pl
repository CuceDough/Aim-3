#!/usr/bin/perl
#setup-manuscript.pl
# sets up configuration files for Cuc's manuscript runs
#To use:
#  1. Create directory "sims" and subdirectories "sims/0" and "sims/1".
#  2. Put the flute executable in a directory called "code".
#  3. Put the data in a directory called "data".
#  4. Create a file called "run-number" in sims/0 and sims/1 that contains "0" on the first line.

# The way the TIV and LAIV are distributed to different groups is weird.
# FluTE has no way to specify coverage levels by age group.
# To vaccinate x% of an age group with LAIV and y% with TIV:
#   1. I use "vaccinedata" to restrict (1-x)% of the age group from getting LAIV. This will cause x% of the group to be vaccinated.
#   2. I use "vaccinedata" to restrict ((1-x)-y)/(1-x) of the age group from getting TIV. In Flute, after x% of the age group is vaccinated with LAIV, we want some fraction of the _remaining_ people to get vaccinated with TIV. The formula above makes this fraction y% of the whole age cohort.
#   3. I created a separate "vaccine" for the 65+ population, which is TIV with relative efficacy of 70%. The elderly can only get vaccinated with this "alternative" TIV, and not regular TIV or LAIV. That is, I defined a new vaccine that only the elderly can get that is less efficacious than TIV rather than vaccinating them with regular TIV and figuring out how to make this vaccine have different efficacies in different age groups.

my $rundir = "sims";

my %baseconfig;   # default parameter set
$baseconfig{"R0"} = 1.4;
$baseconfig{"seed"} = 1;
$baseconfig{"datafile"} = la;
$baseconfig{"logfile"} = 0;
$baseconfig{"prestrategy"} = "prevaccinate";
$baseconfig{"reactivestrategy"} = "none";
$baseconfig{"vaccinationfraction"} = 0.0;
$baseconfig{"seedinfected"} = 9;       # infected people from LAX?
$baseconfig{"seedinfecteddaily"} = 1;  # infected people arrive every day

my %config; # modified version of baseconfig

# save config file with n different seeds
sub saveconfig(@) {
    my $filename = $_[0];
#    foreach $seed (1,2,3,4,5,6,7,8,9,10) {
    for (my $seed=1; $seed<=200; $seed++) {
	open(OUTFILE, ">$filename-$seed");
	$config{"label"} = "$filename-$seed";
	$config{"seed"} = $seed;
	foreach $var (sort keys %config) {
	    print OUTFILE $var . " " . $config{$var} . "\n";
	}
	close(OUTFILE);
    }
}

# base cases (0 interventions)
my @rlist = (1.2,1.3,1.4);
system("cd $rundir/0; ln -s ../../data/la-*dat .");
system("cd $rundir/0; ln -s ../../code/flute .");
foreach $r (@rlist) {
    print $r . "\n";
    %config = %baseconfig;
    $config{"R0"} = $r;
    saveconfig("$rundir/0/config.la." . $config{"R0"} . "-base" );
}

# pre-vaccination
system("cd $rundir/1; ln -s ../../data/la-*dat .");
system("cd $rundir/1; ln -s ../../code/flute .");
my @coveragelaiv = (0.1040,0.1404,0.1858,0.2313,0.2767,0.3222,0.3676,0.4131,0.4585,0.5040,0.5495,0.5949); # laiv coverage level for school children, 12 scenarios
my @coveragetiv  = (0.1560,0.1596,0.1642,0.1687,0.1733,0.1778,0.1824,0.1869,0.1915,0.1960,0.2005,0.2051); # tiv coverage level for school children, 12 scenarios

foreach $r (@rlist) {
    for (my $coveragenum=0; $coveragenum<=$#coveragetiv; $coveragenum++) {
	foreach $hom ("hom","het") {
	    %config = %baseconfig;
	    $config{"R0"} = $r;
	    $config{"vaccinationfraction"} = "1"; #$coverage;
	    my $laiv = 1.0-$coveragelaiv[$coveragenum];
	    my $tiv = ($laiv-$coveragetiv[$coveragenum])/$laiv;
# Try to vaccinate everyone, but use vaccine restrictions to limit coverage
	    if ($hom eq "het") {
		$config{"vaccinedata 0"} = "0.3 0.3 0.57 1.0 0.8480 " . $laiv . " 0.9640 0.9280 1.0 0"; # FluMist, bad match
		$config{"vaccinedata 1"} = "0.3 0.2 0.14 1.0 0.7311 " . $tiv  . " 0.9440 0.8836 1.0 0"; # inactivated, bad match
		$config{"vaccinedata 2"} = "0.2042 0.2 0.0953 1.0 1.0 1.0 1.0 1.0 0.5 0"; # inactivated 65+, bad match
	    } elsif ($hom eq "hom") {
		$config{"vaccinedata 0"} = "0.4 0.5 0.83 1.0 0.8480 " . $laiv . " 0.9640 0.9280 1.0 0"; # FluMist, good match
		$config{"vaccinedata 1"} = "0.4 0.4 0.67 1.0 0.7311 " . $tiv  . " 0.9440 0.8836 1.0 0"; # inactivated, good match
		$config{"vaccinedata 2"} = "0.2478 0.4 0.4151 1.0 1.0 1.0 1.0 1.0 0.5 0"; # inactivated 65+, good match
	    }
	    saveconfig("$rundir/1/config.la." . $config{"R0"} . "-" . $hom . $coveragenum);
	}
    }
}
