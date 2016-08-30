#!/usr/bin/perl -w

use strict;
use POSIX ('strftime');

sub day_offset_to_date($) {
  my ($day_offset) = @_;

  my $time = time + $day_offset * 24 * 60 * 60;
  return strftime '%Y-%m-%d', localtime $time;
}

sub generate_line {
  my $date = day_offset_to_date int(-400 * rand);

  my $category;
  my $amount;
  if (rand() < 0.5) {
    $category = 'Category 1: 10-20';
    $amount = int(10 + 11 * rand);
  } else {
    $category = 'Category 2: 50-100';
    $amount = int(50 + 51 * rand);
  }

  return "- $date $amount \"$category\" \"unused name\"\n";
}

print generate_line for (1..200);
