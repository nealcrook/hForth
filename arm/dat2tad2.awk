# dat2tad2.awk
# usage: gawk -f dat2tad.awk file.macs_data
# called by makesrc.awk

# reverse order of entries in data file.
# the first entry (set of 3 lines) needs to be at the *end* of the
# file, and vice versa.

# $Id: dat2tad2.awk,v 1.1 1998/10/03 23:47:24 crook Exp $
#
# $Log: dat2tad2.awk,v $
# Revision 1.1  1998/10/03 23:47:24  crook
# Initial revision
#

BEGIN {
  infile = ARGV[1]
  accum = "accum.txt"
  entry_count = 0

  # read whole file into an array and find out how many entries are in the
  # file; 3 lines per entry
  while (getline > 0) {
    line1[entry_count] = $0
    getline
    line2[entry_count] = $0
    getline
    line3[entry_count] = $0
    entry_count++
  }

  print ("Found " entry_count " entries")
  close(infile)

  while (entry_count > 0) {
    print (line1[entry_count]) > accum
    print (line2[entry_count]) > accum
    print (line3[entry_count]) > accum
    entry_count--
  }
  close (accum)
}

{
# it's all done in BEGIN
}
