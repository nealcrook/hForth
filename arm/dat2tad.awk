# dat2tad.awk
# usage: gawk -f dat2tad.awk file.msd
# called by makesrc.awk

# reverse order of entries in data file.
# the first entry (set of 3 lines) needs to be at the *end* of the
# file, and vice versa.

# $Id$
#
# $Log$
# Revision 1.3  1997/03/01 16:38:27  crook
# merge Carey's changes for DOS build environment
#
# Revision 1.2  1997/01/18 16:31:18  crook
# support new input format of 3 lines/entry
#
# Revision 1.1  1997/01/13 09:41:28  crook
# Initial revision

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
  # entries in array are stored from 0
  entry_count--

  while (entry_count >= 0) {
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
