# dat2tad.awk
# usage: gawk -f dat2tad.awk file.msd
# called by makesrc.awk

# reverse order of entries in data file.
# the first entry (set of 3 lines) needs to be at the *end* of the
# file, and vice versa.

# $Id$
#
# $Log$
# Revision 1.2  1997/01/18 16:31:18  crook
# support new input format of 3 lines/entry
#
# Revision 1.1  1997/01/13 09:41:28  crook
# Initial revision
#


BEGIN {
  infile = ARGV[1]
  outfile = "accum.txt"
  entry_count = 0

  # find out how many entries in the file; 3 lines per entry
  while ((getline < infile) > 0) {
    entry_count += 1
    getline < infile
    getline < infile
  }

  print ("Found " entry_count " entries")
  close(infile)

  while (entry_count > 0) {

    # go to the first line in the entry
    for (i=1; i<(((entry_count - 1) * 3) + 2); i++) {
      getline < infile
    }

    # append it to the output file
    print ($0) > outfile
    getline < infile
    print ($0) > outfile
    getline < infile
    print ($0) > outfile
    close(infile)
    entry_count -= 1

  }
  close (outfile)
}

{
# it's all done in BEGIN
}
