# nocomment.awk
# remove comments from file

# $Id$
#
# $Log$
# Revision 1.1  1997/01/13 09:41:28  crook
# Initial revision
#

BEGIN {
  print ("\\ comments autoremoved by nocomment.awk")
}

{
  # $0 is the whole line
  where = index($0, "\\")
  if (where == 0) {
    print $0
  }
  else if (where == 1) {
  }
  else {
    print (substr($0, 1, where-1))
  }
}


