# nocomment.awk
# remove comments from file

# $Id$
#
# $Log$

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


