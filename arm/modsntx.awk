# modsntx.awk
# usage: gawk -f modsntx.awk eforth.mac

# convert from MACRO syntax to armasm syntax. In particular:
#
# - strip out the macros. They get processed by makesrc.awk
# - remove : from the end of label names. All labels must start on
#   line 1
# - change DW to DCD
# - change DB to DCB
# - change DS to % 

# $Id$
#
# $Log$

BEGIN {
  fixedfile = ARGV[1] "s"
  print("fixed file is " fixedfile)

  TRUE = -1
  FALSE = 0

  ignore_line = FALSE
  removed_macro_count = 0
}


{
# look for things to change
  if ($0 ~ /ENDM/) {
#print ("End of macro definition")
    ignore_line = FALSE
  } else
    if (ignore_line) {
#print("Ignoring line:" $0)
    } else
#check to see if this line is the start of a macro definition that
#we want to discard. Keep ARM macro definitions, which are lines
#just containing the word MACRO. Eliminate other macro definitions
      if (($2 ~ /MACRO/))  {
print ("Found macro definition " $0)
        removed_macro_count += 1
        ignore_line = TRUE
      } else
	{
# A label is a string that starts in column 1. ARMASM labels must not have a
# colon on the end of them.
	  if ($0 ~ /^[A-Za-z_][0-9A-Za-z_\$]*:/) {
            #print("Label: " $0)
	    if (gsub(/:/,"",$0) != 1) {
	      print ($0)
		print ("^^^ **** Fatal error: found >1 colon in label fixup")
	    }
	  }
# A hex number starts with a digit and ends with an [hH] followed by
# whitespace or comma. ARMASM wants to see them with a leading 0x instead of
# the trailing h. This sequence will only reformat a single hex number in a
# line. I hand-edited hforth source to stop >1hex/line from occurring
#TODO: BUG! the regular expression doesn't match hex followed by cr
	  if (match($0,/[0-9][0-9A-Fa-f]*[hH][, \n\t]/) != 0) {
            #print("hex number in: " RLENGTH, $0)
            $0 = substr($0,1,RSTART-1) "0x" substr($0,RSTART,RLENGTH-2) substr($0,RSTART+RLENGTH-1)
            #print("hex number in: " $0)
	  }
# fix DW
	  gsub(/[ \t]*[dD][wW][ \t]/,"\t\tDCD ",$0)
# fix DB
	    gsub(/[ \t]*[dD][bB][ \t]/,"\t\tDCB ",$0)
# fix DS
	      gsub(/[ \t]*[D][S][ \t]/,"\t\t% ",$0)
# copy the line to output unchanged or changed
		print ($0) > fixedfile
	}
}


END {
   close(fixedfile)
   print ("Removed " removed_macro_count " macro definitions")
}





