# makesrc.awk
# usage: gawk -v dict_pc=1234 -f makesrc.awk eforth.mac_sntx

# bugs:
# d$ didn't get expanded in 2 instances, where there was a label at thr start of
# the line, but modsntx didn't fix it.
# BECAUSE makesrc doesn't split macros onto new lines so a macro could be $2
# instead of $1. Easiest fix is to mod the source
#
# need comma but no space between string and label in colon defn

# $Id$
#
# $Log$
# Revision 1.3  1997/01/18 16:31:18  crook
# change name space file to 3 lines/entry to speed dat2tad
#
# Revision 1.2  1997/01/15 23:24:58  crook
# minor tweaks
#
# Revision 1.1  1997/01/13 09:41:28  crook
# Initial revision
#

BEGIN {
  quot = "\""
  doubquot = "\"\""
  codefile = ARGV[1] "_code"
  datafile = ARGV[1] "_data"
  throwfile = ARGV[1] "_throw"
  source = ARGV[1] "w"
  nameoffset = "- (UNKNOWN)"
  #print("data file is " datafile ", code file is " codefile)

  celll = 4

  slink = 0              # null dictionary link
  flink = 0              # null dictionary link
  envlink = 0            # null dictionary link

  init_dict_pc = dict_pc
  throwoffset = celll
# the throw parameters will get extracted from the source
  throwmsgs = ""         # number of throw messages

  foffset = " "
  soffset = " "

# TODO: express the variables in terms of offsets from their values in
# the original source.

# dict_pc is set on the gawk command line, to be the address
# where the dictionary ends. In the source, this is the value
# of NAMEE. It is the top of memory, minus the space taken up
# by user variables. Find it by setting it high, assembling
# to get the value of NAMEE then fixing up the dict_pc value

  printf("DICTIONARY PC set to %d (0x%x)\n", dict_pc, dict_pc )
}

function split_line(NUM_PARA) {
  # for a line of the form $FRED string_length,string,para3,para4
  # 1. split the the comma-separated list of parameters into separate elements
  # of the array param[]. NUM_PARA is the number of parameters to expect. The
  # string is quoted but may include a comma, which complicates matters.
  # 2. calculate the length of the string and verify. Set calc_length
  # 3. fix up the quotes in the string and escape where necessary
    if (split($2, param, ",") == NUM_PARA) {
      #print("break it up the easy way")
    }
    else {
      #print("break it up the hard, slow way")
      # This is used for definitions that have a comma in the name and for
      # definitions that have extra parameters. For example
      #        $COLON  2,'C,',CComma,_FLINK
      # get:           1  2 3    4      5
      # want:          1  2      3      4
      # proper param[2] is concatenation of 2, a comma, and 3 ..
      param[2] = param[2] "," param[3]
      # copy back to eliminate bogus param[3] and so get code address and link
      param[3] = param[4]
      param[4] = param[5]
    }
  # remove the first and last characters from the name; they are
  # quotes of some kind, but we cannot be sure they are the Right Sort
  calc_length = length(param[2]) - 2
  param[2] = substr(param[2], 2, calc_length) 
  # the string must be enclosed in double quotes. Double quote and dollar
  # characters must be escaped by being doubled up
  gsub(/"/,doubquot,param[2])
  gsub(/\$/,"$$",param[2])
  #print("lex = " param[1] " name = ->" param[2] "<- label = " param[3])
  # lex may include some maths at the front to set compile flag
  # could just grab rh character, but this only works for name lengths <10
  # so the sophisticated way to do it is to look for a numeric string at
  # the end of the string
  match(param[1],/[0-9][0-9]*$/)
  infer_length = substr(param[1],RSTART,RLENGTH)
  if (infer_length != calc_length) {
    print($0)
    print("^^ **** Fatal error; calculated length of " calc_length " but inferred length of " infer_length)
  }
}


function do_code_header(NUM_PARA) {
  split_line(NUM_PARA)

  # do code space stuff
  print("\t\tALIGN") > codefile
  print(param[3]) > codefile

  # do name space stuff
  string_cells = int(calc_length / celll)
  #print(param[2] " requires " string_cells " cells")
  dict_pc -= (( string_cells + 3) * celll)
  printf(";--- org 0x%x %s = 0x%x\n", dict_pc, nameoffset, dict_pc - 232) > datafile
  # link

  if (param[NUM_PARA] == "_SLINK") {
    printf("\t\tDCD\t %s, 0x%x %s ;slink\n", param[3], slink, soffset) > datafile
    slink = dict_pc + (2 * celll)
    soffset = nameoffset # space first time around => offset is 0
  }
  else
  if (param[NUM_PARA] == "_FLINK") {
    #print ("FLink")
    printf("\t\tDCD\t %s, 0x%x %s ;flink\n", param[3], flink, foffset) > datafile
    flink = dict_pc + (2 * celll)
    foffset = nameoffset # space first time around => offset is 0
  }
  else {
    print ("Fatal - unrecognised link name for " $0)
  }
  # length and name. 
  if (param[2] == "\\") {
    #single exception case is backslash, which the assembler cannot handle. Replace it with its ASCII code
    #printf("\t\t************************** found backslash\n")
    printf("\t\tDCB\t" param[1] "," 92 "\n") > datafile
  }
  else {
    printf("\t\tDCB\t" param[1] "," quot param[2] quot "\n") > datafile
  }
}

function do_envir_header() {
  # $ENVIR definitions have 1 comma (2 parameters)
  split_line(2)

  # do code space stuff
  print("\t\tALIGN") > codefile
  printf("ENVIR%x\tbl DoLIST\n", envlink) > codefile

  # do name space stuff
  string_cells = int(calc_length / celll)
  #print(param[2] " requires " string_cells " cells")
  dict_pc -= (( string_cells + 3) * celll)
  printf(";--- org 0x%x %s = 0x%x\n", dict_pc, nameoffset, dict_pc - 232) > datafile
  printf("\t\tDCD\tENVIR%x, 0x%x %s\n", envlink, envlink, nameoffset) > datafile
  envlink = dict_pc + (2 * celll)
  # length and name. 
  printf("\t\tDCB\t" param[1] "," quot param[2] quot "\n") > datafile
}


{
  # look for macro invocations
  if ($1 == "$CODE") {
    #print("code expansion")
    do_code_header(4)
  }
  else
  if ($1 == "$COLON") {
    # print("colon expansion ", $0)
    do_code_header(4)
    print("\t\tbl\tDoLIST\t;Process FORTH words") > codefile
  }
  else
  if ($1 == "$USER") {
    #print("$USER expansion")
    do_code_header(5)
    print("\t\tbl\tDoUSER") > codefile
    #print("$USER:",$0) > codefile
    print("\t\tDCD ", param[4]) > codefile
  }
  else
  if ($1 == "$INSTR") {
    # format of line is: $INSTR  ' some text'  ; optional comment
    # print ("compile an inline string:  --->" $0 "<---")
    # find a substring delimited by single quotes
    match($0,/'[^'][^']*'/)
    param[1] = substr($0, RSTART+1, RLENGTH-2)
    calc_length = length(param[1])
    # Double quote and dollar characters must be escaped by being doubled up
    gsub(/"/,doubquot,param[1])
    gsub(/\$/,"$$",param[1])
    #print(" string = >" param[2] "<, length = " calc_length )
    print ("\t\tDCD\tDoLIT," calc_length ",DoSQuote\t\t\t;$INSTR") > codefile
    print ("\t\tDCB\t" quot param[1] quot ) > codefile
    print ("\t\tALIGN") > codefile
  }
  else
  if ($1 == "$NEXT") {
    #print ("Compile an end-of-definition")
#    print ("\t\tldr\tpc, [fpc], #CELLL\t;goto FPC, incr FPC to nxt word") > codefile
    print ("\t\tb\tudebug\t;micro-debug routine") > codefile
  }
  else
  if ($1 == "$THROWSTR") {
    #print ("expansion of $THROWSTR")
    # format of line is: $THROWSTR   label,' some text'  ; comment
    # $2 is of the form label,' - strip off comma and everything after
    param[1] = substr($2,1,index($2,",") - 1)
    # find a substring delimited by single quotes
    match($0,/'[^'][^']*'/)
    param[2] = substr($0, RSTART+1, RLENGTH-2)
    calc_length = length(param[2])
    # Double quote and dollar characters must be escaped by being doubled up
    gsub(/"/,doubquot,param[2])
    gsub(/\$/,"$$",param[2])
    #print("label = " param[1] " string = >" param[2] "<, length = " calc_length )
    # no need to align. Need length+1 bytes to hold to counted string
    dict_pc -= (calc_length + 1)
    printf(";--- org 0x%x %s = 0x%x.. may be 1,2,3 less\n", dict_pc, nameoffset, dict_pc - 232) > datafile
    printf(param[1] "\tDCB\t" calc_length "," quot param[2] quot "\n\n") > datafile
    printf (";--- org 0x%x\n",init_dict_pc - throwoffset) > throwfile
    printf ("\t\tDCD\t%s\n\n",param[1]) > throwfile
    throwoffset += celll
    print (";Expansion of $THROWSTR for " param[1]) > codefile
  }
  else
  if ($1 == "$STR") {
    #print ("expansion of $STR")
    # format of line is: $STR   label,' some text'  ; comment
    #print ("compile an inline string:  --->" $0 "<---")
    # $2 is of the form label,' - strip off comma and everything after
    param[1] = substr($2,1,index($2,",") - 1)
    # find a substring delimited by single quotes
    match($0,/'[^'][^']*'/)
    param[2] = substr($0, RSTART+1, RLENGTH-2)
    calc_length = length(param[2])
    # Double quote and dollar characters must be escaped by being doubled up
    gsub(/"/,doubquot,param[2])
    gsub(/\$/,"$$",param[2])
    #print("label = " param[1] " string = >" param[2] "<, length = " calc_length )
    # no need to align. Need length+1 bytes to hold to counted string
    dict_pc -= (calc_length + 1)
    printf(";--- org 0x%x %s = 0x%x\n", dict_pc, nameoffset, dict_pc - 232) > datafile
    printf(param[1] "\tDCB\t" calc_length "," quot param[2] quot "\n\n") > datafile
    print (";Expansion of $STR for " param[1]) > codefile
  }
  else
  if ($1 == "$ALIGN") {
    #print ("$ALIGN")
    print ("\t\tALIGN") > codefile
  }
  else
  if ($1 == "$CONST") {
    #print("$CONST expansion")
    do_code_header(5)
    print("\t\tbl\tDoCONST") > codefile
    #print("$CONST:",$0) > codefile
    print("\t\tDCD ", param[4]) > codefile
  }
  else
  if ($1 == "$VALUE") {
    #print("$VALUE expansion")
    do_code_header(4)
    print("\t\tbl\tDoVALUE") > codefile
    #print("$VALUE:",$0) > codefile
    print("\t\tDCD _VAR") > codefile
    print("_VAR\t\tSETA _VAR + CELLL") > codefile
  }
  else
  if ($1 == "$VAR") {
    #print("$VAR expansion")
    do_code_header(4)
    print("\t\tbl\tDoCONST") > codefile
    #print("$VAR:",$0) > codefile
    print("\t\tDCD _VAR") > codefile
    print("_VAR SETA _VAR + CELLL") > codefile
  }
  else
  if ($1 == "$ENVIR") {
    #print ("$ENVIR expansion")
    do_envir_header()
  }
  else
  if ($1 == "$THROWTABLE") {
    #print ("$THROWTABLE expansion")
    split($2, param, ",")
    throwmsgs = param[2]
    nameoffset = "- (" throwmsgs "*CELLL)"
    printf (";--- Throw table\n%s\n\n", param[1]) > throwfile
  }
  else
  if ($1 == "$ALIGN_NAME") {
    # decrement dict_pc such that it is aligned to the cell boundary
    printf("$ALIGN_NAME expansion changed dict_pc from 0x%x", dict_pc )
    dict_pc = dict_pc - dict_pc % 4
    printf(" to 0x%x\n", dict_pc )
  }
  else
  if ($1 == "$GET_NAME") {
    # parameter is a label that is to be assigned to the current value
    # of dict_pc and an offset to be applied to the pc
    # $2=offset in quotes, $3=name. Note no commas separating the parameters
    printf("$GET_NAME expansion.. dict_pc - 0xE8 is 0x%x\n", dict_pc - 232)
    offset2 = substr($2,2,length($2)-2)
    printf("%s\tEQU 0x%x %s %s;get_name\n", $3, dict_pc, nameoffset, offset2) > codefile
  }
  else {
    #print ("no expansion so just echo the line")
    print $0 > codefile
  }
}


END {
   close(datafile)
   close(throwfile)
   system("cat " throwfile " " datafile "> mucca.txt")
   system("gawk -f dat2tad.awk mucca.txt")
   # that makes the file accum.txt
   print (";---------------------------------") > codefile
   print (";Dictionary") > codefile
   print (";---------------------------------") > codefile
   # turn off listing and align to start of dictionary; if we leave the
   # listing on, it produces pages and pages of padding DEFs.
   printf ("\t\tOPT 2\n") > codefile
   printf ("\t\t%% %d - . - (NumTHROWMsgs*CELLL)\t\t;pad to start of dictionary\n", dict_pc) > codefile
   printf ("\t\tOPT 1\n\n") > codefile

   printf ("LASTENV\t\tEQU\t 0x%x %s\n", envlink, nameoffset) > codefile
   printf ("LASTSYSTEM\t\EQU\t 0x%x %s\n", slink, nameoffset) > codefile
   printf ("LASTFORTH\t\EQU\t 0x%x %s\n", flink, nameoffset) > codefile
   printf ("NTOP\t\tEQU\t 0x%x %s\n\n", dict_pc, nameoffset) > codefile
   close(codefile)
   system("cat " codefile " accum.txt > " source)
   system("rm -f accum.txt")
   print ("\t\tEND") >> source
   print("Created unified expanded source, " source)
   close(source)
}








