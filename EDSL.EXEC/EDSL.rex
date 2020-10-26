  /* --------------------  rexx procedure  ------------------- */
  ver = '1.25'
  /* Name:      edsl                                           |
  |                                                            |
  | Function:  Enhanced Data Set List ISPF Applications        |
  |                                                            |
  | Syntax:    %edsl                                           |
  |        or  %edsl action member                             |
  |        or  %edsl action group member                       |
  |        or  %edsl group  (defaults to Select group)         |
  |                                                            |
  | Usage Notes:  1. Non-standard commands only work on        |
  |                  individual datasets                       |
  |                                                            |
  | Dependencies: All ISPF panels are inline                   |
  |                                                            |
  | Author:      Lionel B. Dyck                                |
  | Contributor: John Kalinich                                 |
  |                                                            |
  | History:  (most recent on top)                             |
  |    1.25    10/26/20 LBD - Correct PNS for Find in popup    |
  |                         - add find string field to popup   |
  |                         - improve find results message     |
  |    1.24    10/23/20 JK  - Add / primary command            |
  |    1.23    10/23/20 JK  - Change EDSLG pop-up size/location|
  |    1.22    10/21/20 LBD - Enhance update process           |
  |    1.21    10/20/20 LBD - Correct panel logic in edslg     |
  |    1.20    10/20/20 LBD - Set MEMLIST to 9 chars and remove|
  |                           option from panel and tutorial   |
  |    1.19    10/20/20 LBD - Correct command line options     |
  |    1.18    10/20/20 LBD - Fix duplicate action with cmd    |
  |    1.17    10/20/20 LBD - Use DSN for Group if Group null  |
  |                           and only 1 dsn                   |
  |    1.16    10/20/20 LBD - Tutorial cleanup/enhancements    |
  |    1.15    10/19/20 LBD - Performance/other improvements   |
  |    1.14    10/19/20 LBD - Add display info to popups       |
  |    1.13    10/17/20 LBD - Fix reorder check and add popup  |
  |                           for type null selections         |
  |    1.12    10/17/20 LBD - Add popups for update/review     |
  |                         - Add popup for / selection        |
  |    1.11    10/17/20 LBD - Clean up tutorial panels         |
  |    1.10    10/16/20 LBD - Enhance Tutorial                 |
  |                         - Increase from 8 to 16 datasets   |
  |    1.09    10/16/20 LBD - Enhance Move - and +             |
  |                         - Improve tutorial                 |
  |    1.08    10/15/20 LBD - If rows = 0 then do insert       |
  |                           and issue message about it       |
  |    1.07    10/15/20 LBD - Add bottom of table ztdmark      |
  |    1.06    10/13/20 LBD - Allow update of Group only       |
  |                         - Truncate group name to 8 for     |
  |                           dslist refname                   |
  |    1.05    10/10/20 LBD - Tutorial Update for OMVS updates |
  |                         - Add MD (move down) and M# to     |
  |                           move to a specific location      |
  |            10/09/20 LBD - Support OMVS files/directories   |
  |                           - only 1 per group               |
  |            10/07/20 LBD - Use 1st 8 usable chars of group  |
  |                           name for DSList                  |
  |            09/30/20 LBD - Use Group if provided for 1 dsn  |
  |            09/29/20 LBD - Cleanup display name             |
  |                         - Alias C to U for update/change   |
  |            09/26/20 LBD - Randomize dd name                |
  |            09/20/20 LBD - Enable row select with Enter     |
  |                         - Enable any command on type D     |
  |                         - Allow dsn.* with no group        |
  |                     JK  - Add History Command to display   |
  |                           the change history of EDSL       |
  |            09/11/20 LBD - Save member list size            |
  |                         - Improve entry with group name    |
  |            09/09/20 LBD - Support * in dataset names       |
  |            09/07/20 LBD - Support Find and bug fixes       |
  |            09/06/20 LBD - Small bug fix if used in SYSPROC |
  |            09/05/20 LBD - Use Group name for DSLIST        |
  |            09/04/20 LBD - Make L an alias of U             |
  |                         - Option O to open in DSList       |
  |            09/01/20 LBD - Option Memlist Size added        |
  |                         - Always run applid=ISR            |
  |            08/30/20 LBD - Use 9 char memlist               |
  |            08/10/20 LBD - Open table shared                |
  |            08/04/20 LBD - Support Edit/Browse/View on zcmd |
  |            08/03/20 LBD - Refinement                       |
  |            08/01/20 LBD - Creation                         |
  |                                                            |
  * ---------------------------------------------------------- */

  arg options

  parse value '' with null called
  dd = '$'right(time("s"),7,"0")     /* create unique ddname */

  signal on failure
  signal on syntax

  /* ----------------------- *
  | Define ISPF Environment |
  * ----------------------- */
  Address ISPEXEC
  'control errors return'

  /* ------------------------ *
  | Always run in applid ISR |
  * ------------------------ */
  'vget (zapplid)'
  if zapplid /= 'ISR' then do
    parse source with x cmd .
    'select cmd('cmd options') newappl(isr)'
    exit
  end

  /* ---------------------- *
  | Check for Debug option |
  * ---------------------- */
  if options = 'DEBUG' then do
    trace '?i'
    options = null
  end

  /* ---------------------------- *
  | Open the data set list table |
  * ---------------------------- */
  call open_table

  /* ---------------------------- *
  | Dynamically load ISPF Panels |
  * ---------------------------- */
  load_info = loadispf()

  /* --------------------------- *
  | Process any provide options |
  * --------------------------- */
  if options /= null then do
    if words(options) = 1 then do
      options = 'S' options
    end
    call do_options
  end

  /* ----------------------- *
  | Display Selection Table |
  * ----------------------- */
  table_top = 1
  ztdsels = 0
  do forever
    'tbquery edsl rownum(rows)'
    if rows = 0 then call do_insert
    parse value null with rsel rmem zcmd
    if ztdsels > 1 then
    'tbdispl edsl'
    else do
      'tbtop edsl'
      'tbskip edsl number('table_top')'
      'tbdispl edsl panel(edsl)'
      table_top = ztdtop
    end
    if rc > 4 then call done

    /* --------------------------- *
    | Process Enter for selection |
    * --------------------------- */
    if rowid > 0
    then if zcmd = null
    then if rsel = null
    then do
      'tbtop edsl'
      'tbskip edsl number('rowid')'
      rsel = 'S'
    end

    /* ---------------------------- *
    | Process any primary commands |
    * ---------------------------- */
    TRACE
    if zcmd /= null then do
    Select
      When abbrev('/',zcmd,1) = 1 then do
        parse value '' with zcmd fstring
        call pfshow 'off'
        'Addpop Row(4) column(15)'
        'Display Panel(edspo)'
        'rempop'
        call pfshow 'reset'
        if zcmd = 'F' then
           if fstring /= null
              then zcmd = zcmd fstring
      end
      Otherwise nop
    end
    Select
      When abbrev('INSERT',zcmd,1) = 1 then call do_insert '1'
      When abbrev('FIND',word(zcmd,1),1)   = 1 then call do_find
      When abbrev("HISTORY",word(zcmd,1),1) = 1 then do
        zcmd = ''
        call do_history
      end
      When words(zcmd) = 2 | words(zcmd) = 3 then do
        options = zcmd
        call do_options
        parse value '' with options rsel zcmd
      end
      Otherwise nop
    end
    end
    else
    /* ------------------------------- *
    | Process row selection commands  |
    | selection value process all     |
    | selection value null only a few |
    * ------------------------------- */
    if rsel = '/' then do
      call pfshow 'off'
      'Addpop Row(4) column(15)'
      if edstype /= null
      then 'Display Panel(edslo)'
      else 'Display Panel(edslos)'
      'rempop'
      call pfshow 'reset'
      rsel = zcmd
      zcmd = null
    end
    if rsel /= null then do
      if edstype /= null then
      Select
        When rsel = 'B' then call do_bev
        When rsel = 'D' then 'tbdelete edsl'
        When rsel = 'E' then call do_bev
        When rsel = 'V' then call do_bev
        When rsel = 'I' then call do_insert
        When rsel = 'M' then do
          rsel = 'M-1'
          call do_MoveRow
        end
        When rsel = 'MD' then do
          rsel = 'M1'
          call do_MoveRow
        end
        when left(rsel,1) = 'M' then call do_MoveRow
        When rsel = 'O' then call do_OpenDSL
        When rsel = 'R' then call do_review
        When rsel = 'S' then call do_bev
        When rsel = 'U' then call do_update
        When rsel = 'C' then call do_update
        Otherwise call do_other
      end
      else
      Select
        When rsel = 'U' then call do_update
        When rsel = 'C' then call do_update
        When rsel = 'D' then 'tbdelete edsl'
        When rsel = 'I' then call do_insert
        When rsel = 'M' then do
          rsel = 'M-1'
          call do_MoveRow
        end
        When rsel = 'MD' then do
          rsel = 'M1'
          call do_MoveRow
        end
        when left(rsel,1) = 'M' then call do_MoveRow
        Otherwise call do_other
      end
    end
    rsel = null
    zcmd = null
  end

  /* --------------------------------------------------------- *
  | Done so close the table, free the dynamic ispf resources, |
  | and then exit.                                            |
  * --------------------------------------------------------- */
Done:
  'tbclose edsl library(isptabl)'
  x = dropispf(load_info)
  exit

  /* ------------------------ *
  | Process the Find command |
  * ------------------------ */
Do_Find:
  fstring = translate(subword(zcmd,2))
  'tbtop edsl'
  fhit = 0
  row = 0
  do forever
    'tbskip edsl'
    row = row + 1
    if rc > 4 then leave
    if pos(fstring,translate(edsdisp edsgrp edsdsn)) > 0 then
    do
      fhit = 1
      table_top = row
      zedsmsg = null
      zedlmsg = fstring 'found in row' row
      'setmsg msg(isrz001)'
      return
    end
  end
  if fhit = 0 then do
    'tbtop edsl'
    zedsmsg = 'Not Found'
    zedlmsg = fstring 'not found in the EDSL table.'
    'setmsg msg(isrz001)'
    return
  end

  /* ------------------------ *
  | Process unknown commands |
  * ------------------------ */
Do_Other:
  if edstype = null then return
  if edstype /= 'D' then do
    zedsmsg = null
    zedlmsg = rsel 'is invalid on a group.'
    'setmsg msg(isrz001)'
    return
  end
  else do
    'control display save'
    'select cmd('rsel edsdsn')'
    'control display restore'
  end
  return

  /* ------------------------------ *
  | Process execution time options |
  * ------------------------------ */
Do_Options:
  zcmd = null
  if words(options) = 3 then do
    parse value options with rsel rgroup rmem
    rgroup = translate(rgroup)
  end
  if words(options) = 2 then do
    parse value options with rsel rgroup
    rmem = null
  end
  if length(rsel) > 1 then rsel = left(rsel,1)
  'tbtop edsl'
  do forever
    'tbskip edsl'
    if rc > 0 then do
      zedsmsg = null
      zedlmsg = 'Group' rgroup 'not found...'
      'setmsg msg(isrz001)'
      return
    end
    if translate(edsgrp) = rgroup then do
      if length(rmem) < 8
      then rmem = rmem'*'
      else rmem = left(rmem,7)'*'
      Select
        When pos(rsel,'BEV') > 0 then call do_bev
        When rsel = 'O' then call do_opendsl
        When rsel = 'R' then call do_review
        When rsel = 'C' then call do_update
        When rsel = 'U' then call do_update
        Otherwise call do_bev
      end
      return
    end
  end
  return

  /* -------------------------------------------------- *
  | Process Browse, Edit, View commands and            |
  | if type L (refList) then open as a reference list. |
  * -------------------------------------------------- */
Do_BEV:
  if edstype = 'L' then do
    call do_openDSL
    return
  end
  if edstype = 'O' then do
    call do_omvs
    return
  end
  if edstype = 'D' then do
    cp = pos(rsel,'BEV')
    if cp = 0 then cp = 2
    cp = word('Browse Edit View',cp)
    'control display save'
    if pos('(',edsdsn) = 0 then do
      "lminit dataid(did) dataset("edsdsn")"
      if rc > 0 then call do_smsg
      'Memlist dataid('did') Default('rsel') field(9)'
      if rc > 0 then call do_smsg
      'lmfree dataid('did')'
    end
    else cp 'Dataset('edsdsn')'
    'control display restore'
    return
  end
  Address tso ,
    'alloc f('dd') shr ds('edsdsn') reuse'
  'control display save'
  'lminit dataid(did) ddname('dd')'
  if rc > 0 then call do_smsg
  if rmem /= null
  then bev_mem = 'Member('rmem')'
  else bev_mem = null
  'Memlist dataid('did') Default('rsel')' bev_mem 'field(9)'
  'lmfree dataid('did')'
  address tso 'free f('dd')'
  'control display restore'
  return

  /* --------------------------- *
  | Review an entry (view only) |
  * --------------------------- */
Do_Review:
  save_sgrp = edsgrp
  call pfshow 'off'           /* make sure pfshow is off */
  'Addpop row(1) column(40)'
  'Display Panel(edslg)'
  'Rempop'
  call pfshow 'reset'         /* restore pfshow setting */
  return

  /* ------------------- *
  | Process row inserts |
  * ------------------- */
Do_Insert:
  arg loc
  if datatype(edsloc) /= 'NUM' then edsloc = 0
  if loc = null
  then edsloc = edsloc + 10
  else edsloc = 1
  parse value null with zcmd edsgrp edsdsn edsdisp
  call pfshow 'off'           /* make sure pfshow is off */
  'addpop row(1) column(4)'
  'display panel(edsle)'
  drc = rc
  'rempop'
  call pfshow 'reset'         /* restore pfshow setting */
  if drc > 0
  then if ztdrows = 0 then call done
  else return
  'tbadd edsl'
  'tbsave edsl library(isptabl)'
  'tbsort edsl fields(edsloc,n,a)'
  call re_number_table
  return

  /* ------------ *
  | Move selected |
  * ------------- */
Do_MoveRow:
  change = substr(rsel,2)
  Select
    When left(change,1) = '-' then change = substr(change,2) * -100
    When left(change,1) = '+' then change = substr(change,2) * 100
    otherwise change = change * 100
  end
  if change > 0
  then change = change + 10
  else change = change - 10
  edsloc = edsloc + change
  if edsloc < 1 then edsloc = 999999
  'tbput edsl'
  'tbsave edsl library(isptabl)'
  'tbsort edsl fields(edsloc,n,a)'
  call re_number_table
  return

  /* ----------------------------------- *
  | Renumber the table for easy sorting |
  * ----------------------------------- */
Re_Number_Table:
  'tbtop edsl'
  loc = 100
  do forever
    'tbskip edsl'
    if rc > 0 then return
    edsloc = loc
    'tbput edsl'
    'tbsave edsl library(isptabl)'
    loc = loc +100
  end
  return

  /* -------------- *
  | Issue messages |
  * -------------- */
Do_SMsg:
  zedsmsg = zerrsm
  zedlmsg = zerrlm
  'setmsg msg(isrz001)'
  return

  /* ---------------------------------- *
  | Update the selected row's elements |
  * ---------------------------------- */
Do_Update:
  zcmd = null
  call pfshow 'off'           /* make sure pfshow is off */
  do forever
  'addpop row(1) column(4)'
  'display panel(edsle)'
  drc = rc
  'rempop'
  call pfshow 'reset'         /* restore pfshow setting */
  if drc > 0 then return
  'tbput edsl'
  'tbsave edsl library(isptabl)'
  end
  return

  /* ---------------------------- *
  | Process an OMVS File Request |
  * ---------------------------- */
Do_OMVS:
  Select
    When pos(rsel,'VE') > 0 then
    Address TSO 'OEdit' "'"edsdsn"'"
    Otherwise
    Address TSO 'OBrowse' "'"edsdsn"'"
  end
  Return

  /* --------------------------------------------- *
  | Create and then open the ISPF Reference List. |
  * --------------------------------------------- */
Do_OpenDSL:
  if edstype = 'O' then do
    call do_omvs
    return
  end
  if edstype = 'D' then do
    zedsmsg = ''
    zedlmsg = 'OpenDSL is not allowed for a Dataset Entry.'
    'setmsg msg(isrz001)'
    return
  end
  'tbopen isrplist share write'
  'tbtop isrplist'
  'tbskip isrplist'
  refname = 'EDSL'   /* default */
  if rc = 0 then
  refname = translate(edsgrp)
  refname = translate(refname,' ',"'-"'"')
  if words(refname) > 1 then do
    parse value refname with g1 g2 g3 g4
    if length(g1) < 4
    then refname = left(g1''g2''g3''g4,8)
    else refname = g1
    refname = strip(refname)
  end
  if refname = '' then refname = 'EDSL'
  if length(refname) > 8 then refname = left(refname,8)
  do forever
    'tbget isrplist'
    if zcurtb = refname then do
      'tbdelete isrplist'
      leave
    end
    'tbskip isrplist'
    if rc > 0 then leave
  end
  'tbtop isrplist'
  'tbvclear isrplist'
  zcurtb   = refname
  dsadescp = 'Enhanced DSL List' refname
  dsapet01 = word(edsdsn,1)
  dsapet02 = word(edsdsn,2)
  dsapet03 = word(edsdsn,3)
  dsapet04 = word(edsdsn,4)
  dsapet05 = word(edsdsn,5)
  dsapet06 = word(edsdsn,6)
  dsapet07 = word(edsdsn,7)
  dsapet08 = word(edsdsn,8)
  dsapet09 = word(edsdsn,9)
  dsapet10 = word(edsdsn,10)
  dsapet11 = word(edsdsn,11)
  dsapet12 = word(edsdsn,12)
  dsapet13 = word(edsdsn,13)
  dsapet14 = word(edsdsn,14)
  dsapet15 = word(edsdsn,15)
  dsapet16 = word(edsdsn,16)
  dsactime = date('o')
  if zcurtb /= null then
  if dsapet01 /= null then do
    'TBAdd ISRPlist'
    'TBSave ISRPlist'
    'TBClose ISRPlist'
  end
  'Select Pgm(isrdslst) Parm(DSL' zcurtb') suspend scrname(dslist)'
  return

  /* --------------------------------- *
  | History routine                   |
  * --------------------------------- */
do_history:
  hist. = ''
  hist.1 = 'Extended DSLIST (EDSL) Change History'
  hist.2 = ' '
  line = 1
  x = sourceline(line)
  do until pos(' | History:',x) > 0
    line = line + 1
    x = sourceline(line)
    iterate
  end
  tail = 3
  do until pos(' * ----',x) > 0
    parse value x with '|' hist.tail '|'
    line = line + 1
    tail = tail + 1
    x = sourceline(line)
  end
  hist.0 = tail - 1
  call do_view_stem hist
  drop hist.
  return

  /* --------------------------------- *
  | ISPF View Stem routine            |
  * --------------------------------- */
do_view_stem:
  parse arg stem
  Address TSO
  'alloc f('dd') unit(vio) new reuse space(1,1) tracks',
    'lrecl(80) recfm(f b) blksize(0) dsorg(ps)'
  'execio * diskw' dd '(finis stem' stem'.'

  Address ISPExec
  'lminit dataid(id) ddname('dd') enq(exclu)'
  if rc /= 0 then do
    zedsmsg = 'Error'
    zedlmsg = 'Error.  LMINIT failed for VIO output file'
    'setmsg msg(isrz001)'
    exit
  end
  'view   dataid('id')'
  'lmfree dataid('id')'
  Address TSO 'free f('dd')'
  return

open_table:
  'tbopen edsl library(isptabl) write share'
  if rc = 8 then
  'tbcreate edsl names(edstype edsdisp edsgrp edsdsn edsloc)' ,
    'write share library(isptabl)'
  return

Failure:
Syntax:
  say 'Error encountered at statement:' sigl
  say errortext(rc)
  say sourceline(sigl)
  return

  /* ------------------------------------------------------ *
  | The pfshow routine will:                               |
  | 1. check to see the passed option                      |
  | 2. if Off then it will save the current pfshow setting |
  |    - save the current setting                          |
  |    - turn off pfshow                                   |
  | 3. if the option is Reset then it will                 |
  |    - test if pfshow was on and turn it back on         |
  * ------------------------------------------------------ */
pfshow:
  if zpfshow = 'OFF' then return
  arg pfkopt
  if pfkopt = 'RESET' then do
    if pfkeys = 'ON' then
    'select pgm(ispopf) parm(FKA,ON)'
  end
  if pfkopt = 'OFF' then do
    'vget (zpfshow)'
    pfkeys = zpfshow
    if pfkeys /= 'OFF' then
    'select pgm(ispopf) parm(FKA,OFF)'
  end
  return

/* start of inline elements
>Start
>Panel edsl
)Attr
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low)
)Body Expand(\\)
+\-\%Enhanced Data Set List+@ver +\-\
%Command ===>_zcmd                \ \%Scroll ===>_edsc+
+
%Select     T   Dataset/Group
)Model
$rsel     +@z +@edsdisp                                                 +
)Init
 .cursor = &zcmd
 .help = edslh
 .zvars = '(edstype)'
 if (&edsc EQ &Z)
     &edsc = 'CSR'
 if (&ztdrows = 0)
    &rsel = I
    &zedsmsg = &z
    &zedlmsg = 'No groups in the table. Enter one now.'
    .msg = isrz001
    .resp = ENTER
*REXX(* ztdrows ztdmark)
  ztdmark = center(' (EDSL Bottom of List' ztdrows+0' entries) ',79,'*')
*ENDREXX
)Proc
 &rowid = .csrrow
 vput (edsc) profile
)End
>Panel edspo
)Attr Default(%+_)
  _ type( input) intens(low ) caps(on ) just(left ) hilite(uscore)
  + type(text) intens(low) skip(on)
  ] type(output) caps(off) pas(on) intens(high) color(white) hilite(uscore)
  @ type(output) caps(off)
)Body Window(48,8)
%Command ===>_z
+
+]I+Insert  - insert a row into the table
+]H+History - display the change history of EDSL
+]F+Find    - find the provided string
+   Find:_fstring                 +
+
           +Or%F3+to cancel
)Init
 &zwinttl = 'EDSL Primary Commands:'
 .zvars = '(zcmd)'
 .cursor = zcmd
 .help = edslh
 &I = I
 &F = F
 &H = H
)Proc
 if (&zcmd EQ F)
    ver (&fstring,nb)
)PNTS
 FIELD(I)  VAR(ZCMD) VAL('I')
 FIELD(F)  VAR(ZCMD) VAL('F')
 FIELD(H)  VAR(ZCMD) VAL('H')
)End
>Panel edslo
)Attr Default(%+_)
  _ type( input) intens(low ) caps(on ) just(left ) hilite(uscore)
  + type(text) intens(low) skip(on)
  ] type(output) caps(off) pas(on) intens(high) color(white) hilite(uscore)
  @ type(output) caps(off)
)Body Window(39,11)
+Enter Selection:_z+
+
+ Group:@edsdisp
+
+]B+Browse            +]R+Review (List)
+]D+Delete group      +]S+Select (Open)
+]E+Edit              +]U+Update
+]I+Insert            +]V+View
+]O+Open DSList/UDList
+
           +Or%F3+to cancel
)Init
 &zwinttl = 'EDSL Selection Options:'
 .zvars = '(zcmd)'
 .cursor = zcmd
 .help = edslh
 &B = B
 &D = D
 &E = E
 &I = I
 &O = O
 &R = R
 &S = S
 &U = U
 &V = V
)Proc
)PNTS
 FIELD(B)  VAR(ZCMD) VAL('B')
 FIELD(D)  VAR(ZCMD) VAL('D')
 FIELD(E)  VAR(ZCMD) VAL('E')
 FIELD(I)  VAR(ZCMD) VAL('I')
 FIELD(O)  VAR(ZCMD) VAL('O')
 FIELD(R)  VAR(ZCMD) VAL('R')
 FIELD(S)  VAR(ZCMD) VAL('S')
 FIELD(U)  VAR(ZCMD) VAL('U')
 FIELD(V)  VAR(ZCMD) VAL('V')
)End
>Panel edslos
)Attr Default(%+_)
  _ type( input) intens(low ) caps(on ) just(left ) hilite(uscore)
  + type(text) intens(low) skip(on)
  ] type(output) caps(off) pas(on) intens(high) color(white) hilite(uscore)
  @ type(output) caps(off)
)Body Window(39,9)
+Enter Selection:_z+
+
+ Group:@edsdisp
+
+]D+Delete group
+]I+Insert
+]U+Update
+
           +Or%F3+to cancel
)Init
 &zwinttl = 'EDSL Selection Options:'
 .zvars = '(zcmd)'
 .cursor = zcmd
 .help = edslh
 &D = D
 &I = I
 &U = U
)Proc
)PNTS
 FIELD(D)  VAR(ZCMD) VAL('D')
 FIELD(I)  VAR(ZCMD) VAL('I')
 FIELD(U)  VAR(ZCMD) VAL('U')
)End
>Panel edslh
)Attr
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low)
)Body Expand(\\)
+Tutorial\-\%Enhanced Data Set List+\-\Tutorial
%Command ===>_zcmd
+
+The Enhanced Data Set List dialog makes it easy to access
+
    %*+individual dataset or OMVS file  (Browse/Edit/View)
    %*+groups of datasets               (DSList or Browse/Edit/View)
    %*+OMVS directory                   (UDList)
+
+Panel Fields:%Select+  Row selection field
+
+             %T+       Row type:  %D+Dataset
+                                  %G+Group
+                                  %L+DSList
+                                  %O+OMVS
+
+             %Dataset/Group+Dataset name, Group name, OMVS Directory/File
+
%Note:+ Dataset Groups will create/replace groups within the existing ISPF
+       Personal Data Set List that is used by ISPF 3.4 (DSList).
+
+\-\Press%Enter+to continue the Tutorial\-\
)Init
)Proc
 &zcont = edslh1
)End
>Panel edslh1
)Attr Default(%^_)
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low) intens(low)
)Body Expand(\\)
^Tutorial\-\%Enhanced Data Set List^\-\Tutorial
%Command ===>_zcmd
^
%Primary Commands:
^
    %Insert ^(abbreviation I) to insert a row into the table
    %Find   ^(abbreviation F) to find the provided string
    %History^(abbreviation H) to display the change history of EDSL
^   %/      ^Popup Selection menu
^
%Line Commands:
^
    %B^  Browse                        %O^  Open in Dataset List
    %D^  Delete to delete a row        %R^  To display the group
    %E^  Edit                          %R^  To review (display)
    %I^  Insert a row                  %S^  Select
    %M^  Move row up one               %U^  Update (alias C)
    %MD^ Move row down one             %V^  View
    %Mx^ Move row up (-#) down(+#)     %any^with Dataset only
^   %/^  Popup Selection menu
^
^\-\Press%Enter^to continue the Tutorial\-\
)Init
)Proc
 &zcont = edslh2
)End
>Panel edslh2
)Attr Default(%+_)
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low) intens(low)
)Body Expand(\\)
+Tutorial\-\%Enhanced Data Set List+\-\Tutorial
%Command ===>_zcmd
+
%EDSL Invocation Syntax:
+
%    TSO EDSL
+      - Open the EDSL list of groups
%    TSO EDSL group
+      - open the specified group using the default selection option
+        Type    selection option
        %D+      Member List
+       %G+      Member List
+       %L+      DSList
+       %O+      UDList
%    TSO EDSL sel group
+      - open the group using the sel option (e.g. E)
%    TSO EDSL sel  group member+(or member mask)
+      - open the group using the sel option (e.g. E)
+
+    If EDSL has been added to the ISPF Commands table then eliminate the TSO
+
+\-\Press%Enter+to continue the Tutorial\-\
)Init
)Proc
 &zcont = edslh3
)End
>Panel edslh3
)Attr Default(%+_)
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low) intens(low)
)Body Expand(\\)
+Tutorial\-\%Enhanced Data Set List+\-\Tutorial
%Command ===>_zcmd
+
%EDSL Notes:+
+
+    1. The Group name will be used as the DSList Name when
+       a Group or List is selected.
+    2. When opening as a data set list the list will be added
+       to the Personal Data Set Lists and thus available using
+       DSLIST and REFOPEND.
+    3. Any existing Data Set List with the same name will be
+       replaced.
+    4. The Group name will be truncated to the first word or
+       to the first 8 characters for the Data Set List name.
+    5. Only 1 OMVS File or Directory is allowed in a group and
+       no z/OS datasets may be included with it.
+    6. A Group is limited to 16 dataset names as that is the limit
+       supported by the ISPF MEMLIST service.
+
+
+
+\-\Press%Enter+to continue the Tutorial\-\
)Init
)Proc
 &zcont = edslh
)End
>Panel edsle
)Attr
 $ type(input ) hilite(uscore) caps(off) intens(low)
 @ type(input ) hilite(uscore) caps(off) intens(low)
 + type(text  )
 _ type(input ) hilite(uscore) caps(on) intens(low)
)Body Window(64,21) Expand(\\)
%Command ===>_zcmd
+
+Enter/Update:
%Group:@edsgrp                  +
+
 +$z +>$d1                                                     +
 +$z +>$d2                                                     +
 +$z +>$d3                                                     +
 +$z +>$d4                                                     +
 +$z +>$d5                                                     +
 +$z +>$d6                                                     +
 +$z +>$d7                                                     +
 +$z +>$d8                                                     +
 +$z +>$d9                                                     +
 +$z +>$d10                                                    +
 +$z +>$d11                                                    +
 +$z +>$d12                                                    +
 +$z +>$d13                                                    +
 +$z +>$d14                                                    +
 +$z +>$d15                                                    +
 +$z +>$d16                                                    +
)Init
 &zwinttl = 'EDSL Dataset(s) or OMVS File/Directory'
 .zvars = '(o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16)'
*Rexx(* edsgrp d1 d2 d3 d4 d5 d6 d7 d8 o1 o2 o3 o4 o5 o6 o7 o8 edsdsn ,
       o9 o10 o11 o12 o13 o14 o15 o16 d9 d10 d11 d12 d13 d14 d15 d16)
 parse value '1 2 3 4 5 6 7 8' with o1 o2 o3 o4 o5 o6 o7 o8
 parse value '9 10 11 12 13 14 15 16' with o9 o10 o11 o12 o13 o14 o15 o16
 d1 = word(edsdsn,1)
 d2 = word(edsdsn,2)
 d3 = word(edsdsn,3)
 d4 = word(edsdsn,4)
 d5 = word(edsdsn,5)
 d6 = word(edsdsn,6)
 d7 = word(edsdsn,7)
 d8 = word(edsdsn,8)
 d9 = word(edsdsn,9)
 d10 = word(edsdsn,10)
 d11 = word(edsdsn,11)
 d12 = word(edsdsn,12)
 d13 = word(edsdsn,13)
 d14 = word(edsdsn,14)
 d15 = word(edsdsn,15)
 d16 = word(edsdsn,16)
*EndRexx
 .help = edsleh
)Proc
 &resp = .resp
*REXX(* zedsmsg zedlmsg resp edsgrp edsdsn bad edstype edsdisp ,
      d1 d2 d3 d4 d5 d6 d7 d8 o1 o2 o3 o4 o5 o6 o7 o8 gendsn ,
      o9 o10 o11 o12 o13 o14 o15 o16 d9 d10 d11 d12 d13 d14 d15 d16)
  parse value '' with zedsmsg zedlmsg order null bad
  if resp = 'END' then exit
  call reorder
  if edsgrp = '' then
  if words(edsdsn) > 1 then
  zedlmsg = 'Error. When group is blank only one dataset name is' ,
    'allowed.'
  w = words(d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16)
  Select
    When pos('/',d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16) > 0
      then do
       edstype = 'O'
       if words(d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16) > 1
       then zedlmsg = 'Error. Only one OMVS File/Directory allowed.'
       else edsdisp = edsgrp
       end
    When pos('*',d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16) > 0
      then do
      edstype = 'L'
      gendsn = '1'
      if w = 1 then do
         if left(d1,1) /= "'" then do
         if sysvar('syspref') = ''
            then d1 = "'"sysvar('sysuid')"."d1"'"
            else d1 = "'"sysvar('syspref')"."d1"'"
         end
         else d1 = translate(d1)
         end
    if edsgrp = null
       then if w = 1
            then edsgrp = d1
    edsdisp = edsgrp
    end
    When w = 1 then do
      edstype = 'D'
      if edsgrp = ''
         then edsdisp = edsdsn
         else edsdisp = edsgrp
    end
    When w > 1 then do
      edstype = 'G'
      edsdisp = edsgrp
    end
    Otherwise do
      edstype = null
      edsdisp = edsgrp
    end
  end
  if 1 = 0 then do
  Reorder:
    edsdsn = null
    order. = null
    order.0 = 0
    c = 0
    do i = 1 to 16
      interpret 'dsn = d'i
      if dsn /= null then do
      if pos(left(dsn,1),"'/") = 0 then
        Select
        When pos('/',dsn) > 0 then nop
        when pos('*',dsn) = 0 then do
          x = listdsi(dsn)
          if x > 0 then do
            zedlmsg = dsn sysmsglvl2
            cpos = 'd'i
            bad = 1
            leave
          end
        end
        otherwise  dsn = translate(dsn)
        end
        c = c + 1
        interpret 'order.'c '= o'i
        if left(dsn,1) = "'"
           then if right(dsn,1) /= "'"
           then dsn = translate(dsn)"'"
        order.c = order.c dsn
        order.0 = c
      end
    end
    do forever
      ordered = 0
      do i = 1 to c
        seq = word(order.i,1)
        in = i + 1
        if in > 16 then leave
        if order.in = null
        then do forever
          in = in + 1
          if in > 16 then leave
        end
        nseq = word(order.in,1)
        if nseq = null then leave
        if seq <= nseq then iterate
        ordered = 1
        swap1 = order.i
        swap2 = order.in
        order.i = swap2
        order.in = swap1
      end
      if ordered = 0 then leave
    end
    do i = 1 to c
      dsn = word(order.i,2)
      if pos("'/",left(dsn,1)) > 0 then do
         edsdsn = edsdsn dsn
         iterate
         end
      if pos('(',dsn) > 0
      then do
        parse value dsn with ld'('mem')'rd
        dsn = ld''rd
      end
      else mem = null
      if pos('/',dsn) > 0 then
        edsdsn = dsn
      if pos('/',dsn) = 0 then do
      if pos('*',dsn) > 0 then do
        if left(dsn,1) /= "'" then do
            if sysvar('syspref') = ''
               then pref = sysvar('sysuid')
               else pref = sysvar('syspref')
            if left(dsn,length(pref)) /= pref
               then dsn = pref'.'dsn
            dsn = "'"dsn"'"
            end
        edsdsn = edsdsn dsn
      end
      else do
      if pos(left(dsn,1),"/'") = 0 then do
        x = listdsi(dsn)
        if mem /= null
        then sysdsname = sysdsname'('mem')'
        edsdsn = edsdsn "'"sysdsname"'"
      end
      else edsdsn = edsdsn dsn
      end
    end
    end
    edsdsn = strip(edsdsn)
    return
  end
*ENDREXX
 ver (&o1,nb,num)
 ver (&o2,nb,num)
 ver (&o3,nb,num)
 ver (&o4,nb,num)
 ver (&o5,nb,num)
 ver (&o6,nb,num)
 ver (&o7,nb,num)
 ver (&o8,nb,num)
 ver (&o9,nb,num)
 ver (&o10,nb,num)
 ver (&o11,nb,num)
 ver (&o12,nb,num)
 ver (&o13,nb,num)
 ver (&o14,nb,num)
 ver (&o15,nb,num)
 ver (&o16,nb,num)
 if (&zedlmsg EQ &Z)
    if (&edsgrp EQ &Z)
       if (&gendsn EQ &Z)
          ver (&edsdsn,nb,dsname)
 if (&zedlmsg NE &Z)
    .cursor = &cpos
    .msg = isrz001
)End
>Panel edslg
)Attr
 $ type(output) caps(off) intens(low)
 @ type(output) caps(off) intens(low)
 + type(text  )
 _ type(input ) hilite(uscore) caps(on) intens(low)
)Body Window(45,20) Expand(\\)
%Command ===>_zcmd
+
+
 +$z +$d1                                 $z
 +$z +$d2                                 $z
 +$z +$d3                                 $z
 +$z +$d4                                 $z
 +$z +$d5                                 $z
 +$z +$d6                                 $z
 +$z +$d7                                 $z
 +$z +$d8                                 $z
 +$z +$d9                                 $z
 +$z +$d10                                $z
 +$z +$d11                                $z
 +$z +$d12                                $z
 +$z +$d13                                $z
 +$z +$d14                                $z
 +$z +$d15                                $z
 +$z +$d16                                $z
+
)Init
*Rexx(zwinttl edsgrp)
 zwinttl = 'EDSL Group:' edsgrp
*EndRexx
 .zvars = '(o1 sc1 o2 sc2 o3 sc3 o4 sc4 o5 sc5 o6 sc6 o7 sc7 o8 sc8 +
            o9 sc9 o10 sc10 o11 sc11 o12 sc12 o13 sc13 o14 sc14 +
            o15 sc15 o16 sc16)'
*Rexx(* edsgrp d1 d2 d3 d4 d5 d6 d7 d8 o1 o2 o3 o4 o5 o6 o7 o8 edsdsn ,
       o9 o10 o11 o12 o13 o14 o15 o16 d9 d10 d11 d12 d13 d14 d15 d16)
 parse value '1 2 3 4 5 6 7 8' with o1 o2 o3 o4 o5 o6 o7 o8
 parse value '9 10 11 12 13 14 15 16' with o9 o10 o11 o12 o13 o14 o15 o16
 d1 = word(edsdsn,1)
 d2 = word(edsdsn,2)
 d3 = word(edsdsn,3)
 d4 = word(edsdsn,4)
 d5 = word(edsdsn,5)
 d6 = word(edsdsn,6)
 d7 = word(edsdsn,7)
 d8 = word(edsdsn,8)
 d9 = word(edsdsn,9)
 d10 = word(edsdsn,10)
 d11 = word(edsdsn,11)
 d12 = word(edsdsn,12)
 d13 = word(edsdsn,13)
 d14 = word(edsdsn,14)
 d15 = word(edsdsn,15)
 d16 = word(edsdsn,16)
*EndRexx
 .help = edsleh
)Proc
)FIELD
  FIELD(d1)  LEN(55) IND(SC1,'<>')
  FIELD(d2)  LEN(55) IND(SC2,'<>')
  FIELD(d3)  LEN(55) IND(SC3,'<>')
  FIELD(d4)  LEN(55) IND(SC4,'<>')
  FIELD(d5)  LEN(55) IND(SC5,'<>')
  FIELD(d6)  LEN(55) IND(SC6,'<>')
  FIELD(d7)  LEN(55) IND(SC7,'<>')
  FIELD(d8)  LEN(55) IND(SC8,'<>')
  FIELD(d9)  LEN(55) IND(SC9,'<>')
  FIELD(d10) LEN(55) IND(SC10,'<>')
  FIELD(d11) LEN(55) IND(SC11,'<>')
  FIELD(d12) LEN(55) IND(SC12,'<>')
  FIELD(d13) LEN(55) IND(SC13,'<>')
  FIELD(d14) LEN(55) IND(SC14,'<>')
  FIELD(d15) LEN(55) IND(SC15,'<>')
  FIELD(d16) LEN(55) IND(SC16,'<>')
)End
>Panel edsleh
)Attr
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low)
)Body Expand(\\)
+Tutorial\-\%Enhanced Data Set List+\-\Tutorial
%Command ===>_zcmd
+
+Enter, or Update, the dataset name or group name and datasets. Or a single
+OMVS file or directory may be entered (per group).
+
+If the%Group+name is blank then only 1 dataset name may be entered.
+
+If only a%Group+name is entered without datasets then it functions as a
+separator row.
+
+Each row has a sequence number that may be changed to change the order of the
+datasets in the group (from 1 to 8).
+
+The%Dataset Name+is entered using standard TSO/ISPF conventions. After entry
+the dataset name will be validated and fully qualified with quotes.
+
+A member name may be entered with a dataset name (no masking allowed).
+
+If the%Dataset+is entered with an%*+then it will be considered a dataset list
+and any action selection will open using the ISPF Data Set List dialog.
+
)Init
)Proc
 &zcont = edsleh
)End
>End */

/* --------------------  rexx procedure  -------------------- *
 * Name:      LoadISPF                                        *
 *                                                            *
 * Function:  Load ISPF elements that are inline in the       *
 *            REXX source code.                               *
 *                                                            *
 * Syntax:    load_info = loadispf()                          *
 *            rc = dropispf(load_info)                        *
 *                                                            *
 *            The inline ISPF resources are limited to        *
 *            ISPF Messages, Panels, and Skeletons,           *
 *                 CLISTs and EXECs are also supported.       *
 *                                                            *
 *            The inline resources must start in column 1     *
 *            and use the following syntax:                   *
 *                                                            *
 *            >START    used to indicate the start of the     *
 *                      inline data                           *
 *                                                            *
 *            >END    - used to indicate the end of the       *
 *                      inline data                           *
 *                                                            *
 *            Each resource begins with a type record:        *
 *            >type name                                      *
 *               where type is CLIST, EXEC, MSG, PANEL, SKEL  *
 *                     name is the name of the element        *
 *                                                            *
 * Sample usage:                                              *
 *          -* rexx *-                                        *
 *          load_info = loadispf()                            *
 *          ... magic code happens here (your code) ...       *
 *          rc = dropispf(load_info)                          *
 *          exit                                              *
 *          >Start inline elements                            *
 *          >Panel panel1                                     *
 *          ...                                               *
 *          >Msg msg1                                         *
 *          ...                                               *
 *          >End of inline elements                           *
 *                                                            *
 * Returns:   the list of ddnames allocated for use along     *
 *            with the libdef's performed or altlib           *
 *                                                            *
 *            format is ddname libdef ddname libdef ...       *
 *                   libdef may be altlibc or altlibe         *
 *                   for altlib clist or altlib exec          *
 *                                                            *
 * Notes:     Entire routine must be included with REXX       *
 *            exec - inline with the code.                    *
 *                                                            *
 * Comments:  The entire rexx program is processed from the   *
 *            last record to the first to find the >START     *
 *            record at which point all records from that     *
 *            point on are processed until the >END           *
 *            statement or the end of the program is found.   *
 *                                                            *
 *            It is *strongly* suggested that the inline      *
 *            elements be at the very end of your code so     *
 *            that the search for them is faster.             *
 *                                                            *
 *            Inline ISPTLIB or ISPLLIB were not supported    *
 *            because the values for these would have to be   *
 *            in hex.                                         *
 *                                                            *
 * Author:    Lionel B. Dyck                                  *
 *                                                            *
 * History:                                                   *
 *            01/09/19 - Include DROPISPF routine             *
 *            08/29/17 - Fixup static values that were vars   *
 *            05/31/17 - Change default directory count       *
 *            12/09/16 - update for add_it routine            *
 *            05/10/16 - correction for clist and exec        *
 *            04/19/16 - bug correction                       *
 *            06/04/04 - Enhancements for speed               *
 *            08/05/02 - Creation                             *
 *                                                            *
 * ---------------------------------------------------------- *
 * Disclaimer: There is no warranty, either explicit or       *
 * implied with this code. Use it at your own risk as there   *
 * is no recourse from either the author or his employeer.    *
 * ---------------------------------------------------------- */
LoadISPF: Procedure

  parse value "" with null kmsg kpanel kskel first returns ,
    kclist kexec
/* ------------------------------------------------------- *
 * Find the InLine ISPF Elements and load them into a stem *
 * variable.                                               *
 *                                                         *
 * Elements keyword syntax:                                *
 * >START - start of inline data                           *
 * >CLIST name                                             *
 * >EXEC name                                              *
 * >MSG name                                               *
 * >PANEL name                                             *
 * >SKEL name                                              *
 * >END   - end of all inline data (optional if last)      *
 * ------------------------------------------------------- */
  last_line = sourceline()
  do i = last_line to 1 by -1
    line = sourceline(i)
    if translate(left(line,6)) = ">START " then leave
  end
  rec = 0
/* --------------------------------------------------- *
 * Flag types of ISPF resources by testing each record *
 * then add each record to the data. stem variable.    *
 * --------------------------------------------------- */
  do j = i+1 to last_line
    line = sourceline(j)
    if translate(left(line,5)) = ">END "   then leave
    if translate(left(line,7)) = ">CLIST " then kclist = 1
    if translate(left(line,6)) = ">EXEC "  then kexec  = 1
    if translate(left(line,5)) = ">MSG "   then kmsg   = 1
    if translate(left(line,7)) = ">PANEL " then kpanel = 1
    if translate(left(line,6)) = ">SKEL "  then kskel  = 1
    rec  = rec + 1
    data.rec = line
  end

/* ----------------------------------------------------- *
 * Now create the Library and Load the Member(s)         *
 * ----------------------------------------------------- */
  Address ISPExec
/* ----------------------------- *
 * Assign dynamic random ddnames *
 * ----------------------------- */
  clistdd = "lc"random(999)
  execdd  = "le"random(999)
  msgdd   = "lm"random(999)
  paneldd = "lp"random(999)
  skeldd  = "ls"random(999)

/* ---------------------------------------- *
 *  LmInit and LmOpen each resource library *
 * ---------------------------------------- */
  if kclist <> null then do
    call alloc_dd clistdd
    "Lminit dataid(clist) ddname("clistdd")"
    "LmOpen dataid("clist") Option(Output)"
    returns = strip(returns clistdd 'ALTLIBC')
  end
  if kexec <> null then do
    call alloc_dd execdd
    "Lminit dataid(exec) ddname("execdd")"
    "LmOpen dataid("exec") Option(Output)"
    returns = strip(returns execdd 'ALTLIBE')
  end
  if kmsg <> null then do
    call alloc_dd msgdd
    "Lminit dataid(msg) ddname("msgdd")"
    "LmOpen dataid("msg") Option(Output)"
    returns = strip(returns msgdd 'ISPMLIB')
  end
  if kpanel <> null then do
    call alloc_dd paneldd
    "Lminit dataid(panel) ddname("paneldd")"
    "LmOpen dataid("panel") Option(Output)"
    returns = strip(returns paneldd 'ISPPLIB')
  end
  if kskel <> null then do
    call alloc_dd skeldd
    "Lminit dataid(skel) ddname("skeldd")"
    "LmOpen dataid("skel") Option(Output)"
    returns = strip(returns skeldd 'ISPSLIB')
  end

/* ----------------------------------------------- *
 * Process all records in the data. stem variable. *
 * ----------------------------------------------- */
  do i = 1 to rec
    record = data.i
    recordu = translate(record)
    if left(recordu,5) = ">END " then leave
    if left(recordu,7) = ">CLIST " then do
      if first = 1 then call add_it
      type = "Clist"
      first = 1
      parse value record with x name
      iterate
    end
    if left(recordu,6) = ">EXEC " then do
      if first = 1 then call add_it
      type = "Exec"
      first = 1
      parse value record with x name
      iterate
    end
    if left(recordu,5) = ">MSG " then do
      if first = 1 then call add_it
      type = "Msg"
      first = 1
      parse value record with x name
      iterate
    end
    if left(recordu,7) = ">PANEL " then do
      if first = 1 then call add_it
      type = "Panel"
      first = 1
      parse value record with x name
      iterate
    end
    if left(recordu,6) = ">SKEL " then do
      if first = 1 then call add_it
      type = "Skel"
      first = 1
      parse value record with x name
      iterate
    end
   /* --------------------------------------------*
    * Put the record into the appropriate library *
    * based on the record type.                   *
    * ------------------------------------------- */
    Select
      When type = "Clist" then
      "LmPut dataid("clist") MODE(INVAR)" ,
        "DataLoc(record) DataLen(255)"
      When type = "Exec" then
      "LmPut dataid("exec") MODE(INVAR)" ,
        "DataLoc(record) DataLen(255)"
      When type = "Msg" then
      "LmPut dataid("msg") MODE(INVAR)" ,
        "DataLoc(record) DataLen(80)"
      When type = "Panel" then
      "LmPut dataid("panel") MODE(INVAR)" ,
        "DataLoc(record) DataLen(80)"
      When type = "Skel" then
      "LmPut dataid("skel") MODE(INVAR)" ,
        "DataLoc(record) DataLen(80)"
      Otherwise nop
    end
  end
  if type <> null then call add_it
/* ---------------------------------------------------- *
 * Processing completed - now lmfree the allocation and *
 * Libdef the library.                                  *
 * ---------------------------------------------------- */
  if kclist <> null then do
    Address TSO,
      "Altlib Act Application(Clist) File("clistdd")"
    "LmFree dataid("clist")"
  end
  if kexec <> null then do
    Address TSO,
      "Altlib Act Application(Exec) File("execdd")"
    "LmFree dataid("exec")"
  end
  if kmsg <> null then do
    "LmFree dataid("msg")"
    "Libdef ISPMlib Library ID("msgdd") Stack"
  end
  if kpanel <> null then do
    "Libdef ISPPlib Library ID("paneldd") Stack"
    "LmFree dataid("panel")"
  end
  if kskel <> null then do
    "Libdef ISPSlib Library ID("skeldd") Stack"
    "LmFree dataid("skel")"
  end
  return returns

/* --------------------------- *
 * Add the Member using LmmAdd *
 * based upon type of resource *
 * --------------------------- */
Add_It:
  Select
    When type = "Clist" then
    "LmmAdd dataid("clist") Member("name")"
    When type = "Exec" then
    "LmmAdd dataid("exec") Member("name")"
    When type = "Msg" then
    "LmmAdd dataid("msg") Member("name")"
    When type = "Panel" then
    "LmmAdd dataid("panel") Member("name")"
    When type = "Skel" then
    "LmmAdd dataid("skel") Member("name")"
    Otherwise nop
  end
  type = null
  return

/* ------------------------------ *
 * ALlocate the temp ispf library *
 * ------------------------------ */
Alloc_DD:
  arg dd
  Address TSO
  if pos(left(dd,2),"lc le") > 0 then
  "Alloc f("dd") unit(sysda) spa(5,5) dir(5)",
    "recfm(v b) lrecl(255) blksize(32760)"
  else
  "Alloc f("dd") unit(sysda) spa(5,5) dir(5)",
    "recfm(f b) lrecl(80) blksize(23440)"
  return

/* --------------------  rexx procedure  -------------------- *
 * Name:      DropISPF                                        *
 *                                                            *
 * Function:  Remove ISPF LIBDEF's and deactivate ALTLIB's    *
 *            that were created by the LoadISPF function.     *
 *                                                            *
 * Syntax:    rc = dropispf(load_info)                        *
 *                                                            *
 * Author:    Janko                                           *
 *                                                            *
 * History:                                                   *
 *            12/05/18 - Creation                             *
 * ---------------------------------------------------------- */
DropISPF: Procedure
  arg load_info
  Address ISPEXEC
  do until length(load_info) = 0
    parse value load_info with dd libd load_info
    if left(libd,6) = "ALTLIB" then do
      if libd = "ALTLIBC" then lib = "CLIST"
      else lib = "EXEC"
      Address TSO,
        "Altlib Deact Application("lib")"
    end
    else "libdef" libd
    address tso "free f("dd")"
  end
  return 0
