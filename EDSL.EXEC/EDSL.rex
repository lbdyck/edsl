  /* --------------------  rexx procedure  ------------------- */
  ver = '1.44'
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
  | Authors:     Lionel B. Dyck                                |
  |              John Kalinich                                 |
  |                                                            |
  | History:  (most recent on top)                             |
  |    1.44    11/18/20 JK  - Add ISPList command (Tree)       |
  |    1.43    11/16/20 JK  - Add QREF primary command         |
  |    1.42    11/16/20 JK  - Update header logic (Tree)       |
  |    1.41    11/14/20 JK  - Add / primary command (Tree)     |
  |    1.40    11/13/20 JK  - Edit EDSHELP skeleton            |
  |    1.39    11/12/20 JK  - Add HEL primary command          |
  |    1.38    11/12/20 LBD - Correct exit after Set change    |
  |    1.37    11/11/20 JK  - If SET changes view, then branch |
  |    1.36    11/11/20 JK  - Correct dataset logic (Tree)     |
  |                         - Update cursor select (Tree)      |
  |    1.35    11/10/20 LBD - Fix group in tree for dataset    |
  |                         - Change TBDISPL to TABLE in Set   |
  |                         - Change tree TABLE to DEBUG       |
  |                         - Add TABLE to commands in Tree    |
  |    1.34    11/10/20 JK  - Add callable Version routine     |
  |                         - Add SET command                  |
  |    1.33    11/09/20 LBD - Fix add/update of OMVS file      |
  |                         - Update version prose             |
  |    1.32    11/08/20 JK  - Add / line command (Tree)        |
  |                         - Add M/N line commands (Tree)     |
  |                         - Add VERSION primary cmd (Tree)   |
  |    1.31    11/01/20 JK  - Colorize nodes (Tree)            |
  |                         - Add X ALL primary command (Tree) |
  |                         - Add INSERT primary command (Tree)|
  |                         - Add E/V/B/S line commands (Tree) |
  |                         - Add O/I/U/D line commands (Tree) |
  |                         - Add X toggle line command (Tree) |
  |                         - Cursor position S line cmd (Tree)|
  |    1.30    10/27/20 LBD - Add TREE to / Primary popup menu |
  |                         - right justify rows in tree       |
  |    1.29    10/27/20 JK  - Add TREE primary command         |
  |    1.28    10/27/20 LBD - Fix lost dsn during update       |
  |    1.27    10/27/20 LBD - Correct check of xxx.* entries   |
  |    1.26    10/26/20 LBD - Check list of datasets and force |
  |                           to type List if mixed format     |
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
  'vget (edsinit)'
  if edsinit = 'TREE' then do        /* Dynamic--display tree format  */
    listit = 0
    call do_tree
    if listit = 0 then call done
    end
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
      When abbrev('FIND',word(zcmd,1),1) = 1 then call do_find
      When abbrev('TREE',word(zcmd,1),1) = 1 then call do_tree
      When abbrev('SET',word(zcmd,1),1)  = 1 then do
        'vget (edsinit)'
        save_init = edsinit
        call do_settings
        if edsinit = 'TREE' &,
           save_init <> edsinit then do
          listit = 0
          call do_tree
          call done
          end
        end
      When abbrev('VERSION',word(zcmd,1),1) = 1 then call do_version
      when abbrev("QREF",word(zcmd,1),1) = 1 then do
        zcmd = ''
        "select pgm(isptutor) parm(edsqref)"
        end
      When abbrev("HEL",word(zcmd,1),3) = 1 then call do_help
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
  call check_type
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
  call check_type
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

/* ------------------------------------------------ *
 | Confirm all the entries are the same dsorg/recfm |
 | and if not set to type O                         |
 * ------------------------------------------------ */
Check_Type:
  do cti = 1 to words(edsdsn)
     if pos(edstype,'OL') > 0 then return
     if pos('*',word(edsdsn,cti)) > 0 then do
        edstype = 'L'
        return
        end
     x = listdsi(word(edsdsn,cti))
     if cti = 1 then do
        ctdsorg = sysdsorg
        ctrecfm = sysrecfm
        end
     else do
       if ctdsorg /= sysdsorg then edstype = 'L'
       if ctrecfm /= sysrecfm then edstype = 'L'
       if edstype = 'L' then return
       end
    end
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
  /* --------------------------------- *
  | Dynamic Tree routine              |
  * --------------------------------- */
Do_Tree:
  zcmd  = ''
  rsel  = ''
  edsxgrp = ''                              /* Excluded groups        */

Tree_Exclude:
  xgrp   = 'OFF'
  red    = '01'x                            /* Assign colors to       */
  blue   = '02'x                            /*     Attribute bytes    */
  green  = '03'x                            /*     found in the data  */
  white  = '04'x
  turq   = '05'x
  pink   = '06'x
  yellow = '07'x
  ndisp  = '09'x                            /*  non-display field     */
  sel    = '10'x                            /* . line command pad     */
  selx   = '11'x                            /* + line command pad     */
  selh   = '12'x                            /*   line command pad     */
  dyndata  = ''                             /* initialize data        */
  shadata  = ''                             /* initialize shadow var  */
  parse value '0 0 0 0 0' with g# o# l# d# h#   /* edstype statistics */
  maxlines = 0

  Address ISPExec
  'tbtop edsl'
  'tbskip edsl'
  ttn = 1
  all_groups  = ''
  do forever                                       /* Build dynamic   */
    'tbget edsl'                                   /*   area          */
    if edstype = 'G' then g# = g# + 1
    if edstype = 'O' then o# = o# + 1
    if edstype = 'L' then l# = l# + 1
    if edstype = 'D' then d# = d# + 1
    if edstype = ' ' then h# = h# + 1
    tree_tbl.ttn = 'edstype='left(edstype,1)  'edsgrp='left(edsgrp,24),
                    'edsdsn='edsdsn  'edsloc='edsloc
    lineno = right(ttn,5,'0')
    ttn = ttn + 1
    do
      if edstype = 'D' then
        edsgrp = left(edsdisp,24)||ndisp||lineno
      group = edsgrp
      all_groups = all_groups left(group,24)       /* Build list of   */
                                                   /*   groups        */
      color = blue                                 /* Group           */
      if edstype = 'D' then                        /* DSNAME          */
        color = pink
      if edstype = 'O' then                        /* OMVS            */
        color = yellow
      if edstype = 'L' then                        /* DSList          */
        color = green

      if pos(left(edsgrp,24),edsxgrp) > 0 then do  /* Excluded?       */
        if edstype = '' then
          y = selh||' '||white||group              /* Section header  */
        else
          y = selx||' '||color||group              /* +=Excluded      */
        end
      else
        if edstype = '' then
          y = selh||' '||white||group              /* Section header  */
        else
          y = sel||' '||color||group

      dyndata=dyndata||left(y,80)
      maxlines = maxlines + 1

      do num = 1 to 16
        if pos(left(edsgrp,24),edsxgrp) > 0 then
          leave
        if word(edsdsn,num) <> '' then do
          y = ' '||' '||turq||'   ' word(edsdsn,num)
          dyndata=dyndata||left(y,80)
          maxlines = maxlines + 1
        end
      end
    end
    'tbskip edsl'
    if rc > 0 then do
      tree_tbl.0 = ttn - 1
      leave
      end
  end
  dyndata = dyndata||,                     /* mark bottom             */
            white||centre(' Bottom of data ',79,'*')
  usrrfind = 'PASSTHRU'                    /* RFIND                   */
  'vput (usrrfind)'

/* ------------------------------------------------------------------ */
/* Doug Nadel scrolling code; ISPF Panels Beyond the Basics, Mar 2000 */
/* ------------------------------------------------------------------ */
Doug:
  dynarea  = ''                            /* initialize data         */
  dynshad  = ''                            /* initialize shadow var   */
  curline  = 1                             /* set current line #      */
  Address ISPExec
  'vget (zscreend)'                        /* get screen depth        */
  ht = zscreend                            /* screen height           */
do until disprc > 0
  dynarea = substr(dyndata,1+(curline-1)*80,ht*80) /* set dynamic var */
  dynshad = ''                                     /* set shadow  var */
  'display panel(edsdyn)'                  /* display the data        */
  disprc = rc                              /* save return code        */
  'vget (zverb,zscrolla,zscrolln)'         /* get scroll values       */
  select                                   /* process scrolling       */
    when(zverb = 'UP')   then              /* scroll up               */
     if zscrolla = 'MAX' then              /*  if scroll was max      */
      curline = 1                          /*     scroll to top       */
     else                                  /*  else a number is known */
      curline = max(1,curline-zscrolln);   /* (maximum is top)        */
    when(zverb = 'DOWN') then              /* scroll down             */
     if zscrolla = 'MAX' then              /*  if scroll was max      */
      curline = maxlines                   /*     scroll to bottom    */
     else                                  /*  else a number is known */
      curline = min(maxlines,curline+zscrolln)  /* (max is bottom)    */
    otherwise                       /* could use left & right too     */
  end
/* ------------------------------------------------------------------ */
/* Doug Nadel scrolling code; end                                     */
/* ------------------------------------------------------------------ */
/* --------------------------------- *
 | Cursor Select line command        |
 * --------------------------------- */
Cursor_Pos_Select:
  if datatype(cpos,'N') = 1 then do
    if cname = 'DYNAREA' &,                        /* Dynamic area    */
       (cpos // 80) = 2  &,                        /* Column 2        */
       substr(dynarea,cpos,1)   <> '*' &,          /* ^Bottom of data */
       substr(dynarea,cpos+2,4) <> ' ' then        /* ^DSN's          */
         dynarea = substr(dynarea,1,cpos-1)||'S'||,
                   substr(dynarea,cpos+1,length(dynarea))
    end
/* --------------------------------- *
 | Tree primary commands             |
 * --------------------------------- */
  zcmd_lower = zcmd
  zcmd = translate(zcmd)
  select
  when abbrev('/',zcmd,1) = 1 then do
    zcmd = ''
    call pfshow 'off'
    'Addpop Row(4) column(15)'
    'Display Panel(edsdynpo)'
    'rempop'
    call pfshow 'reset'
    end
  otherwise nop
  end
  select
  when abbrev('TABLE',zcmd,1) = 1 then do
    listit = 1
    return
    end
  when abbrev('SET',word(zcmd,1),1)   = 1 then do
       'vget (edsinit)'
       save_init = edsinit
       call do_settings
       if edsinit = 'TABLE' &,
          save_init <> edsinit then do
         listit = 1
         return
         end
       end
  when abbrev("FIND",word(zcmd,1),1) = 1 then do
    findstr = strip(word(zcmd,2),,"'")
    findstr_lower = strip(word(zcmd_lower,2),,"'")
    zcmd = ''
    call tree_find
    end
  when abbrev("RFIND",word(zcmd,1),2) = 1 then do
    if curline = maxlines then
      curline = 0
    curline = curline + 1
    zcmd = ''
    call tree_find
    end
  when abbrev("REFRESH",word(zcmd,1),3) = 1 then do
    zcmd = ''
    signal do_tree
    end
  when word(zcmd,1) = 'X' & word(zcmd,2) = 'ALL' then do
    zcmd = ''
    edsxgrp = all_groups
    signal tree_exclude
    end
  when abbrev("INSERT",word(zcmd,1),1) = 1 then do
    zcmd = ''
    call do_insert '1'
    signal do_tree
    end
  when abbrev("DEBUG",word(zcmd,1),1) = 1 then do
    zcmd = ''
    do x = 1 to tree_tbl.0
      say tree_tbl.x
      end
    end
  when abbrev("ISPLIST",word(zcmd,1),4) = 1 then do
    zcmd = ''
    call do_isplist
    end
  When abbrev("HEL",word(zcmd,1),3) = 1 then call do_help
  when abbrev("HISTORY",word(zcmd,1),1) = 1 then do
    zcmd = ''
    call do_history
    end
  when abbrev("METRICS",word(zcmd,1),1) = 1 then do
    zcmd = ''
    call do_stats
    end
  when abbrev("QREF",word(zcmd,1),1) = 1 then do
    zcmd = ''
    "select pgm(isptutor) parm(edsqref)"
    end
  when abbrev("VERSION",word(zcmd,1),1) = 1 then call do_version
  otherwise nop
  end
/* --------------------------------- *
 | Tree line commands                |
 * --------------------------------- */
  do line = 1 to ht-3
    grp = substr(dynarea,4+(line-1)*80,24)
    dsngrp = substr(dynarea,4+(line-1)*80,30)
    /* Popup line commands */
    if substr(dynarea,2+(line-1)*80,1) = '/' then do
      edsdisp = left(grp,24)
      call pfshow 'off'
      'Addpop Row(4) column(15)'
      if substr(dynarea,3+(line-1)*80,1) /= '04'x then    /* ^Header  */
        'Display Panel(edsdyno)'
      else
        'Display Panel(edslos)'                           /* Header   */
      'rempop'
      call pfshow 'reset'
      slash = zcmd
      if slash = 'R' then
        slash = ' '
      dynarea = substr(dynarea,1,1+(line-1)*80,1)||slash||,
                substr(dynarea,3+(line-1)*80,length(dynarea))
      zcmd = null
      end
    select
    /* Exclude - toggle */
    when substr(dynarea,2+(line-1)*80,1) = 'X' then do
      if substr(dynarea,3+(line-1)*80,1) = '04'x then     /* Header   */
         leave
      sel_pos = pos(grp,edsxgrp)
      if sel_pos > 0 then
        edsxgrp = delstr(edsxgrp,sel_pos,24)
      else
        edsxgrp = edsxgrp left(grp,24)
      xgrp = 'ON'
      end
    /* Edit ,View, Browse, Library, OpenDSL */
    when substr(dynarea,2+(line-1)*80,1) = 'E' |,
         substr(dynarea,2+(line-1)*80,1) = 'V' |,
         substr(dynarea,2+(line-1)*80,1) = 'B' |,
         substr(dynarea,2+(line-1)*80,1) = 'S' then do
      if substr(dynarea,3+(line-1)*80,1) = '04'x then     /* Header   */
         leave
      if substr(dynarea,3+(line-1)*80,1) = '06'x then do  /* DSN  */
        rsel = substr(dynarea,2+(line-1)*80,1)
        edsdsn = substr(dynarea,8+(line)*80,56)     /* DSN next line  */
        edstype = 'D'
        edsgrp = ''
        end
      else                                          /* Group, DSLIST  */
        do
          rsel = substr(dynarea,2+(line-1)*80,1)
          rmem = null
          edsgrp = left(grp,24)
          'tbtop edsl'
          'tbscan edsl arglist(edsgrp)'
          end
      call do_BEV
      rsel = ''
      end
    /* Insert, Delete, OpenDSL, Move Up, Move Dn, Update */
    when substr(dynarea,2+(line-1)*80,1) = 'I' |,
         substr(dynarea,2+(line-1)*80,1) = 'D' |,
         substr(dynarea,2+(line-1)*80,1) = 'O' |,
         substr(dynarea,2+(line-1)*80,1) = 'M' |,
         substr(dynarea,2+(line-1)*80,1) = 'N' |,
         substr(dynarea,2+(line-1)*80,1) = 'U' then do
      if substr(dynarea,3+(line-1)*80,1) = '04'x &,       /* Header   */
         pos(substr(dynarea,2+(line-1)*80,1),'O') = 1 then
        leave
      if substr(dynarea,3+(line-1)*80,1) = '06'x then do  /* DSN  */
        rsel = substr(dynarea,2+(line-1)*80,1)
        skipnum = substr(dsngrp,26,5)          /* Non-display row num */
        'tbtop edsl'
        'tbskip edsl number('skipnum')'
        end
      else                                          /* Group, DSLIST  */
        do
          rsel = substr(dynarea,2+(line-1)*80,1)
          edsgrp = left(grp,24)
          'tbtop edsl'
          'tbscan edsl arglist(edsgrp)'
        end
      if pos(rsel,'D') > 0 then do
        zwinttl = 'Confirm Request'
        ckey    = ''
        do until (ckey = 'PF03') | (ckey = 'ENTER')
          'control nocmd'
          'addpop'
          'display panel(edsdync)'
          'rempop'
          end
        if ckey = 'PF03' then do
          rsel = ''
          leave
          end
        'tbdelete edsl'
        signal do_tree
        end
      if pos(rsel,'I') > 0 then do
        call do_insert
        signal do_tree
        end
      if pos(rsel,'M') > 0 then do
        rsel = 'M-1'
        call do_MoveRow
        signal do_tree
        end
      if pos(rsel,'N') > 0 then do
        rsel = 'M1'
        call do_MoveRow
        signal do_tree
        end
      if pos(rsel,'O') > 0 then do
        call do_OpenDSL
        end
      if pos(rsel,'U') > 0 then do
        call do_update
        signal do_tree
        end
      rsel = ''
      end
    otherwise
    end
  end
  if xgrp = 'ON' then
    signal tree_exclude
end
return
/* --------------------------------- *
 | Tree find group with matching     |
 * --------------------------------- */
Tree_Find:
  do x = curline to maxlines
    line = substr(dyndata,1+(x-1)*80,80)
    if pos(findstr,line) > 0 |,
       pos(findstr_lower,line) > 0 then do
      zerrsm = "CHARS '"findstr"' found"
      zerrlm = "Search for CHARS '"findstr"' was successful."
      zerrhm = 'edslh'
      zerralrm = 'NO'
      Address ISPExec 'setmsg msg(isrz002)'
      curline = x
      leave x
      return
      end
  end
  curline = x
  if curline > maxlines then do
    curline = maxlines
    zerrsm = '*Bottom of list reached*'
    zerrlm = "CHARS '"findstr"' not found. Press RFIND key to",
             "continue the search from the top of the group list."
    zerrhm   = 'edslh'
    zerralrm = 'NO'
    Address ISPExec 'setmsg msg(isrz002)'
    end
  return
/* --------------------------------- *
 | Write tree table to ISPLIST       |
 * --------------------------------- */
Do_ISPList:
    do x = 1 to tree_tbl.0
      parse var tree_tbl.x with 'edstype=' typ 'edsgrp=' grp,
                                'edsdsn='  dsn 'edsloc=' loc
      d. = ''
      parse var dsn d.1 d.2  d.3  d.4  d.5  d.6  d.7  d.8,
                    d.9 d.10 d.11 d.12 d.13 d.14 d.15 d.16
      if left(typ,1) = 'D' then
        grp = left(dsn,24)
      dtl = left(grp,24)
      'list bufname(dtl) linelen(80) single'
      do i = 1 to 16
        if d.i <> '' then do
          dtl = '    'd.i
          'list bufname(dtl) linelen(80) single'
          end
        end
      end
    zerralrm = 'NO'
    zerrhm = 'edsdynh'
    zerrsm = tree_tbl.0 'Groups listed'
    zerrlm = 'Tree table has been successfully listed'
    'Setmsg msg(isrz002)'
    return
/* --------------------------------- *
 | Display EDSL� version info        |
 * --------------------------------- */
Do_Version:
    zcmd = ''
    zerrsm = ''
    zerrlm = left('EDSL� '||ver,73),
             left('Lionel Dyck and John Kalinich',73),
             left('The ISPF Cabal - Vive la r�volution',73),
             left('Copyleft - GNU GPL v3',73)
    zerralrm = 'NO'
    zerrhm = 'edsdynh'
    Address ISPExec
    'setmsg msg(isrz002)'
    return
/* --------------------------------- *
 | Display EDSL� settings            |
 * --------------------------------- */
Do_Settings:
    zcmd = ''
    Address ISPExec
    'Addpop Row(4) column(15)'
    'Display Panel(edsset)'
    'rempop'
    return
/* --------------------------------- *
 | Display EDSL� help member         |
 * --------------------------------- */
Do_Help:
    findcmd  = word(zcmd,2)
    zcmd = ''
    parse var load_info . . . . skeldd .
    Address ISPExec
    "lminit dataid(skel) ddname("skeldd")"
    "view dataid("skel") member(edshelp)",
      "macro(edsfhelp) parm(findcmd)"
    "lmfree dataid("skel")"
    return
/* --------------------------------- *
 | Display EDSL� row statistics      |
 * --------------------------------- */
Do_Stats:
    zerrsm = ''
    zerrlm = left('Group  = 'right(g#,5),73),
             left('OMVS   = 'right(o#,5),73),
             left('DSList = 'right(l#,5),73),
             left('DSName = 'right(d#,5),73),
             left('Header = 'right(h#,5),73),
             left('Total  = 'right(g#+o#+l#+d#+h#,5),73)
    zerralrm = 'NO'
    zerrhm = 'edsdynh'
    Address ISPExec
    'setmsg msg(isrz002)'
    return
/* start of inline elements
>Start
>Panel edsdyn
)ATTR
  � TYPE(INPUT) INTENS(HIGH) CAPS(OFF) COLOR(RED)
  @ AREA(DYNAMIC)            SCROLL(ON) EXTEND(ON)
  01 TYPE(DATAOUT)           COLOR(RED)
  02 TYPE(DATAOUT)           COLOR(BLUE)
  03 TYPE(DATAOUT)           COLOR(GREEN)
  04 TYPE(DATAOUT)           COLOR(WHITE)
  05 TYPE(DATAOUT)           COLOR(TURQ)
  06 TYPE(DATAOUT)           COLOR(PINK)
  07 TYPE(DATAOUT)           COLOR(YELLOW)
  09 TYPE(DATAOUT)           INTENS(NON)
  10 TYPE(DATAIN) CAPS(ON) PAD('.') PAS(ON) COLOR(RED)
  11 TYPE(DATAIN) CAPS(ON) PAD('+') PAS(ON) COLOR(RED)
  12 TYPE(DATAIN) CAPS(ON) PAD(' ') PAS(ON) COLOR(RED)
  r  TYPE(CHAR) COLOR(RED)   HILITE(REVERSE)
  g  TYPE(CHAR) COLOR(GREEN) HILITE(REVERSE)
  b  TYPE(CHAR) COLOR(BLUE)  HILITE(REVERSE)
  $  TYPE(TEXT)              COLOR(YELLOW)
  #  TYPE(TEXT)              COLOR(TURQ)
  [  TYPE(output)            COLOR(TURQ) just(right) caps(off)
  ~  TYPE(OUTPUT)            COLOR(TURQ)
  ?  TYPE(OUTPUT) PAS(ON)    COLOR(YELLOW) CAPS(OFF) HILITE(USCORE)
  !  TYPE(PS)
)BODY EXPAND(\\)
%EDSL�+                      $Tree Display              [rowline
%Command ===>�zcmd                                            %Scroll ===>_Z   +
+
@dynarea,dynshad                                                               @
)INIT
.zvars = '(zscml)'
*REXX(* rowline curline maxlines)
rowline = 'Row' curline 'of' maxlines
*Endrexx
.help  = 'edsdynh'
if (&zscml = ' ') &zscml = 'CSR'
)PROC
vput (zscml) profile
&cpos = .CSRPOS
&cname = .CURSOR
)END
>Panel edsdynh
)Attr Default(%^_)
 @ type(output) caps(off) intens(low) color(turq)
 ~ type(text) caps(off) intens(low) color(yellow)
 ` type(text) caps(off) intens(low) color(blue)
 ] type(text) caps(off) intens(low) color(green)
 [ type(text) caps(off) intens(low) color(pink)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low) intens(low)
   color(red)
)Body Expand(\\)
^Tutorial\-\%Enhanced Data Set List^\-\Tutorial
%Command ===>_zcmd
^
%Node Colors:  `Group   ~OMVS   ]DSList   [DSName
^
%Primary Commands:
    %Find   ^to find the provided string
    %REFresh^to refresh the group tree display
    %X ALL  ^to exclude all group tree nodes
    %Insert ^to insert a row into the table
    %Table  ^to switch to ISPF table view
    %Set    ^to display the settings window
    %HEL cmd^to display EDSL� help member
    %History^to display change history of EDSL
    %Version^to display EDSL� version number
^
%Line Commands:
    %B^  Browse     %M^  Move up       %O^  Open in Dataset List
    %D^  Delete     %N^  Move down     %S^  Library utility
    %E^  Edit       %U^  Update        %.^  Cursor select (S)
    %I^  Insert     %V^  View          %X^  Exclude tree node (toggle)
    %/^  Popup menu
)Init
)Proc
 &zcont = edsdynh
)End
>Panel edsdync
)attr default(%+_)
   $  type(output)  intens(low)  caps(off)  color(turq)  just(left)
   _  type(input)   intens(high) caps(on)   just(left)   hilite(uscore)
   +  type(text)    intens(low)  skip(on)   color(green)
)body window(50,6)
+
%Confirm delete of Group:$edsgrp
+
+Are you sure?
+
+Press%ENTER+to process or%END+to exit
)PROC
  &ckey = .pfkey
  if (&ckey = ' ')
      &ckey = 'ENTER'
  vput (ckey) shared
)end
>Panel edsdynpo
)Attr Default(%+_)
  _ type( input) intens(low ) caps(on ) just(left ) hilite(uscore)
  + type(text) intens(low) skip(on)
  ] type(output) caps(off) pas(on) intens(high) color(white) hilite(uscore)
  @ type(output) caps(off)
)Body Window(48,12)
%Command ===>_z
+
+]I+Insert  - insert a row into the table
+]H+History - display the change history of EDSL
+]S+Set     - display the settings window
+]T+Table   - display the EDSL table
+]V+Version - display EDSL� version number
+]R+Refresh - refresh the group tree display
+]X+Exclude - exclude all group tree nodes
+
           +Or%F3+to cancel
)Init
 &zwinttl = 'Tree Primary Commands:'
 .zvars = '(zcmd)'
 .cursor = zcmd
 .help = edsdynh
 &I = I
 &H = H
 &R = R
 &S = S
 &T = T
 &V = V
 &X = X
)Proc
 if (&zcmd = 'X')
   &zcmd = 'X ALL'
 if (&zcmd = 'R')
   &zcmd = 'REF'
)PNTS
 FIELD(I)  VAR(ZCMD) VAL('I')
 FIELD(H)  VAR(ZCMD) VAL('H')
 FIELD(R)  VAR(ZCMD) VAL('REF')
 FIELD(S)  VAR(ZCMD) VAL('S')
 FIELD(T)  VAR(ZCMD) VAL('T')
 FIELD(V)  VAR(ZCMD) VAL('V')
 FIELD(X)  VAR(ZCMD) VAL('X ALL')
)End
>Panel edsdyno
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
+]B+Browse            +]O+Open DSL/UDL
+]D+Delete group      +]S+Select (Open)
+]E+Edit              +]U+Update
+]I+Insert            +]V+View
+]M+Move up           +]X+Exclude
+]N+Move dn
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
 &M = M
 &N = N
 &O = O
 &S = S
 &U = U
 &V = V
 &X = X
)Proc
)PNTS
 FIELD(B)  VAR(ZCMD) VAL('B')
 FIELD(D)  VAR(ZCMD) VAL('D')
 FIELD(E)  VAR(ZCMD) VAL('E')
 FIELD(I)  VAR(ZCMD) VAL('I')
 FIELD(M)  VAR(ZCMD) VAL('M')
 FIELD(N)  VAR(ZCMD) VAL('N')
 FIELD(O)  VAR(ZCMD) VAL('O')
 FIELD(S)  VAR(ZCMD) VAL('S')
 FIELD(U)  VAR(ZCMD) VAL('U')
 FIELD(V)  VAR(ZCMD) VAL('V')
 FIELD(X)  VAR(ZCMD) VAL('X')
)End
>Panel edsset
)Attr Default(%+_)
  _ type( input) intens(low ) caps(on ) just(left ) hilite(uscore)
  + type(text) intens(low) skip(on)
  ] type(output) caps(off) pas(on) intens(high) color(white) hilite(uscore)
  @ type(output) caps(off)
)Body Window(42,5)
%Command ===>_z
+
%Display view ===>_edsinit+(TABLE or TREE)
+
)Init
 &zwinttl = 'EDSL Settings:'
 .zvars = '(zcmd)'
 .cursor = edsinit
 .help = edslh
 if (&edsinit = '')
   &edsinit = 'TABLE'
)Proc
*REXX(*)
 if abbrev('TABLE',edsinit,2) = 1 then edsinit = 'TABLE'
 if abbrev('TREE',edsinit,2)  = 1 then edsinit = 'TREE'
*ENDREXX
 ver (&edsinit,nb,list,TABLE,TREE)
 vput (edsinit) profile
)End
>Panel edsqref
)Attr Default(`[_)
/* _ type( input) intens(high) caps(on ) just(left )               */
   ! type(text) intens(high) caps(off) just(asis ) color(white)
   ` type(text) intens(high) caps(off) just(asis ) color(yellow)
   ~ type(text) intens(high) caps(off) just(asis ) color(green) hilite(uscore)
   [ type(text) intens(low ) color(turq)
   ] type(text) intens(low ) color(blue)
     skip(on)
   ^ area(SCRL) Extend(ON)
)Body Window(62,20)

~Command[          ~Function[
^help ------------------------------------------------------^
)Area Help
!Find             -[Find the provided string
!HEL cmd          -[Display EDSL help for a cmd
!History          -[Display modification history
!Insert           -[Insert a row into the table
!Qref             -[What you are reading now
!REFresh          -]Refresh the group tree display
!RFind            -]Repeat find of the provided string
!Set              -[Display the settings window
!Table            -]Switch to ISPF table view
!Tree             -[Display the group tree view
!Version          -[Display EDSL� version number
!X ALL            -]Exclude all group tree nodes
!Debug            -]Dump table for debugging
!Metrics          -]Display EDSTYPE statistics
!ISPList          -]Write tree table to ISPLIST
!/                -[Popup selection menu
`B               !-[Browse
`D               !-[Delete to delete a row
`E               !-[Edit
`I               !-[Insert a row
`M               !-[Move row up one
`MD              !-[Move row down one
`Mx              !-[Move row up (-#) down(+#)
`N               !-]Move row down one
`/               !-[Popup Selection menu
`O               !-[Open in Dataset List
`R               !-[To display the group
`S               !-[Select
`.               !-[Cursor select (S)
`U               !-[Update (alias C)
`V               !-[View
`X               !-]Exclude tree node (toggle)
`any             !-[(with Dataset only)
)Init
&zwinttl = 'EDSL Quick Reference'
&zup = EDSQREF
)Proc
)End
>Exec edsfhelp
/* rexx */
ISREdit 'macro (arg)'
ISREdit 'reset'
if arg = '' then
  nop
else
  ISREdit 'find <'arg
>Panel edsl
)Attr
 @ type(output) caps(off) intens(low) color(turq)
 $ type(input ) hilite(uscore) caps(on) intens(low)
 _ type(input ) hilite(uscore) caps(on) intens(low)
)Body Expand(\\)
%EDSL�+\-\%Enhanced Data Set List+@ver +\-\
%Command ===>_zcmd                \ \%Scroll ===>_edsc+
+
%Select     Type   Dataset/Group
)Model
$rsel     +  @z + @edsdisp                                              +
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
)Body Window(48,11)
%Command ===>_z
+
+]I+Insert  - insert a row into the table
+]H+History - display the change history of EDSL
+]S+Set     - display the settings window
+]T+Tree    - display a tree view of the table
+]V+Version - display EDSL� version number
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
 &S = S
 &T = T
 &V = V
)Proc
 if (&zcmd EQ F)
    ver (&fstring,nb)
)PNTS
 FIELD(I)  VAR(ZCMD) VAL('I')
 FIELD(F)  VAR(ZCMD) VAL('F')
 FIELD(H)  VAR(ZCMD) VAL('H')
 FIELD(S)  VAR(ZCMD) VAL('S')
 FIELD(T)  VAR(ZCMD) VAL('T')
 FIELD(V)  VAR(ZCMD) VAL('V')
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
+Panel Fields:%Select+       Row selection field
+
+             %Type+         Row type:  %D+Dataset
+                                       %G+Group
+                                       %L+DSList
+                                       %O+OMVS
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
%Primary Commands: %Insert ^(abbreviation I) insert a row into the table
                   %Find   ^(abbreviation F) find the provided string
                   %HEL cmd^(abbr HEL)       display EDSL� help for a cmd
                   %History^(abbreviation H) display change history of EDSL
                   %Set    ^(abbreviation S) display the settings window
                   %Tree   ^(abbreviation T) display the group tree view
                   %Version^(abbreviation V) display EDSL� version number
                   %/      ^Popup Selection menu
^
%Line Commands:    %B^  Browse                      %O^  Open in Dataset List
                   %D^  Delete to delete a row      %R^  To display the group
                   %E^  Edit                        %R^  To review (display)
                   %I^  Insert a row                %S^  Select
                   %M^  Move row up one             %U^  Update (alias C)
                   %MD^ Move row down one           %V^  View
                   %Mx^ Move row up (-#) down(+#)   %any^with Dataset only
                   %/^  Popup Selection menu
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
%    TSO EDSL sel group member+(or member mask)
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
      if pos("/",dsn) > 0 then do
         edsdsn = strip(dsn)
         return
         end
      dsn = translate(dsn)
      if pos('(',dsn) > 0
      then do
        parse value dsn with ld'('mem')'rd
        dsn = ld''rd
      end
      else mem = null
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
      end
      else do
      if pos("/",dsn) = 0 then
        if pos('*',dsn) = 0 then do
        x = listdsi(dsn)
        if mem /= null
        then sysdsname = sysdsname'('mem')'
        dsn = "'"sysdsname"'"
      end
      end
      edsdsn = edsdsn dsn
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
>Skel edshelp
 Function:
 The EDSL exec invokes the Extended Data Set List dialog.

 The Enhanced Data Set List dialog makes it easy to access an individual
 dataset or OMVS file (Browse/Edit/View), groups of datasets (DSList or
 Browse/Edit/View), or an OMVS directory (UDList).

 A group will create or replace a list within the existing ISPF Personal
 Data Set List structure after an Open in DSLIST has been issued for the
 group.  The lists can be accessed in ISPF by the REFOPEND command.

 ---
 Copyleft (C) 2020, Lionel Dyck and Janko Kalinic

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

   http://www.gnu.org/licenses/

 Syntax:

   %EDSL
         Open the EDSL list of groups
   %EDSL group
         open the specified group using the default selection option
         Type    selection option
         D       Member List
         G       Member List
         L       DSList
         O       UDList
   %EDSL sel group
         open the group using the sel option (e.g. E)
   %EDSL sel group member (or member mask)
         open the group using the sel option (e.g. E)

 Aliases  - none.
 Required - none.

Operands:
  group        - open the specified group using the default selection
                 option.
  sel          - selection option
                 (E=Edit, B=Browse, V=View, O=DSList/UDList)
  member       - member name or mask.

<PRIMARY><COMMANDS>

    Find    - Find the provided string
    HEL cmd - Display EDSL help for a cmd
    History - Display modification history
    Insert  - Insert a row into the table
    REFresh - Refresh the group tree display        (Tree)
    RFind   - Repeat find of the provided string    (Tree)
    Set     - Display the settings window
    Table   - Switch to ISPF table view             (Tree)
    Tree    - Display the group tree view           (Table)
    Version - Display EDSL� version number
    X ALL   - Exclude all group tree nodes          (Tree)
    Qref    - Display EDSL� quick reference
    Debug   - Dump table for debugging              (Tree-undoc)
    Metrics - Display EDSTYPE statistics            (Tree-undoc)
    ISPList - Write tree table to ISPLIST           (Tree-undoc)
    /       - Popup selection menu

<LINE><COMMANDS>

  B   -  Browse
  D   -  Delete to delete a row
  E   -  Edit
  I   -  Insert a row
  M   -  Move row up one
  MD  -  Move row down one                 (Table)
  Mx  -  Move row up (-#) down(+#)         (Table)
  N   -  Move row down one                 (Tree)
  /   -  Popup Selection menu
  O   -  Open in Dataset List
  R   -  To display the group              (Table)
  S   -  Select
  .   -  Cursor select (S)                 (Tree)
  U   -  Update (alias C)
  V   -  View
  X   -  Exclude tree node (toggle)        (Tree)
  any -  (with Dataset only)               (Table)

 Syntax: line command

<FIND>
 Function:

 Find will find the value in the display.

 Syntax: Find value

<RFIND>
 Function:

 Repeat Find will find the next value in the display.

 The ISPF command table must have a user controlled variable for RFIND
 in order for this to work properly.

    Verb      T  Action
                    Description
    RFIND     0  &USRRFIND
                    User controlled variable for RFIND
    RFIND     0  SETVERB
                    REPEAT FIND

 If you can not update the installation ISPF command table, here is
 a rexx solution you can add to EDSL early on in the exec.

    zctverb  = "RFIND"
    zcttrunc = 0
    zctact   = "&USRRFIND"
    zctdesc  = "User controlled variable for RFIND"
    'vget (zsctpref)'
    ctab = zsctpref'cmds'
    'tbtop' ctab
    'tbsarg' ctab
    'tbscan' ctab 'arglist(zctdesc) condlist(EQ) Next'
    if rc > 0 then 'tbadd' ctab
    usrrfind = null
    'vput (usrrfind)'

 Syntax: RFind

<REFRESH>
 Function:

 REFresh will refresh the Tree Display by reading the EDSL ISPF
 table.


 Syntax: REFresh

<SET>
 Function:

 Set will display the settings pop-up and allow you to change the
 display view (TABLE or TREE).

 Syntax: Set

<INSERT>
 Function:

 Insert will insert a row into the table

 Syntax: Insert

<TREE>
 Function:

 Tree will display the the group tree view when in Table display.

 Node Colors:   Group=blue  OMVS=yellow  DSList=green  DSName=pink

  .------------------------------------------------------.
  | EDSL�            Tree Display                        |
  | Command ===>                                         |
  |                                                      |
  | . 'TRIDJK.GEN3'                                      |
  |       'TRIDJK.GEN3'                                  |
  | . GENS-2                                             |
  |       'TRIDJK.GEN3'                                  |
  |       'TRIDJK.GEN7.PDS'                              |
  |       'AD.RACFADM.*'                                 |
  | . SequentialDatasetsBIN...                           |
  |       'TRIDJK.BIN'                                   |
  |       'TRIDJK.BIN2'                                  |
  | . RACFADM Version 130                                |
  |       'ad.racfadm.v130.*'                            |
  | . g5                                                 |
  |       'TRIDJK.GEN.CNTL'                              |
  |       'TRIDJK.GEN3'                                  |
  |   ------------------------                           |
  | . Unix                                               |
  |       /u/tridjk/                                     |
  .------------------------------------------------------.

 Syntax: Tree

<TABLE>
 Function:

 Table will display the ISPF table view when in Tree display.

   .-------------------------------------------------------.
  | EDSL� ----------- Enhanced Data Set List  1.37       |
  | Command ===>                                         |
  |                                                      |
  | Select     Type   Dataset/Group                      |
  |              D    'TRIDJK.GEN3'                      |
  |              L    GENS-2                             |
  |              G    SequentialDatasetsBIN...           |
  |              L    RACFADM Version 130                |
  |              G    g5                                 |
  |                   ------------------------           |
  |              O    Unix                               |
  |              G    aftunix                            |
  |              G    g11                                |
  |                   ------------------------           |
  |              D    'TRIDJK.GEN4'                      |
  .------------------------------------------------------.

 Syntax: Table

<X ALL>
 Function:

 X ALL will exclude all dsname lines in the Tree display.

  .------------------------------------------------------.
  | EDSL�             Tree Display                       |
  | Command ===>                                         |
  |                                                      |
  | + 'TRIDJK.GEN3'                                      |
  | + GENS-2                                             |
  | + SequentialDatasetsBIN...                           |
  | + RACFADM Version 130                                |
  | + g5                                                 |
  |   ------------------------                           |
  | + Unix                                               |
  | + aftunix                                            |
  | + g11                                                |
  |   ------------------------                           |
  | + 'TRIDJK.GEN4'                                      |
  .------------------------------------------------------.

 Syntax: X ALL

<VERSION>
 Function:

 Version will display the EDSL version in the long message area.

 Syntax: Version

  .-------------------------------------.
  | EDSL� 1.37                          |
  | Lionel Dyck and John Kalinich       |
  | The ISPF Cabal - Vive la r�volution |
  | Copyleft - GNU GPL v3               |
  .-------------------------------------.

</>
 Function:

 / will display a point-and-shoot Hotlist Command pop-up panel
 for you to select from.

  .------------ EDSL Primary Commands: -------------.
  | Command ===>                                    |
  |                                                 |
  |  I Insert  - insert a row into the table        |
  |  H History - display the change history of EDSL |
  |  S Set     - display the settings window        |
  |  T Tree    - display a tree view of the table   |
  |  V Version - display EDSL� version number       |
  |  F Find    - find the provided string           |
  |    Find:                                        |
  |                                                 |
  |            Or F3 to cancel                      |
  .-------------------------------------------------.

 Syntax: /

<HISTORY>
 Function:

 History will display a history of EDSL modifications.

 Syntax: History

<QREF>
 Function:

 Qref will display the PGLITE quick reference card.  Commands that are
 only available in the Tree display view are colored blue.

 Syntax: Qref

<ISPLIST>
 Function:

ISPList will write out the tree table to the ISPLIST file.

 Syntax: ISPList

<HEL>
 Function:

 HEL will display the EDSL pseudo TSO Help member that you are
 reading now.

 Syntax: HEL command

<MESSAGES>
 Function:
 The MESSAGE HELP entry is provided to document EDSL messages.

 EDSL has the following messages:

  CHARS 'chars' found
   Search for CHARS 'chars' was successful.

  *Bottom of list reached*
   CHARS 'chars' not found. Press RFIND key to continue the search from
   the top of the member list.

  EDSL� version
   The VERSION command displays the EDSL version information.

  EDSL� Motto...
   When you believe an idea, hold true.
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
