;-----------------------------------------------------------------------------;
; Notes                                                  dgw v0.00 2002.03.03 ;
;                                                                             ;
; Lines in this file are up to 100 characters long (eg use DISP_SIZE 640,480) ;
;                                                                             ;
; To compile to match original binary image required changes marked ***       ;
; Also all occurences of exg ay,ax changed to exg ax,ay (assembler trait)     ;
;                                                                             ;
; Direct references to system variables can be found by searching for the     ;
; text '+sv_base'. Preceding this will be the system variable ie. sv_mcsta    ;
;                                                                             ;
; Code which comprimises addressing range of QDOS (if >16Mb) marked [a]       ;
;                                                                             ;
; [!] denotes lines which I consider coding errors or find confusing          ;
;                                                                             ;
; [d] denotes restrictions in display parameters                              ;
;                                                                             ;
;-----------------------------------------------------------------------------;

        include qdos_defns

;Unavoidable absolute references (to internal ROM routines/data).

MAD9A   equ     $AD9A         ;character fonts #1
MB106   equ     $B106         ;    "       "   #2
MB458   equ     $B458         ;key code table
MBFE2   equ     $BFE2         ;translation table
MBFE6   equ     $BFE6         ;message table
MBFEA   equ     $BFEA         ;serial fetch translation routine
MBFEE   equ     $BFEE         ;   "   send       "         "
MBFF2   equ     $BFF2         ;key matrix to key code routine
MBFF6   equ     $BFF6         ;QDOS version string
MBFFA   equ     $BFFA         ;BASIC version string

        org     0

;JS v1.10 ROM starts here

L0000   dc.l    $00030000     ;RESET value of SSP
        dc.l    L016A         ;RESET value of PC
        dc.l    L005C         ;Bus error
        dc.l    L0028         ;Address error
        dc.l    L002A         ;Illegal instruction
        dc.l    L002C         ;Divide by zero
        dc.l    L002E         ;CHK instruction
        dc.l    L0030         ;TRAPV instruction
        dc.l    L0032         ;Privilege error
        dc.l    L0034         ;Trace

;The exception handler address forms the offset into any redirection table.
;If a table has not been defined for job then exceptions marked @ will loop
;endlessly (return address points to instruction causing exception).

L0028   bsr.s   L0050         ;called by Address error        @  offset $00
L002A   bsr.s   L0050         ;   "      Illegal instruction  @     "   $04
L002C   bsr.s   L0050         ;   "      Divide by zero             "   $08
L002E   bsr.s   L0050         ;   "      CHK instruction            "   $0C
L0030   bsr.s   L0050         ;   "      TRAPV instruction          "   $10
L0032   bsr.s   L0050         ;   "      Privilege error      @     "   $14
L0034   bsr.s   L0050         ;   "      Trace                      "   $18
L0036   bsr.s   L0050         ;   "      Interrupt level 7          "   $1C
L0038   bsr.s   L0050         ;   "      Trap #5                    "   $20
L003A   bsr.s   L0050         ;   "        "   6                    "   $24
L003C   bsr.s   L0050         ;   "        "   7                    "   $28
L003E   bsr.s   L0050         ;   "        "   8                    "   $2C
L0040   bsr.s   L0050         ;   "        "   9                    "   $30
L0042   bsr.s   L0050         ;   "        "   10                   "   $34
L0044   bsr.s   L0050         ;   "        "   11                   "   $38
L0046   bsr.s   L0050         ;   "        "   12                   "   $3C
L0048   bsr.s   L0050         ;   "        "   13                   "   $40
L004A   bsr.s   L0050         ;   "        "   14                   "   $44
L004C   bsr.s   L0050         ;   "        "   15                   "   $48
                         
        dc.w    0

L0050   bra     L013C

;No redirection table found for exception

L0054   subi.l  #$2A,(a7)+    ;called from Address error ?
        bne.s   L005E         ;no
L005C   addq.w  #8,a7         ;discard data stacked from exception
L005E   rte

;Normal Motorola vectors

        dc.l    L005E         ;Spurious interrupt
        dc.l    L005E         ;Interrupt level 1
        dc.l    L0352         ;    "       "   2
        dc.l    L005E         ;    "       "   3
        dc.l    L005E         ;    "       "   4
        dc.l    L005E         ;    "       "   5
        dc.l    L005E         ;    "       "   6
        dc.l    L0036         ;    "       "   7
        dc.l    L031A         ;Trap #0
        dc.l    L031E         ;  "   1
        dc.l    L0324         ;  "   2
        dc.l    L032A         ;  "   3
        dc.l    L0330         ;  "   4
        dc.l    L0038         ;  "   5
        dc.l    L003A         ;  "   6
        dc.l    L003C         ;  "   7
        dc.l    L003E         ;  "   8
        dc.l    L0040         ;  "   9
        dc.l    L0042         ;  "   10
        dc.l    L0044         ;  "   11
        dc.l    L0046         ;  "   12
        dc.l    L0048         ;  "   13
        dc.l    L004A         ;  "   14
        dc.l    L004C         ;  "   15

;QDOS vectors (non-standard Motorola)

        dc.w    L2FAE         ;$C0  mm_alchp
        dc.w    L305E         ;$C2  mm_rechp
        dc.w    L39F2         ;$C4  ut_windw
        dc.w    L39F6         ;$C6  ut_con
        dc.w    L39FC         ;$C8  ut_scr
        dc.w    L395E         ;$CA  ut_err0
        dc.w    L3968         ;$CC  ut_err
        dc.w    L3990         ;$CE  ut_mint
        dc.w    L39B2         ;$D0  ut_mtext
        dc.w    L39DC         ;$D2  ut_link
        dc.w    L39E2         ;$D4  ut_unlnk
        dc.w    0             ;$D6  (undefined)
        dc.w    L3104         ;$D8  mm_alloc
        dc.w    L3162         ;$DA  mm_linkfr
        dc.w    L37F4         ;$DC  io.qset
        dc.w    L380A         ;$DE  io.qtest
        dc.w    L3838         ;$E0  io.qin
        dc.w    L385E         ;$E2  io.qout
        dc.w    L3888         ;$E4  io.qeof
        dc.w    L3A9C         ;$E6  ut_cstr
        dc.w    L37CC         ;$E8  io.serq
        dc.w    L388C         ;$EA  io.serio
        dc.w    L405E         ;$EC  cn_date
        dc.w    L40BE         ;$EE  cn_day
        dc.w    L3EF6         ;$F0  cn_ftod
        dc.w    L3E54         ;$F2  cn_itod
        dc.w    L3EDC         ;$F4  cn_itobb
        dc.w    L3ED8         ;$F6  cn_itobw
        dc.w    L3ED4         ;$F8  cn_itobl
        dc.w    L3EB0         ;$FA  cn_itohb
        dc.w    L3EAC         ;$FC  cn_itohw
        dc.w    L3EA8         ;$FE  cn_itohl
        dc.w    L3D16         ;$100 cn_dtof
        dc.w    L3DC2         ;$102 cn_dtoi
        dc.w    L3E34         ;$104 cn_btoib
        dc.w    L3E38         ;$106 cn_btoiw
        dc.w    L3E3C         ;$108 cn_btoil
        dc.w    L3DD4         ;$10A cn_htoib
        dc.w    L3DD8         ;$10C cn_htoiw
        dc.w    L3DDC         ;$10E cn_htoil
        dc.w    L6DA6         ;$110 bp_init
        dc.w    L61DA         ;$112 ca_gtint
        dc.w    L61DE         ;$114 ca_gtfp
        dc.w    L61D6         ;$116 ca_gtstr
        dc.w    L61E2         ;$118 ca_gtlin
        dc.w    L4E4E         ;$11A bv_chrix
        dc.w    L41AC         ;$11C ri_exec
        dc.w    L41B4         ;$11E ri_execb
        dc.w    L72C2         ;$120 bp_let
        dc.w    L372C         ;$122 io.name
        dc.w    L525C-L4000   ;$124 md_read
        dc.w    L51B0-L4000   ;$126 md_write
        dc.w    L5262-L4000   ;$128 md_verin
        dc.w    L523A-L4000   ;$12A md_sectr                            (Minerva vector name)
        dc.w    L87D4-L4000   ;$12C basic syntax analyser               (pa_graph)
        dc.w    L8B5A-L4000   ;$12E commands syntax table               (pa_table)
        dc.w    L8CE7-L4000   ;$130 expressions syntax table            (pa_expr)
        dc.w    L8AB4-L4000   ;$132 format precompiled basic line       (pa_strip)
        dc.w    L8A4E-L4000   ;$134 error when compiling                (pa_mist)
        dc.w    L8E88-L4000   ;$136 store precompiled line              (pf_nwlin)
        dc.w    L7518-L4000   ;$138 convert precompiled basic to ascii  (pf_liste)
        dc.w    L890C-L4000   ;$13A initialise basic stacks             (pa_ini)

;For redirectable exceptions use jobs exception table, if one exists.

L013C   tst.l   sv_trapv+sv_base
        beq     L0054                   ;no vector redirection table
        move.l  a6,-(a7)
        movea.w 6(a7),a6                ;return address (lower word)
        adda.w  a6,a6                   ;create table offset
        adda.l  sv_trapv+sv_base,a6     ;to form handler routine
        move.l  (a6),4(a7)              ;on return
        movea.l (a7)+,a6
        rts

;RAM failure, write continuosly to display memory.

L015C   movea.l a3,a5                   ;start of screen 0
L015E   move.w  d7,(a5)+
        cmpa.l  #sv_base,a5             ;to end of screen 0
        bne.s   L015E
        bra.s   L015C                   ;then repeat

;RESET value of PC. Find top of system RAM (multiple 64K).

L016A   movea.l #ra_bot+ra_min,a1       ;first location above standard 128K
        movea.l a1,a4
L0172   move.l  a4,(a4)                 ;write address value to address location
        cmpa.l  (a4),a4                 ;found top ?
        bne.s   L0184                   ;yes
        cmpa.l  (a1),a1                 ;was it a shadow ?
        bne.s   L0184                   ;yes
        adda.l  #$00010000,a4           ;next 64K boundary
        bra.s   L0172

;Do RAM test. First pass writes/read $00, $FF and a random data byte to every location.
;If this first test fails a white screen is displayed (bit failure). A second test is then
;performed which, unlike the first simple test allows a finite time to pass (based on
;total amount of memory) before verifying previous data written is still valid. If this
;second test fails then a green screen is displayed (DRAM refresh).

L0184   moveq   #0,d0                   ;pass 1, first test data
        moveq   #-1,d1                  ;pass 1, second test data
        moveq   #-1,d7                  ;white screen for first test failure
L018A   lea     ra_bot,a3               ;start at bottom of RAM
        movea.l a3,a5
        movea.l d0,a1                   ;use ROM as varied data, start at 0
        lea     8164(a1),a2             ;cycle back when at address 8164
        sf      mc_stat-ra_bot(a3)      ;display on, 4 colour, screen 0
L019C   cmpa.l  a1,a2                   ;cycle point ?
        bne.s   L01A2                   ;no
        movea.l d0,a1                   ;reset to base of ROM
L01A2   move.l  (a1)+,d2                ;get ROM data
        tst.b   d7                      ;pass 2 ?
        beq.s   L01C2                   ;yes
        move.l  d0,(a5)                 ;write 0
        cmp.l   (a5),d0                 ;verify 0 ?
        bne.s   L015C                   ;ram failure pass 1
        move.l  d1,(a5)                 ;write -1
        cmp.l   (a5),d1                 ;verify -1 ?
        bne.s   L015C                   ;ram failure pass 1
        move.l  d2,(a5)                 ;write ROM data
        cmp.l   (a5)+,d2                ;verify ROM data ?
        bne.s   L015C                   ;ram failure pass 1
        cmpa.l  a5,a4                   ;top of ram ?
        bne.s   L019C                   ;no
        lsl.w   #8,d7                   ;green screen for second pass failure
        bra.s   L018A                   ;now do pass 2
L01C2   cmp.l   (a5),d2                 ;data still valid ?
        bne.s   L015C                   ;ram failure pass 2
        clr.l   (a5)+                   ;clear ram as we go
        cmpa.l  a4,a5                   ;top of ram ?
        bne.s   L019C                   ;no

;RAM test passed, continue by setting up system.

        movea.l #L4A74,a1               ;when finished setting up jump here (see below)
        lea     sv_base,a6              ;a6 mostly points here (except BASIC)
        lea     sv_stact(a6),a7         ;SSP always here

;Initialise hardware

        lea     pc_mctrl,a3                  ;microdrive/link cotrol port
        move.b  #$08,mc_stat-pc_mctrl(a3)    ;display on, 8 colour, screen#0
        sf      pc_tctrl-pc_mctrl(a3)        ;net out high, serial mode by default
        move.l  #$061F0000,(a3)              ;pc_mctrl / pc_tctrl / pc_trak1 / pc_trak2
        move.b  #$1F,pc_intr-pc_mctrl(a3)    ;(as set above) interrupt register port
        move.b  #$01,pc_ipcwr-pc_mctrl(a3)   ;condition IPC write port
        move.b  #$C0,sv_pcint(a6)            ;(gap interrupt to be masked)
        jsr     L2C50(pc)                    ;turn off all microdrive motors

;Continue initialising system variables, setting up system tables.

        move.w  #sv.ident,(a6)          ;set identification word
        move.l  a5,sv_ramt(a6)          ;top of RAM found
        suba.l  #0,a5                   ;(redundant function ?)
        move.l  a5,sv_respr(a6)         ;RESPR area empty
        move.l  a5,sv_trnsp(a6)         ;TRNSP area empty
        move.l  sv_ramt(a6),d0
        sub.l   a6,d0
        lsr.l   #6,d0                   ;1/64 of total memory (excluding screen#0)
        movea.l a7,a3                   ;memory block table starts above SSP
        lea     sv_btpnt(a6),a4
        bsr.s   L0250                   ;set sv_btpnt, sv_btbas and sv_bttop
        lsr.l   #1,d0
        add.l   d0,sv_btpnt(a6)         ;current entry is mid-position of block table
        lsr.l   #2,d0                   ;1/256 total memory
        addi.l  #32,d0                  ;minimum of 8
        cmpi.w  #480,d0                 ;maximum of 120 job table entries
        bls.s   L0246
        move.w  #480,d0
L0246   bsr.s   L0250                   ;set sv_jbpnt, sv_jbbas and sv_jbtop
        mulu.w  #3,d0                   ;maximum 360 channel table entries
        bsr.s   L0250                   ;set sv_chpnt, sv_chbas and sv_chtop
        bra.s   L025C

;Set table pointers

L0250   move.l  a3,(a4)+                ;current entry pointer
        move.l  a3,(a4)+                ;start of table pointer
        adda.w  d0,a3                   ;size of table
        move.l  a3,(a4)+                ;end of table pointer
        addq.w  #4,a4
        rts

;Set up entries in memory block table accordingly.

L025C   move.l  a3,d0                   ;from sv_chtop
        sub.l   a6,d0                   ;down to sv_base
        lsr.l   #6,d0                   ;each block table entry
        movea.l d0,a4
        adda.l  a7,a4                   ;are left as unavailable =0 (bt_unav)
        movea.l sv_bttop(a6),a3         ;but the rest, to the top of the table
        move.w  #512,d1                 ;(initial minimum BASIC space)
        lsr.w   #6,d1
        suba.w  d1,a3                   ;except the last entry
        moveq   #bt.empty,d0            ;is available for slaving
L0274   move.b  d0,(a4)                 ;flag memory block is empty
        addq.w  #bt_end,a4              ;next table entry
        cmpa.l  a3,a4                   ;last ?
        blt.s   L0274                   ;no

;Set all job and channel table entries as free.

        movea.l sv_jbbas(a6),a4         ;start at job table base
        movea.l sv_chtop(a6),a3         ;to the top of channel table
        moveq   #$FF,d0                 ;flag for entry free/unused
L0286   move.b  d0,(a4)
        addq.w  #4,a4                   ;next table entry
        cmpa.l  a3,a4                   ;last ?
        blt.s   L0286                   ;no

;Continue setting up system pointers and variables

        move.l  a3,sv_cheap(a6)         ;heap is presently empty
        move.l  a3,sv_free(a6)          ;free area is all unused memory
        movea.l sv_respr(a6),a4
        lea     -512(a4),a4             ;allocate bare minimum space for BASIC
        move.l  a4,sv_basic(a6)         ;pointer to BASIC job header and data space
        lea     L2CF8(pc),a5            ;internal polled tasks
        move.l  a5,sv_plist(a6)         ;establish link
        lea     L1202(pc),a5            ;internal scheduler tasks
        move.l  a5,sv_shlst(a6)         ;establish link
        lea     L0AC0(pc),a5            ;list of internal I/O drivers
        move.l  a5,sv_drlst(a6)         ;establish link
        lea     L1230(pc),a5            ;list of internal directory drivers
        move.l  a5,sv_ddlst(a6)         ;establish link
        addq.b  #1,sv_netnr(a6)         ;NET number
        addq.b  #8,sv_mcsta(a6)         ;copy of master chip register (display setting)
        addq.b  #1,sv_tmode(a6)         ;transmit mode is serial / baud 9600 / NET high
        addq.w  #1,sv_timov(a6)         ;timing for serial output (based on baud)
        move.w  #30,sv_ardel(a6)        ;key repeat delay
        addq.w  #2,sv_arfrq(a6)         ;key repeat frequency
        addq.w  #3,sv_cqch(a6)          ;keyboard queue control key (CTRL C)
        move.l  MBFE6,sv_mgtab(a6)      ;default message table
        move.l  MBFE2,sv_trtab(a6)      ;default translation table
        movea.l sv_jbbas(a6),a4         ;base entry is job 0
        move.l  a4,sv_jbpnt(a6)         ;which is now the current job
        movea.l sv_trnsp(a6),a3         ;top of BASIC job data space
        clr.l   -(a3)                   ;stack is empty
        movea.l sv_basic(a6),a0         ;BASIC job header
        move.l  a0,(a4)                 ;to job 0 table entry
        move.b  #32,jb_princ(a0)        ;set job priority increment
        move    a3,usp                  ;BASIC job stack pointer
        lea     jb_end(a0),a6           ;a6 always points to data area for BASIC
        movea.l a3,a5                   ;pointer to end of jobs data area
        suba.l  a6,a5                   ;is always relative a6
        move.w  #$0000,sr               ;user mode, interrupts on
        jmp     (a1)                    ;set up BASIC job, scan for ROMs, start BASIC

;Trap #0 exception

L031A   addq.w  #2,a7                   ;discard user status register
        rts                             ;return in supervisor mode

;Trap #1 exception

L031E   bsr.s   L0336                   ;save trap level registers
        bra     L0460                   ;perform trap #1 operation

;Trap #2 exception

L0324   bsr.s   L0336                   ;save trap level registers
        bra     L32A2                   ;perform trap #2 operation

;Trap #3 exception

L032A   bsr.s   L0336                   ;save trap level registers
        bra     L337C                   ;perform trap #3 operation

;Trap #4 exception

L0330   bsr.s   L0336                   ;save trap level registers
        bra     L3432                   ;perform trap #4 operation

;Save trap level registers

L0336   subq.w  #8,a7                   ;make space
        move.l  8(a7),-(a7)             ;shift return address
        movem.l d7/a5-a6,4(a7)          ;preserve trap level registers
        movea.l #sv_base,a6             ;always during trap
        lea     4(a7),a5                ;pointer to trap level stack
        moveq   #$7F,d7                 ;mask
        and.l   d7,d0                   ;limit operation code passed
        rts

;Interrupt level 2

L0352   movem.l d7/a5-a6,-(a7)          ;preserve interrupt level registers
        movea.l a7,a5                   ;stack recovery
        movea.l #sv_base,a6             ;always during interrupt handlers
        move.b  pc_intr,d7              ;get register setting from port
        lsr.b   #1,d7                   ;shift each interrupt bit into carry
        bcs     L2ABC                   ;microdrive gap interrupt
        lsr.b   #1,d7
        bcs     L2CCC                   ;interface interrupt
        lsr.b   #1,d7
        bcs     L2CD8                   ;transmit interrupt
        lsr.b   #1,d7
        bcs     L0900                   ;frame interrupt
        lsr.b   #1,d7
        bcc.s   L03A0                   ;no interrupt

;External interrupt handler

        movem.l d0-d6/a0-a4,-(a7)
        moveq   #-sv_lxint,d0           ;offset to linkage base
        movea.l sv_i2lst(a6),a0
        jsr     L0A9E(pc)               ;go through handler list
        move.b  sv_pcint(a6),d7
        ori.b   #pc.intre,d7            ;clear external interrupt
        move.b  d7,pc_intr              ;interrupt register port
        movem.l (a7)+,d0-d6/a0-a4
L03A0   bra     L03B6

;Return from trap, via scheduler if called in user mode

L03A4   moveq   #err.ok,d0
L03A6   btst    #5,trlv_sr(a7)          ;trap called in supervisor mode ?
        bne.s   L03B6                   ;yes, return directly
        tst.w   sv_pollm(a6)            ;polls missed ?
        bne     L0936                   ;yes, return via scheduler
L03B6   movem.l (a7)+,d7/a5-a6          ;restore trap level registers
        rte

;Return current job ID or check validity of job ID given

L03BC   tst.w   d1                      ;job ID given
        bge.s   L03D8                   ;yes
        move.l  sv_jbpnt(a6),d1         ;else get current job table entry
        movea.l d1,a0
        movea.l (a0),a0                 ;job header
        sub.l   sv_jbbas(a6),d1
        lsr.l   #2,d1                   ;job number
        swap    d1
        move.w  jb_tag(a0),d1           ;add its tag
        swap    d1                      ;return job ID
L03D6   rts

L03D8   bsr.s   L03E4                   ;verify job ID
        beq.s   L03D6                   ;job ID OK
        moveq   #err.nj,d0              ;bad job
        addq.w  #$04,a7                 ;abort operation  [!] #$14
        dc.w    $6000    *** bra L03A6  ;return from trap
        dc.w    L03A6-*

;Verify job ID in d1.l, return EQ if valid.

L03E4   cmp.w   sv_jbmax(a6),d1
        bhi.s   L03D6                   ;job number too high
        movea.w d1,a0
        adda.w  a0,a0
        adda.w  a0,a0                   ;form table entry
        adda.l  sv_jbbas(a6),a0
        tst.b   (a0)                    ;valid job table entry ?
        blt.s   L03D6                   ;no
        movea.l (a0),a0                 ;get pointer to job header
        swap    d1
        cmp.w   jb_tag(a0),d1           ;correct tag ?
        bne.s   L03D6                   ;no
        swap    d1
        cmp.b   d1,d1                   ;job ID given is OK
        rts

;Return current job ID in d0.l and job header in a3

L0408   movea.l sv_jbpnt(a6),a3         ;current job table entry
        move.l  a3,d0
        sub.l   sv_jbbas(a6),d0         ;less base of table
        lsr.w   #2,d0                   ;yields job number
        movea.l (a3),a3                 ;pointer to job header
        swap    d0
        move.w  jb_tag(a3),d0           ;add job tag
        swap    d0                      ;return job ID
        rts

;Set microdrive or network output mode

L0420   move.b  d0,-(a7)                ;required transmit mode
L0422   subq.w  #1,sv_timo(a6)          ;counter for timing serial output
        blt.s   L0432                   ;has now expired (or was never set)
        move.w  #8331,d0                ;for each timing count (based on baud)
L042C   dbf     d0,L042C                ;wait in loop
        bra.s   L0422                   ;until serial output finishes
L0432   clr.w   sv_timo(a6)             ;reset counter
        andi.b  #pc.notmd,sv_tmode(a6)  ;serial mode by default
        move.b  (a7)+,d0                ;get transmit mode requested
        or.b    d0,sv_tmode(a6)         ;set mode
        andi.b  #$7F,sv_pcint(a6)       ;disable (serial) transmit interrupts
L0448   move.b  sv_tmode(a6),pc_tctrl   ;transmit mode register to hardware port
        rts

;Prepare for serial transmit mode 

L0452   bclr    #pc..serb,sv_tmode(a6)  ;set serial mode
        ori.b   #pc.maskt,sv_pcint(a6)  ;enable transmit interrupts
        bra.s   L0448                   ;set pc_tctrl port

;Call routine for requested Trap #1 operation

L0460   cmpi.w  #mt.cntry,d0            ;highest supported operation
        bhi.s   L04BC                   ;undefined operation, err.bp
        move.w  d0,d7
        add.w   d7,d7                   ;operation code creates offset
        move.w  L0472(pc,d7.w),d7       ;to get the table offset
        jmp     L0460(pc,d7.w)          ;which points to routine

;Offsets to Trap #1 operations

L0472   dc.w    L04C2-L0460             ;$00 mt.inf
        dc.w    L054C-L0460             ;$01 mt.cjob
        dc.w    L0524-L0460             ;$02 mt.jinf
        dc.w    L04BC-L0460             ;$03 (undefined)
        dc.w    L05EA-L0460             ;$04 mt.rjob
        dc.w    L0602-L0460             ;$05 mt.frjob
        dc.w    L30E0-L0460             ;$06 mt.free
        dc.w    L070A-L0460             ;$07 mt.trapv
        dc.w    L0960-L0460             ;$08 mt.susjb
        dc.w    L0972-L0460             ;$09 mt.reljb
        dc.w    L09A0-L0460             ;$0A mt.activ
        dc.w    L0990-L0460             ;$0B mt.prior
        dc.w    L071E-L0460             ;$0C mt.alloc
        dc.w    L072C-L0460             ;$0D mt.lnkfr
        dc.w    L076A-L0460             ;$0E mt.alres
        dc.w    L076A-L0460             ;$0F mt.reres
        dc.w    L07C2-L0460             ;$10 mt.dmode
        dc.w    L0872-L0460             ;$11 mt.ipcom
        dc.w    L0882-L0460             ;$12 mt.baud
        dc.w    L195A-L0460             ;$13 mt.rclck
        dc.w    L195A-L0460             ;$14 mt.sclck
        dc.w    L195A-L0460             ;$15 mt.aclck
        dc.w    L07A4-L0460             ;$16 mt.albas
        dc.w    L07AA-L0460             ;$17 mt.rebas
        dc.w    L073A-L0460             ;$18 mt.alchp
        dc.w    L075E-L0460             ;$19 mt.rechp
        dc.w    L08E6-L0460             ;$1A mt.lxint
        dc.w    L08F2-L0460             ;$1B mt.rxint
        dc.w    L08E6-L0460             ;$1C mt.lpoll
        dc.w    L08F2-L0460             ;$1D mt.rpoll
        dc.w    L08E6-L0460             ;$1E mt.lschd
        dc.w    L08F2-L0460             ;$1F mt.rschd
        dc.w    L08E6-L0460             ;$20 mt.liod
        dc.w    L08F2-L0460             ;$21 mt.riod
        dc.w    L08E6-L0460             ;$22 mt.ldd
        dc.w    L08F2-L0460             ;$23 mt.rdd
        dc.w    L04D4-L0460             ;$24 mt.cntry

;Trap #1 D0=$03 or > $24, operation undefined

L04BC   moveq   #err.bp,d0
        bra     L03A6                   ;return from trap

;Trap #1 D0=$00 MT.INF - Get system information

L04C2   moveq   #my.job,d1              ;current job
        jsr     L03BC(pc)               ;get job ID
        move.l  MBFF6,d2                ;ascii of version
        movea.l a6,a0                   ;sv_base
        bra     L03A4                   ;return from trap err.ok

;Trap #1 D0=$24 MT.CNTRY - Set error messages and printer translate tables

L04D4   tst.l   d2                      ;new message table ?
        beq.s   L04EA                   ;no
        btst    #0,d2                   ;even address ?
        bne.s   L051E                   ;no
        movea.l d2,a0
        cmpi.w  #$4AFB,(a0)             ;correct header ?
        bne.s   L051E                   ;no
        move.l  a0,sv_mgtab(a6)         ;set new message table
L04EA   clr.b   sv_tran(a6)             ;flag don't translate
        tst.l   d1                      ;new translation table ?
        beq.s   L0518                   ;no, finished
        cmpi.l  #1,d1                   ;local translation table ?
        bne.s   L0500                   ;no
        move.l  MBFE2,d1                ;use default translation table
L0500   btst    #0,d1                   ;even address ?
        bne.s   L051E                   ;no
        movea.l d1,a0
        cmpi.w  #$4AFB,(a0)             ;correct header ?
        bne.s   L051E                   ;no
        move.b  #1,sv_tran(a6)          ;flag use translate table
        move.l  a0,sv_trtab(a6)         ;at this address
L0518   moveq   #err.ok,d0
        bra     L03A6                   ;return from trap

L051E   moveq   #err.bp,d0
        bra     L03A6                   ;return from trap

;Trap #1 D0=$02 MT.JINF - Get job information (of subject job)

L0524   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        moveq   #0,d3
        tst.w   jb_stat(a0)             ;job active ?
        beq.s   L0532                   ;yes
        moveq   #-1,d3                  ;negate long word for subject suspended
L0532   move.b  jb_princ(a0),d3         ;get job priority increment
        move.l  d2,d0                   ;job ID at top of tree
        move.l  a0,-(a7)                ;preserve job header of subject
        jsr     L06C6(pc)               ;get ID of next job in tree
        movea.l (a7)+,a0
        move.l  jb_owner(a0),d2         ;owner of subject
        lea     jb_end(a0),a0           ;start of subject job (after header)
        bra     L03A4                   ;return from trap err.ok

;Trap #1 D0=$01 MT.CJOB - Create job

L054C   tst.l   d1                      ;job 0 to be the owner ?
        beq.s   L0554                   ;yes, so skip
        jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
L0554   movem.l d1-d3/a1-a4,-(a7)
        moveq   #0,d7                   ;eventual job number
        movea.l sv_jbbas(a6),a4
L055E   tst.b   (a4)                    ;job entry free ?
        blt.s   L0570                   ;yes
        addq.w  #1,d7                   ;update job number
        addq.w  #4,a4                   ;next entry
        cmpa.l  sv_jbtop(a6),a4         ;end of job table ?
        blt.s   L055E                   ;no
        moveq   #err.nj,d0              ;busy machine !
        bra.s   L05E0                   ;abort operation

L0570   moveq   #jb_end,d1              ;size of job header
        add.l   d2,d1                   ;add requested code space
        add.l   d3,d1                   ;add requested data space
        jsr     L2FFA(pc)               ;get space in transient program area
        bne.s   L05E0                   ;no space, abort
        movem.l (a7),d1-d3/a1
        move.l  a0,(a4)                 ;set job pointer in job table
        cmp.w   sv_jbmax(a6),d7         ;is job number higher than highest ?
        bls.s   L058C                   ;no
        move.w  d7,sv_jbmax(a6)         ;update highest job in job table
L058C   addq.w  #4,a0                   ;from jb_start
        moveq   #24,d0                  ;to jb_end
L0590   clr.l   (a0)+                   ;clear job header
        dbf     d0,L0590
        suba.w  #jb_end-jb_owner,a0
        move.l  d1,(a0)                           ;jb_owner, job owner as supplied
        addq.w  #8,a0
        swap    d7                                ;job number
        move.w  sv_jbtag(a6),d7                   ;next available job tag
        move.w  d7,(a0)                           ;jb_tag
        swap    d7                                ;created job ID
        addq.w  #1,sv_jbtag(a6)                   ;update next job tag issued
        move.l  sv_trapv(a6),jb_trapv-jb_tag(a0)  ;redirection table is inherited from owner
        adda.w  #jb_a4-jb_tag,a0
        move.l  d2,(a0)+                          ;jb_a4, code space (offset for a6)
        add.l   d2,d3
        move.l  d3,(a0)+                          ;jb_a5, code + data space (offset for a6)
        moveq   #jb_end-jb_a6,d0
        add.l   a0,d0
        move.l  d0,(a0)+                ;jb_a6, pointer to start of code area
        add.l   d0,d3
        exg     d3,a0                   ;pointer to end of job area
        clr.l   -(a0)                   ;initial stack (no channels or command string)
        exg     d3,a0
        move.l  d3,(a0)                 ;jb_a7, job stack pointer
        addq.w  #jb_pc-jb_a7,a0
        move.l  a1,d3                   ;supplied job start address
        beq.s   L05D4                   ;defaults to start of code area (jb_end)
        move.l  d3,d0                   ;else use it as
L05D4   move.l  d0,(a0)                 ;jb_pc, program counter
        move.l  d0,jb_start-jb_pc(a0)   ;and store in job header
        lea     jb_end-jb_pc(a0),a0     ;return start of code space (jb_end)
        moveq   #err.ok,d0              ;all done
L05E0   movem.l (a7)+,d1-d3/a1-a4
        move.l  d7,d1                   ;ID of job created
        bra     L03A6                   ;return from trap

;Trap #1 D0=$04 MT.RJOB - Remove a job

L05EA   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        move.l  d1,d0                   ;include all jobs owned by this one
L05F0   tst.b   jb_princ(a0)            ;job inactive ?
        bne     L06C0                   ;no, abort operation with err.nc
        jsr     L06C6(pc)               ;get ID of next job in job tree
        tst.l   d1                      ;end of tree ?
        bne.s   L05F0                   ;no, check all jobs first
        move.l  d0,d1                   ;before going on a killing spree...

;Trap #1 D0=$05 MT.FRJOB - Force remove a job

L0602   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        move.l  d1,d0                   ;include all jobs owned by job
        beq     L06C0                   ;but never remove job 0 !
        movea.w d1,a1                   ;use job number
        adda.w  a1,a1
        adda.w  a1,a1                   ;to form
        adda.l  sv_jbbas(a6),a1         ;job table entry
L0616   addq.b  #1,(a1)                 ;mark entry as victim [a]
        jsr     L06C6(pc)               ;get ID of next job in tree
        tst.l   d1                      ;end of tree ?
        bne.s   L0616                   ;no, get next
        sf      -(a7)                   ;flag 'not the current job'
        moveq   #0,d1                   ;job number
        movea.l sv_jbbas(a6),a1
L0628   addq.w  #4,a1                   ;to next entry in table
        addq.w  #1,d1                   ;next job
        cmp.w   sv_jbmax(a6),d1         ;highest used ?
        bhi     L06B6                   ;yes
        tst.b   (a1)                    ;is job entry in use ?
        ble.s   L0628                   ;no, get next
        sf      (a1)                    ;else it is a victim [a]
        movea.l (a1),a0                 ;pointer to job header
        swap    d1
        move.w  jb_tag(a0),d1           ;job tag
        swap    d1                      ;forms job ID
        cmpa.l  sv_jbpnt(a6),a1         ;current job ?
        bne.s   L064C                   ;no
        st      (a7)                    ;flag 'current job' (suicide)
L064C   tst.b   jb_wflag(a0)            ;another job waiting on victim ?
        beq.s   L0670                   ;no
        move.l  jb_wjob(a0),d0          ;ID of job waiting
        exg     d0,d1
        jsr     L03E4(pc)               ;verify job ID in d1.l
        exg     d1,d0
        bne.s   L0670                   ;waiting job ID was invalid
        cmpi.w  #-2,jb_stat(a0)         ;othewise was it waiting on a job to complete ?
        bne.s   L0670                   ;no
        clr.w   jb_stat(a0)             ;re activate job
        move.l  d3,jb_d0(a0)            ;this is the error code
L0670   movea.l sv_cheap(a6),a0         ;base of common heap
L0674   cmp.l   hp_owner(a0),d1         ;block owned by job ?
        bne.s   L069A                   ;no
        movem.l d1/d3/a0-a1,-(a7)
        move.l  hp_rflag(a0),d1         ;a location to be set on release ?
        beq.s   L0688                   ;no
        movea.l d1,a1
        st      (a1)                    ;flag that heap has been released
L0688   movea.l hp_drivr(a0),a1         ;driver of heap block
        lea     -sv_lio(a1),a3          ;base driver linkage (possibly meaningless)
        movea.l hp_close(a1),a1         ;close routine (or release heap)
        jsr     (a1)                    ;clear up
        movem.l (a7)+,d1/d3/a0-a1
L069A   adda.l  (a0),a0                 ;next heap link
        cmpa.l  sv_free(a6),a0          ;end of heap area ?
        blt.s   L0674                   ;no, look at next heap block
        movem.l d1/d3/a1,-(a7)
        movea.l (a1),a0                 ;get pointer to job header of victim
        jsr     L308C(pc)               ;clear up transient program space used by job
        movem.l (a7)+,d1/d3/a1
        st      (a1)                    ;job table entry is now free
        bra     L0628                   ;look for more victims

L06B6   tst.b   (a7)+                   ;was job removed the current job ?
        beq     L03A4                   ;no, return from trap err.ok
        bra     L093A                   ;otherwise forced return via scheduler

L06C0   moveq   #err.nc,d0              ;job removal denied
        bra     L03A6                   ;return from trap

;Return in d1.l the job ID of a job owned by the job whose ID is given in d1.l
;If this fails then use the job ID given in d0.l as the top of the job tree to
;search for the next job in the tree, if there isn't one return d1.l as 0.
;Note, this latter search is done from next entry in job table onwards.

L06C6   move.l  d1,d2                   ;the job in question
        moveq   #0,d1                   ;search the whole job table (bar job 0)
L06CA   addq.w  #1,d1                   ;next entry in job table
        cmp.w   sv_jbmax(a6),d1         ;highest used ?
        bgt.s   L06E0                   ;yes, check if top of tree
        bsr.s   L06EE                   ;get pointers to job
        tst.b   (a1)                    ;is job entry in use ?
        blt.s   L06CA                   ;no, get next
        cmp.l   jb_owner(a0),d2         ;is it owned by the job in question ?
        beq.s   L0700                   ;yes, return the job ID
        bra.s   L06CA                   ;else try next job in table

L06E0   cmp.w   d2,d0                   ;at top of subject jobs own job tree ?
        beq.s   L06FC                   ;yes, end of search
        move.w  d2,d1                   ;else search is now from subjects entry
        bsr.s   L06EE                   ;get pointers to job
        move.l  jb_owner(a0),d2         ;move up a level in the job tree
        bra.s   L06CA                   ;find next job to subject in job tree

L06EE   movea.w d1,a1                   ;job table index
        adda.w  a1,a1
        adda.w  a1,a1                   ;form offset to get
        adda.l  sv_jbbas(a6),a1         ;pointer to job table entry
        movea.l (a1),a0                 ;and pointer to job header
        rts

L06FC   moveq   #0,d1                   ;no more jobs found / end of job tree
        rts

L0700   swap    d1                      ;job number
        move.w  jb_tag(a0),d1           ;and job tag
        swap    d1                      ;yields job ID
        rts

;Trap #1 D0=$07 MT.TRAPV - Set RAM exception table

L070A   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        suba.w  #sv_trapo,a1            ;offset the location of table
        move.l  a1,sv_trapv(a6)         ;it's presently current and then whenever
        move.l  a1,jb_trapv(a0)         ;this job (or any offspring) is executing
        bra     L03A4                   ;return from trap err.ok

;Trap #1 D0=$0C MT.ALLOC - Allocate heap space (in user heap)

L071E   adda.l  trlv_a6(a5),a0          ;absolute pointer to free space
        jsr     L3104(pc)               ;mm_alloc, get d1.l heap allocation
        suba.l  trlv_a6(a5),a0          ;relative pointer to allocated area
        bra.s   L07A2                   ;return from trap

;Trap #1 D0=$0D MT.LNKFR - Release heap space

L072C   adda.l  trlv_a6(a5),a0          ;absolute pointer to new sapce
        adda.l  trlv_a6(a5),a1          ;absolute pointer to free space
        jsr     L3162(pc)               ;mm_lnkfr, link back in d1.l released heap
        bra.s   L07A2                   ;return from trap

;Trap #1 D0=$18 MT.ALCHP - Allocate common heap space

L073A   exg     d2,d1                   ;swap job ID with requested memory
        jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        move.l  d1,-(a7)                ;preserve valid job ID
        moveq   #hp_end,d1              ;heap header size
        add.l   d2,d1                   ;+ requested bytes
        jsr     L2FAE(pc)               ;mm_alchp, make allocation in common heap
        bne.s   L07BC                   ;no space, return from trap
        addq.w  #hp_drivr,a0            ;address of heap driver linkage is offset so
        move.l  #L075A-hp_close,(a0)+   ;that link for close routine points here
        move.l  (a7)+,(a0)+             ;hp_owner, supplied job ID is heap owner
        clr.l   (a0)+                   ;hp_rflag, no address to set on release
        bra.s   L0766                   ;return from trap err.ok

L075A   dc.l    L305E                   ;mm_rechp, release common heap

;Trap #1 D0=$19 MT.RECHP - Release common heap space

L075E   lea     -hp_end(a0),a0          ;start of heap header
        jsr     L305E(pc)               ;mm_rechp, release common heap
L0766   bra     L03A4                   ;return from trap err.ok

;Trap #1 D0=$0E MT.ALRES - Allocate resident procedure space
;Trap #1 D0=$0F MT.RERES - Release resident procedure space (collapse)

L076A   movea.l sv_respr(a6),a0            ;base of resident procedure area
        cmpa.l  sv_trnsp(a6),a0            ;transient program area empty ?
        bne.s   L07A0                      ;no, allocation cannot be made
        cmpi.b  #mt.reres,d0               ;was operation release whole area ?
        beq.s   L0786                      ;yes
        tst.l   d1                         ;number of bytes requested
        ble.s   L0766                      ;zero or negative, abort as done
        jsr     L2FFA(pc)                  ;get space in transient program area
        blt.s   L07BE                      ;error, return from trap
        bra.s   L0792                      ;set pointers and return

L0786   move.l  sv_ramt(a6),d1             ;very top of ram
        sub.l   sv_respr(a6),d1            ;size of resident procedure area
        jsr     L308C(pc)                  ;release whole area (should be L308E) [!]
L0792   move.l  sv_trnsp(a6),sv_respr(a6)  ;transient area is empty
        clr.l   sv_trnfr(a6)               ;no offset to free space
        bra     L03A4                      ;return from trap err.ok

L07A0   moveq   #err.nc,d0                 ;TRNSP not empty, cannot do RESPR action
L07A2   bra.s   L07BE                      ;return from trap

;Trap #1 D0=$16 MT.ALBAS - Allocate BASIC area

L07A4   jsr     L31B8(pc)               ;make space d1.l above BASIC
        bra.s   L07BE                   ;return from trap

;Trap #1 D0=$17 MT.REBAS - Release BASIC area

L07AA   move.l  sv_trnsp(a6),-(a7)      ;original base of TRNSP
        sub.l   d1,sv_trnsp(a6)         ;pretend it is TRNSP contracting
        jsr     L31C8(pc)               ;reclaim space d1.l above BASIC
        move.l  (a7)+,sv_trnsp(a6)      ;pretence over, it was BASIC all along
        bra.s   L07BE                   ;return from trap

L07BC   addq.w  #4,a7                   ;(discard job ID from mt.alchp)
L07BE   bra     L03A6                   ;return from trap

;Trap #1 D0=$10 MT.DMODE - Set or read display mode

L07C2   move.b  sv_mcsta(a6),d0         ;current display register setting
        tst.b   d1                      ;mode setting ?
        blt     L085E                   ;only to be read
        andi.b  #$F7,d0                 ;preserve all but mode bit
        andi.b  #$08,d1                 ;ignore all but mc..m256 (mode 4/8)
        or.b    d1,d0
        move.b  d0,sv_mcsta(a6)         ;keep copy of new mode setting
        move.b  d0,mc_stat              ;before writing to hardware register
        move.l  a6,-(a7)                ;preserve sv_base before using it to
        move.w  #8191,d0                ;clear whole of screen#0
L07E6   clr.l   -(a6)                   ;starting at bottom right of screen
        dbf     d0,L07E6                ;until whole display black paper
        movea.l (a7)+,a6                ;(+sv_base linked to top of hw_scrn)
        movea.l sv_chbas(a6),a4         ;base of channel table
L07F2   move.l  (a4)+,d0                ;get table entry
        movem.l d1-d6/a0-a6,-(a7)
        blt.s   L0852                   ;channel table entry unused
        movea.l d0,a0                   ;channel definition pointer
        cmpi.l  #L0D36,ch_drivr(a0)     ;CON/SCR driver ?
        bne.s   L0852                   ;no
        move.b  d1,-(a7)                ;preserve new mode setting
        move.w  sd_borwd(a0),-(a7)      ;preserve window border width
        moveq   #0,d2                   ;define zero width
        jsr     L1AFC(pc)               ;set window border
        lea     sd_pmask(a0),a1         ;pointer to colour masks
        lea     sd_pcolr(a0),a5         ;pointer to colour bytes
        moveq   #2,d0                   ;three parameters paper/strip/ink
L081C   move.b  (a5)+,d1                ;get colour byte
        jsr     L27D8(pc)               ;set colour mask from colour byte
        addq.w  #4,a1                   ;next colour mask
        dbf     d0,L081C                ;next parameter
        jsr     L1CAE(pc)               ;sd_clear, clear window
        move.b  (a5),d1                 ;sd_bcolr, border colour byte
        move.w  (a7)+,d2                ;original border width
        jsr     L1AF8(pc)               ;sd_bordr, set window border
        subq.w  #5,a5                   ;sd_cattr, chracter attributes
        andi.b  #$00,(a5)               ;all off
        move.l  #$0006000A,sd_xinc(a0)  ;cursor size 6 by 10 pixels
        tst.b   (a7)+                   ;new mode setting ?
        beq.s   L084E                   ;was mode 4
        bset    #sd..dbwd,(a5)+         ;else for mode 8 text is double width
        lsl.w   sd_xinc(a0)             ;and cursor is twice as wide
L084E   tst.b   (a5)                    ;sd_curf, cursor status ?
        sne     (a5)                    ;if not suppressed then make invisible
L0852   movem.l (a7)+,d1-d6/a0-a6
        cmpa.l  sv_chtop(a6),a4         ;top of channel table ?
        blt.s   L07F2                   ;no, get next entry
        bra.s   L0862                   ;finish by checking display setting

L085E   moveq   #$08,d1                 ;mask for mc..m256 (mode 4/8)
        and.b   d0,d1                   ;only return the mode bit
L0862   tst.b   d2                      ;monitor/TV request ?
        bge.s   L086A                   ;was to set the display type
        move.b  sv_tvmod(a6),d2         ;else return current setting
L086A   move.b  d2,sv_tvmod(a6)         ;action taken next mode change
        bra     L03A4                   ;return from trap err.ok

;Trap #1 D0=$11 MT.IPCOM - Send IPC command

L0872   movem.l d4/d6/a0-a1/a3,-(a7)
        jsr     L2C72(pc)               ;perform IPC dialogue
        movem.l (a7)+,d4/d6/a0-a1/a3
        bra     L03A4                   ;return from trap err.ok

;Trap #1 D0=$12 MT.BAUD - Set serial baud rate

L0882   movem.l d2/d6/a0-a2,-(a7)
        lea     L08D6(pc),a0                ;list of valid baud rates
        moveq   #7,d2                       ;number of available settings -1
L088C   cmp.w   (a0)+,d1                    ;supported baud rate ?
        beq.s   L089E                       ;yes
        dbf     d2,L088C                    ;continue through list, until
        moveq   #err.bp,d0                  ;unsupported baud rate
L0896   movem.l (a7)+,d2/d6/a0-a2
        bra     L03A6                       ;return from trap

L089E   move.l  #1200,d0                    ;the slower the baud rate below 1200
        divu.w  d1,d0                       ;the higher the timing value when
        addq.w  #1,d0                       ;(but a minimum of one)
        move.w  d0,sv_timov(a6)             ;waiting for serial output to complete
        ori.w   #$0700,sr                   ;interrupts off
        jsr     L2F6E(pc)                   ;assign IPC hardware registers
        lea     sv_tmode(a6),a2             ;transmit mode register
        andi.b  #$F8,(a2)                   ;mask out current baud setting
        or.b    d2,(a2)                     ;new setting to system variable
        move.b  (a2),pc_tctrl-pc_ipcwr(a1)  ;and then copy it to hardware port
        moveq   #baud_cmd,d0                ;IPC command set baud rate
        jsr     L2F7C(pc)                   ;send d0.b to IPC
        move.b  d2,d0                       ;baud rate setting
        jsr     L2F7C(pc)                   ;send d0.b to IPC
        andi.w  #$F8FF,sr                   ;interrupts on
        moveq   #err.ok,d0
        bra.s   L0896                       ;baud setting complete

L08D6   dc.w    75,300,600,1200,2400,4800,9600,19200  ;supported baud rates

;Trap #1 D0=$1A MT.LXINT - Link in external interrupt handler
;Trap #1 D0=$1C MT.LPOLL - Link in polled task
;Trap #1 D0=$1E MT.LSCHD - Link in scheduler task
;Trap #1 D0=$20 MT.LIOD  - Link in I/O driver
;Trap #1 D0=$22 MT.LDD   - Link in directory driver

L08E6   add.w   d0,d0                   ;operation code forms offset to
        lea     4(a6,d0.w),a1           ;get start of appropriate linked list
        jsr     L39DC(pc)               ;ut_link, link in routine at a0
        bra.s   L08FC                   ;operation complete

;Trap #1 D0=$1B MT.RXINT - Unlink external interrupt handler
;Trap #1 D0=$1D MT.RPOLL - Unlink polled task
;Trap #1 D0=$1F MT.RSCHD - Unlink scheduler task
;Trap #1 D0=$21 MT.RIOD  - Unlink I/O driver
;Trap #1 D0=$23 MT.RDD   - Unlink directory driver

L08F2   add.w   d0,d0                   ;operation code forms offset to
        lea     2(a6,d0.w),a1           ;get start of appropriate linked list
        jsr     L39E2(pc)               ;ut_unlnk, unlink routine at a0
L08FC   bra     L03A4                   ;return from trap err.ok

;Frame interrupt handler

L0900   addq.w  #1,sv_pollm(a6)         ;increment missed polls counter
        bvc.s   L090A                   ; < 32768 no overflow
        subq.w  #1,sv_pollm(a6)         ;ceiling is 32767
L090A   movem.l d0-d6/a0-a4,-(a7)
        moveq   #-sv_lpoll,d0           ;offset to linkage base
        moveq   #1,d3                   ;this is a poll call
        movea.l sv_plist(a6),a0         ;polled interrupt list
        jsr     L0A9E(pc)               ;call each routine in list
        movem.l (a7)+,d0-d6/a0-a4
        move.b  sv_pcint(a6),d7         ;copy of interrupt register
        ori.b   #pc.intrf,d7            ;clear frame interrupt
        move.b  d7,pc_intr              ;write to hardware register port
        btst    #5,trlv_sr(a7)          ;called in supervisor mode ?
        bne     L03B6                   ;yes, skip scheduler list (poll missed)
L0936   jsr     L09D4(pc)               ;save current job details
L093A   move.w  sv_pollm(a6),d3         ;accumulated/lost polls
        clr.w   sv_pollm(a6)            ;reset counter
        addq.w  #1,sv_rand(a6)          ;increment random number
        moveq   #-sv_lschd,d0           ;offset to linkage base
        movea.l sv_shlst(a6),a0         ;scheduler interrupt list
        jsr     L0A9E(pc)               ;call each routine in list
        jsr     L0A0C(pc)               ;look for candidate to be next job
        tst.l   d0                      ;table entry of next job ?
        blt.s   L093A                   ;there wasn't one, rerun scheduler
        move.l  d0,sv_jbpnt(a6)         ;this is the next active job
        jsr     L0A78(pc)               ;return to it (jsr redundant function)

;Trap #1 D0=$08 MT.SUSJB - Suspend a job

L0960   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        move.w  d3,jb_stat(a0)          ;suspension timeout to job status
        move.l  a1,jb_hold(a0)          ;zero this address on release
        moveq   #err.ok,d0              ;operation complete
        dc.w    $6000    *** bra L0936  ;return via scheduler
        dc.w    L0936-*

;Trap #1 D0=$09 MT.RELJB - Release a suspended job

L0972   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        tst.w   jb_stat(a0)             ;job status
        beq.s   L098A                   ;the job was not suspended
        clr.w   jb_stat(a0)             ;release suspension
        move.l  jb_hold(a0),d0          ;was there a location to clear on release ?
        beq.s   L098A                   ;no
        movea.l d0,a0
        sf      (a0)                    ;clear location, the job has been released
L098A   moveq   #err.ok,d0              ;operation complete
        dc.w    $6000    *** bra L0936  ;return via scheduler
        dc.w    L0936-*

;Trap #1 D0=$0B MT.PRIOR - Change job priority

L0990   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        move.b  d2,jb_princ(a0)         ;new job priority increment
        bne.s   L09CA                   ;which is an active setting, complete
        sf      jb_prior(a0)            ;else zero accumulated priority
        bra.s   L09CA                   ;operation complete

;Trap #1 D0=$0A MT.ACTIV - Activate a job

L09A0   jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        tst.b   jb_princ(a0)            ;job priority increment ?
        bne.s   L09D0                   ;job has already been activated
        move.b  d2,jb_princ(a0)         ;set priority increment
        move.l  jb_start(a0),jb_pc(a0)  ;set jobs pc to code start point
        tst.w   d3                      ;calling job to wait for completion ?
        beq.s   L09CA                   ;no, reschedule jobs
        st      jb_wflag(a0)            ;job is waiting on this activated job
        jsr     L0408(pc)               ;return current job ID in d0.l
        move.l  d0,jb_wjob(a0)          ;this is the waiting jobs ID
        move.w  #-2,jb_stat(a3)         ;owner is waiting on activated job
L09CA   moveq   #err.ok,d0              ;operation complete
L09CC   bra     L0936                   ;return via scheduler

L09D0   moveq   #err.nc,d0              ;error, job is already active
        bra.s   L09CC

;Save current job register set and if job was active reset its priority counter.

L09D4   move.l  a6,-(a7)                ;preserve sv_base
        movea.l sv_jbpnt(a6),a6         ;current job table entry
        movea.l (a6),a6                 ;current job header
        tst.b   jb_prior(a6)            ;was job active ?
        beq.s   L09E8                   ;no
        move.b  #1,jb_prior(a6)         ;else reset priority counter
L09E8   movem.l d0-d7/a0-a4,jb_d0(a6)   ;save jobs registers, but restore
        move.l  (a5)+,jb_d7(a6)         ;interrupt level d7
        move.l  (a5)+,jb_a5(a6)         ;    "       "   a5
        move.l  (a5)+,jb_a6(a6)         ;    "       "   a6
        move    usp,a0
        move.l  a0,jb_a7(a6)            ;    "       "   a7
        move.w  (a5)+,jb_sr(a6)         ;    "       "   sr
        move.l  (a5)+,jb_pc(a6)         ;    "       "   pc
        movea.l (a7)+,a6                ;restore sv_base
        rts

;Go through job table looking for next job to schedule.

L0A0C   moveq   #err.nj,d0              ;default is no next job found
        moveq   #0,d1                   ;highest job priority found
        movea.l sv_jbpnt(a6),a2         ;current job table entry
        movea.l a2,a4
        move.w  sv_jbmax(a6),d2         ;highest job number
        lsl.w   #2,d2                   ;form offset into job table
        movea.l sv_jbbas(a6),a3         ;base of job table
        adda.w  d2,a3                   ;highest job table entry
L0A22   addq.w  #4,a2                   ;point to next job in table
        cmpa.l  a3,a2                   ;highest entry ?
        ble.s   L0A2C                   ;no
        movea.l sv_jbbas(a6),a2         ;else start again at base of table
L0A2C   tst.b   (a2)                    ;is table entry in use ?
        blt.s   L0A72                   ;no, try next
        movea.l (a2),a0                 ;pointer to job header
        tst.b   jb_princ(a0)            ;is job active ?
        beq.s   L0A72                   ;no, try next
        tst.w   jb_stat(a0)             ;job status ?
        beq.s   L0A54                   ;job is active, possible candidate
        blt.s   L0A72                   ;suspended or waiting on another job
        sub.w   d3,jb_stat(a0)          ;else deduct lost polls (mt.susjb)
        bgt.s   L0A72                   ;timeout not expired, look at next
        clr.w   jb_stat(a0)             ;reactivate timed-out job
        move.l  jb_hold(a0),d2          ;location to clear on release ?
        beq.s   L0A54                   ;no
        movea.l d2,a1
        sf      (a1)                    ;clear location, job has been released
L0A54   move.b  jb_prior(a0),d2         ;priority counter
        beq.s   L0A64                   ;job has just been rescheduled
        add.b   jb_princ(a0),d2         ;add priority increment
        bcc.s   L0A66                   ;priority ceiling not reached
        st      d2                      ;else set at maximum 255
        bra.s   L0A66
L0A64   moveq   #1,d2                   ;rescheduled jobs start at 1
L0A66   move.b  d2,jb_prior(a0)         ;update priority counter
        cmp.b   d1,d2                   ;highest candidate ?
        bls.s   L0A72                   ;no
        move.l  a2,d0                   ;best candidate so far for next job
        move.b  d2,d1                   ;highest job priority found
L0A72   cmpa.l  a4,a2                   ;reached current/last job entry ?
        bne.s   L0A22                   ;no, look at next entry
        rts

;Restore to next job

L0A78   movea.l sv_jbpnt(a6),a0            ;job table entry of current job
        movea.l (a0),a0                    ;get pointer to job header
        adda.w  #22,a7                     ;discard pc/d7/a5-6/sr/pc
        move.l  jb_pc(a0),-(a7)            ;return where job stopped
        move.w  jb_sr(a0),-(a7)            ;with this status register
        move.l  jb_trapv(a0),sv_trapv(a6)  ;and this exception table
        movea.l jb_a7(a0),a1
        move    a1,usp                     ;job stack was here
        movem.l jb_d0(a0),d0-d7/a0-a6      ;registers as before
        rte                                ;reactivate job

;Go through list of interrupt handlers (external, poll or scheduler).

L0A9E   move.w  d0,-(a7)                ;linkage offset
L0AA0   movea.l a0,a3                   ;pointer to next link
        adda.w  (a7),a3                 ;form driver linkage base
        move.l  a0,-(a7)                ;preserve pointer to next link
        beq.s   L0ABC                   ;empty or list finished
        move.w  d3,-(a7)                ;polls missed
        andi.w  #$007F,d3               ;limit to < 128
        movea.l 4(a0),a0                ;get address of service routine
        jsr     (a0)                    ;call it
        move.w  (a7)+,d3                ;restore polls missed
        movea.l (a7)+,a0                ;pointer to next link
        movea.l (a0),a0                 ;get next link address
        bra.s   L0AA0                   ;next linkage
L0ABC   addq.l  #6,a7                   ;discard d3/a0
        rts

;Linkage block for SER driver (first in internal list)

L0AC0   dc.l    L0C88                   ;next in list (PIPE)
        dc.l    L0BB6                   ;Serial I/O
        dc.l    L0AD0                   ;   "   OPEN
        dc.l    L0B8E                   ;   "   CLOSE

;SER driver OPEN

L0AD0   subq.w  #8,a7                   ;make room
        movea.l a7,a3                   ;for 4 parameters
        jsr     L372C(pc)               ;io.name, decode device name
        bra.s   L0B20                   ;err.nf
        bra.s   L0B20                   ;err.bp
        bra.s   L0B28                   ;OK

        dc.w    3                       ;device name string
        dc.b    'SER',0
        dc.w    4                       ;four parameters
        dc.w    -1,1                    ;port number default 1
        dc.w    4                       ;possible parity
        dc.b    'OEMS'
        dc.w    2                       ;handshake or ignore
        dc.b    'IH'
        dc.w    3                       ;raw, ctrl Z or CR/LF (& ctrl Z)
        dc.b    'RZC',0

L0AFA   dc.b    ops1_cmd,0,0,0,0,0,1,0  ;IPC command open serial port 1
        dc.b    ops2_cmd,0,0,0,0,0,1,0  ; "     "    open serial port 2
L0B0A   dc.b    cls1_cmd,0,0,0,0,0,1,0  ; "     "    close serial port 1
        dc.b    cls2_cmd,0,0,0,0,0,1,0  ; "     "    close serial port 2

L0B1A   moveq   #err.nf,d0              ;unsupported port number
        bra.s   L0B20
L0B1E   moveq   #err.iu,d0              ;serial port in use
L0B20   addq.w  #8,a7                   ;discard parameters
        andi.w  #$F8FF,sr               ;interrupts on
        rts

;Set up a SER channel

L0B28   ori.w   #$0700,sr               ;interrupts off
        move.w  (a7),d4                 ;port number
        ble.s   L0B1A                   ;abort not found
        subq.w  #2,d4                   ;port 1 or 2
        bgt.s   L0B1A                   ;else abort not found
        lea     sv_ser1c(a6),a5         ;pointer to pointer to SER1 input queue
        lea     L0AFA(pc),a4            ;IPC command to open to SER1
        blt.s   L0B42                   ;port 1 requested
        addq.w  #4,a5                   ;else SER2 input queue
        addq.w  #8,a4                   ;and IPC command to open to SER2
L0B42   move.l  (a5),d0                 ;address of input queue
        beq.s   L0B56                   ;none, free for use
        movea.l d0,a0
        suba.w  #ser_rxq,a0             ;start of channel definition
        bclr    #7,ser_txq(a0)          ;transmit queue at EOF ?
        bne.s   L0B6A                   ;yes, port can be used
        bra.s   L0B1E                   ;else abort, port in use

L0B56   move.w  #ser_end,d1             ;size of SER channel definition
        jsr     L2FAE(pc)               ;mm_alchp
        bne.s   L0B20                   ;unsuccessful
        moveq   #ser_txql,d1            ;transmit queue length
        lea     ser_txq(a0),a2          ;location of queue header
        jsr     L37F4(pc)               ;io.qset, set up a queue
L0B6A   moveq   #ser_rxql,d1            ;receive queu length
        lea     ser_rxq(a0),a2          ;location of queue header
        jsr     L37F4(pc)               ;io.qset. set up a queue
        move.l  a2,(a5)                 ;set pointer to receive queue for port
        move.l  (a7),ser_chno(a0)       ;copy port number / parity
        move.l  4(a7),ser_txhs(a0)      ;and handshake / protocol
        subq.w  #1,ser_txhs(a0)         ;handshake I 0 or H >0
        subq.w  #2,ser_prot(a0)         ;protocol R <0, Z 0, or C >0
        bsr.s   L0BAA                   ;send IPC command to set up port
        moveq   #err.ok,d0
        bra.s   L0B20                   ;open complete

;SER driver CLOSE

L0B8E   lea     L0B0A(pc),a4            ;IPC command to close SER1
        btst    #1,ser_chno(a0)         ;channel number
        beq.s   L0B9C                   ;is 1
        addq.l  #8,a4                   ;else to close SER2
L0B9C   bsr.s   L0BAA                   ;send IPC command
        lea     ser_txq(a0),a2          ;transmit queue header
        jsr     L3888(pc)               ;io.qeof, put EOF marker in queue
        moveq   #err.ok,d0              ;close complete
        rts

L0BAA   move.l  a0,-(a7)                ;preserve channel definition address
        movea.l a4,a3                   ;location of appropriate IPC command
        jsr     L2C72(pc)               ;perform IPC dialogue
        movea.l (a7)+,a0
        rts

;SER driver I/O

L0BB6   jsr     L388C(pc)               ;io.serio
        dc.l    L0BC8                   ;pending test
        dc.l    L0C24                   ;fetch byte
        dc.l    L0BD0                   ;send byte
        rts

;Test for pending SER input

L0BC8   lea     ser_rxq(a0),a2          ;receive queue header
        jmp     L380A(pc)               ;io.qtest, test queue status

;Send a byte to SER channel

L0BD0   cmpi.b  #10,d1                  ;is byte LF character ?
        bne.s   L0BDE                   ;no
        tst.b   ser_prot+1(a0)          ;exchange LF for CR ?
        ble.s   L0BDE                   ;no
        moveq   #13,d1                  ;send CR character instead
L0BDE   move.w  ser_par(a0),d0          ;get parity parameter
        move.b  L0BEA(pc,d0.w),d0       ;and use as offset
        jmp     L0BEA(pc,d0.w)          ;to appropriate routine

;Serial send parity requirement

L0BEA   dc.b    L0BF4-L0BEA             ;none
        dc.b    L0C18-L0BEA             ;odd
        dc.b    L0C20-L0BEA             ;even
        dc.b    L0BF0-L0BEA             ;mark
        dc.b    L0C12-L0BEA             ;space
        dc.b    0

L0BF0   bset    #7,d1                   ;mark parity
L0BF4   lea     ser_txq(a0),a2          ;transmit queue header
        tst.b   sv_tran(a6)             ;translate table active ?
        beq.s   L0C0C                   ;no
        move.l  a3,-(a7)                ;preserve driver linkage
        movea.l MBFEE,a3                ;address of routine to
        jsr     (a3)                    ;put translated d1.b into queue
        movea.l (a7)+,a3                ;restore driver linkage
        bra.s   L0C74                   ;return
L0C0C   jsr     L3838(pc)               ;io.qin, place byte in queue
        bra.s   L0C74                   ;return (ok or queue full)
L0C12   bclr    #7,d1                   ;space parity
        bra.s   L0BF4
L0C18   bsr.s   L0C76                   ;generate even parity
        bchg    #7,d1                   ;but then make it odd
        bra.s   L0BF4
L0C20   bsr.s   L0C76                   ;generate even parity
        bra.s   L0BF4

;Fetch a byte from SER channel

L0C24   lea     ser_rxq(a0),a2          ;receive queue header
        jsr     L385E(pc)               ;io.qout, read a byte from queue
        bne.s   L0C74                   ;queue empty or at EOF
        move.w  ser_par(a0),d3          ;get parity requirement
        move.b  L0C3A(pc,d3.w),d3       ;and use as offset
        jmp     L0C3A(pc,d3.w)          ;to appropriate routine

;Serial fetch parity requirement

L0C3A   dc.b    L0C54-L0C3A             ;none
        dc.b    L0C46-L0C3A             ;odd
        dc.b    L0C4A-L0C3A             ;even
        dc.b    L0C40-L0C3A             ;mark
        dc.b    L0C4C-L0C3A             ;space
        dc.b    0

L0C40   bchg    #7,d1                   ;invert mark parity to space
        bra.s   L0C4C
L0C46   bchg    #7,d1                   ;invert odd parity to even
L0C4A   bsr.s   L0C76                   ;generate even parity
L0C4C   btst    #7,d1                   ;check parity
        beq.s   L0C54                   ;parity ok
        moveq   #err.te,d0              ;else tranmission error
L0C54   tst.b   sv_tran(a6)             ;translate table active ?
        beq.s   L0C66                   ;no
        move.l  a3,-(a7)                ;preserve driver linkage
        movea.l MBFEA,a3                ;address of routine to
        jsr     (a3)                    ;return translation of d1.b
        movea.l (a7)+,a3                ;restore driver linkage
L0C66   cmpi.b  #13,d1                  ;is byte a CR character ?
        bne.s   L0C74                   ;no
        tst.b   ser_prot+1(a0)          ;exchange CR for LF ?
        ble.s   L0C74                   ;no
        moveq   #10,d1                  ;return LF character instead
L0C74   rts

;Generate even parity in d1.b

L0C76   moveq   #6,d3                   ;seven bits to check
        move.b  d1,d4                   ;bit 7 in d4 initially (b6-0 ignored)
L0C7A   ror.b   #1,d1                   ;next bit to bit 7
        eor.b   d1,d4                   ;accumulate bit parity in bit 7
        dbf     d3,L0C7A                ;until b7-0 all eor'd together
        roxl.b  #1,d4                   ;parity bit to eXtend
        roxr.b  #1,d1                   ;then to data byte top bit
        rts

;Linkage block for PIPE device driver (second in internal list)

L0C88   dc.l    L0D36                   ;next in list (CON)
        dc.l    L37CC                   ;Pipe I/O uses io.serq
        dc.l    L0C98                   ;  "  OPEN
        dc.l    L0D0A                   ;  "  CLOSE

;PIPE driver OPEN

L0C98   subq.w  #2,a7                   ;make room
        movea.l a7,a3                   ;for 1 parameter
        jsr     L372C(pc)               ;io.name, decode device name
        bra.s   L0D00                   ;err.nf
        bra.s   L0D00                   ;err.bp
        bra.s   L0CB2                   ;OK

        dc.w    4                       ;device name string
        dc.b    'PIPE'
        dc.w    1                       ;one parameter
        dc.b    ' _'
        dc.w    0                       ;default value

;Set up a PIPE channel

L0CB2   move.w  (a7),d1                 ;get size of output pipe
        beq.s   L0CD2                   ;it is an input pipe
        addi.w  #49,d1                  ;add overhead
        jsr     L2FAE(pc)               ;mm_alchp
        bne.s   L0D00                   ;no memory for channel definition
        move.w  (a7),d1                 ;original request
        addq.w  #1,d1                   ;for queue lost byte
        lea     ch_qend(a0),a2          ;skip channel definitions
        jsr     L37F4(pc)               ;io.qset, set up a queu
        move.l  a2,ch_qout(a0)          ;set pointer to output queue
        bra.s   L0CFE                   ;operation complete

;Link new input pipe to previously opened output pipe.

L0CD2   movea.w d3,a2                   ;channel number of output pipe
        adda.l  a2,a2                   ;form offset to
        adda.l  a2,a2
        adda.l  sv_chbas(a6),a2         ;base of channel table
        movea.l (a2),a2                 ;channel definition of output pipe
        cmpi.l  #L0C88,ch_drivr(a2)     ;PIPE driver ?
        bne.s   L0D04                   ;incorrect driver
        move.l  a2,-(a7)
        moveq   #ch_qend,d1             ;minimum definition for input
        jsr     L2FAE(pc)               ;mm_alchp
        movea.l (a7)+,a2
        bne.s   L0D00                   ;no memory for channel definition
        lea     ch_qend(a2),a2          ;output pipe queue header
        move.l  a0,(a2)                 ;set q_nextq as input channel definition
        move.l  a2,ch_qin(a0)           ;set pointer to input queue for input pipe
L0CFE   moveq   #err.ok,d0
L0D00   addq.w  #2,a7                   ;discard parameter
        rts

L0D04   moveq   #err.bp,d0              ;output ID given not pipe
        addq.w  #2,a7                   ;discard parameter
        rts

;PIPE driver CLOSE

L0D0A   tst.l   ch_qin(a0)              ;is there an input queue ?
        bne.s   L0D1C                   ;yes, so it must be an input pipe
        lea     ch_qend(a0),a2          ;q_nextq, output queue header
        tst.l   (a2)                    ;is the input closed ?
        beq.s   L0D32                   ;yes, reclaim channel definition
        jmp     L3888(pc)               ;io.qeof, put EOF in queue

;Close input side of pipe

L0D1C   move.l  ch_qin(a0),-(a7)        ;preserve queue header
        bsr.s   L0D32                   ;reclaim channel definition
        movea.l (a7)+,a2
        lea     -ch_qend(a2),a0         ;base of output channel definition
        tst.b   (a2)                    ;at EOF ?
        blt.s   L0D32                   ;yes, reclaim channel definition & return
        clr.l   (a2)                    ;input no longer exists
        moveq   #err.ok,d0              ;operation complete
        rts

L0D32   jmp     L305E(pc)               ;mm_rechp

;Linkage block for CON/SCR device driver (third in internal list)

L0D36   dc.l    L1104                   ;next in list (NET)
        dc.l    L0E76                   ;Console I/O
        dc.l    L0D46                   ;   "    OPEN
        dc.l    L0E3A                   ;   "    CLOSE

;CON driver OPEN

L0D46   suba.w  #10,a7                  ;make room
        movea.l a7,a3                   ;for 5 parameters
        jsr     L372C(pc)               ;io.name
        bra.s   L0D9E                   ;not found, try SCR
        bra.s   L0DC8                   ;err.bp
        bra.s   L0D72                   ;OK

        dc.w    3                       ;device name string
        dc.b    'CON',0
        dc.w    5                       ;five parameters
        dc.b    ' _'
        dc.w    448                     ;default width
        dc.b    ' X'
        dc.w    180                     ;default height
        dc.b    ' A'
        dc.w    32                      ;default x origin
        dc.b    ' X'
        dc.w    16                      ;default y origin
        dc.b    ' _'
        dc.w    128                     ;default key buffer

;Set up a CON channel

L0D72   moveq   #sd_kbd+q_queue+2,d1    ;length of minimum CON definition
        add.w   8(a7),d1                ;add length of keyboard buffer
        bsr.s   L0DCE                   ;set up channel definition
        bne.s   L0DC8                   ;return any error
        lea     sd_kbd(a0),a2           ;keyboard queue header
        subi.w  #sd_kbd+q_queue,d1      ;length of queue
        jsr     L37F4(pc)               ;io.qset, set up a queue
        movea.l sv_keyq(a6),a3          ;current keyboard queue
        move.l  a3,d3                   ;is there one ?
        bne.s   L0D98                   ;yes
        move.l  a2,(a2)                 ;else q_qnext points to itself
        move.l  a2,sv_keyq(a6)          ;set as current keyboard queue
        bra.s   L0DC6                   ;operation complete

;Link new keyboard queue into current list.

L0D98   move.l  (a3),(a2)               ;next queue is displaced one place
        move.l  a2,(a3)                 ;this queue is next in list
        bra.s   L0DC6                   ;operation complete

;SCR driver OPEN

L0D9E   jsr     L372C(pc)               ;io.name
        bra.s   L0DC8                   ;err.nf
        bra.s   L0DC8                   ;err.bp
        bra.s   L0DC0                   ;OK

        dc.w    3                       ;device name string
        dc.b    'SCR',0
        dc.w    4                       ;four parameters
        dc.b    ' _'
        dc.w    448                     ;default width
        dc.b    ' X'
        dc.w    180                     ;default height
        dc.b    ' A'
        dc.w    32                      ;default x origin
        dc.b    ' X'
        dc.w    16                      ;default y origin

;Set up a SCR channel

L0DC0   moveq   #sd_end,d1              ;length of SCR definition
        bsr.s   L0DCE                   ;set up channel definition
        bra.s   L0DC8                   ;return any error

L0DC6   moveq   #err.ok,d0              ;successful open
L0DC8   adda.w  #10,a7                  ;discard parameters
        rts

;Common CON/SCR set up channel definition

L0DCE   jsr     L2FAE(pc)               ;mm_alchp
        bne.s   L0E38                   ;no memory for channel definition
        move.w  d1,-(a7)                ;preserve bytes allocated
        lea     sd_xinc(a0),a2          ;initial cursor size
        move.l  #$0006000A,(a2)+        ;is 6 by 10 pixels
        move.l  #MAD9A,(a2)+            ;sd_font, table 1 of character fonts
        move.l  #MB106,(a2)+            ;         table 2 of character fonts
        move.l  #hw_scrn,(a2)+          ;sd_scrb, screen#0 base is fixed
        move.w  #hw_scrll,sd_linel(a0)  ;and so is line length
        lea     sd_imask(a0),a1         ;ink colour mask
        moveq   #4,d1                   ;green ink (strip/paper default to black)
        move.b  d1,sd_icolr(a0)         ;set colour byte
        jsr     L27D8(pc)               ;set ink colour mask
        move.l  #$08076400,sd_scal(a0)  ;fp[100] scale default
        btst    #mc..m256,sv_mcsta(a6)  ;current mode setting ?
        beq.s   L0E20                   ;is mode 4
        addq.w  #6,sd_xinc(a0)          ;else mode 8 cursor is twice as wide
        bset    #sd..dbwd,sd_cattr(a0)  ;and so are characters
L0E20   moveq   #0,d2                   ;initial border has no width
        lea     6(a7),a1                ;here are the parameters to
        jsr     L1AB6(pc)               ;sd.wdef, redfine window
        move.w  (a7)+,d1                ;restore bytes allocated
        tst.l   d0                      ;any error from sd.wdef ?
        beq.s   L0E38                   ;no, return
        move.l  d0,-(a7)                ;else preserve error
        jsr     L305E(pc)               ;mm_rechp, reclaim channel definition
        move.l  (a7)+,d0                ;restore error setting CCR
L0E38   rts

;CON/SCR driver CLOSE

L0E3A   lea     sd_kbd(a0),a3           ;channel keyboard queue
        tst.l   (a3)                    ;is there a queue ?
        beq.s   L0E70                   ;no, so it must be a SCR channel
        movea.l (a3),a2                 ;q_qnext, next queue in keyboard list
        cmpa.l  a2,a3                   ;is ours the only keyboard queue ?
        bne.s   L0E4E                   ;no, so ours will have to be unlinked
        clr.l   sv_keyq(a6)             ;else clear pointer to keyboard queue list
        bra.s   L0E70                   ;clear up and return

L0E4E   cmpa.l  sv_keyq(a6),a3          ;is ours the current keyboard queue ?
        bne.s   L0E66                   ;no, just unlink it then
L0E54   tst.b   sd_curf-sd_kbd(a2)      ;else check cursor status of next queue ?
        bne.s   L0E62                   ;not suppressed
        cmpa.l  (a2),a3                 ;does next keyboard queue point to ours ?
        beq.s   L0E62                   ;yes, unlink ours
        movea.l (a2),a2                 ;else get link to next keyboard queue
        bra.s   L0E54                   ;and keep looking until ours is next in link

L0E62   move.l  a2,sv_keyq(a6)          ;next keyboard queue becomes current
L0E66   movea.l a3,a2                   ;starting with ours
L0E68   movea.l (a2),a2                 ;get pointer to next in link
        cmpa.l  (a2),a3                 ;is it ours ?
        bne.s   L0E68                   ;no, try next
        move.l  (a3),(a2)               ;unlink ours
L0E70   jsr     L305E(pc)               ;mm_rechp, reclaim channel definition
        rts

;CON/SCR driver I/O

L0E76   tst.b   sv_scrst(a6)            ;screen status ?
        beq.s   L0E80                   ;is active
        moveq   #err.nc,d0              ;else operation frozen out for now
        rts

L0E80   cmpi.b  #io.sstrg,d0            ;IO operation ?
        bhi.s   L0EFC                   ;no, SD or FS operation
        moveq   #-1,d7                  ;default
        moveq   #0,d6                   ;default
        move.w  d2,d5                   ;IOSS parameter
        move.l  d1,d4                   ;  "     "
        movea.l a1,a4                   ;  "     "
        lea     sd_kbd(a0),a5           ;keyboard queue for channel
        tst.l   d3                      ;entry condition ?
        blt.s   L0EB0                   ;previous call was incomplete
        cmpi.b  #io.edlin,d0            ;IO operation type ?
        bhi.s   L0EB0                   ;is send byte/s
        tst.l   (a5)                    ;else fetch, so is there a keyboard queue ?
        beq.s   L0EEC                   ;no, SCR channel, so abort with error
        movea.l sv_keyq(a6),a2          ;current keyboard queue
        tst.b   sd_curf-sd_kbd(a2)      ;cursor status ?
        bne.s   L0EB0                   ;not suppressed
        move.l  a5,sv_keyq(a6)          ;else make our keyboard current for input
L0EB0   move.b  L0EB8(pc,d0.w),d0       ;use operation code as offset
        jmp     L0EB8(pc,d0.w)          ;to call appropriate routine

L0EB8   dc.b    L0EC0-L0EB8             ;$00 io.pend
        dc.b    L0EC4-L0EB8             ;$01 io.fbyte
        dc.b    L0F54-L0EB8             ;$02 io.fline
        dc.b    L0ECA-L0EB8             ;$03 io.fstrg
        dc.b    L0F5A-L0EB8             ;$04 io.edlin
        dc.b    L0F00-L0EB8             ;$05 io.sbyte
        dc.b    L0EEC-L0EB8             ;$06 (undefined)
        dc.b    L0EDE-L0EB8             ;$07 io.sstrg

;CON/SCR Trap #3 D0=$00 IO.PEND - Test pending input

L0EC0   jmp     L380A(pc)               ;io.qtest, test queue status (sv_keyq) [!]

;CON/SCR Trap #3 D0=$01 IO.FBYTE - Fetch a byte

L0EC4   movea.l a5,a2                   ;keyboard queue of channel
        jmp     L385E(pc)               ;io.qout, read a byte from queue

;CON/SCR Trap #3 D0=$03 IO.FSTRG - Fetch a string of bytes

L0ECA   moveq   #err.ok,d0
L0ECC   cmp.w   d4,d5                   ;fetch complete ?
        bls.s   L0EEE                   ;yes
        movea.l a5,a2                   ;keyboard queue of channel
        jsr     L385E(pc)               ;io.qout, read a byte from queue
        blt.s   L0EEE                   ;abort if error
        move.b  d1,(a4)+                ;byte to buffer
        addq.w  #1,d4                   ;increment byte counter
        bra.s   L0ECC                   ;continue until done

;CON/SCR Trap #3 D0=$07 IO.SSTRG - Send a string of bytes

L0EDE   moveq   #err.ok,d0
        cmp.w   d4,d5                   ;send complete ?
        bls.s   L0EEE                   ;yes
        move.b  (a4)+,d1                ;get byte from buffer
        bsr.s   L0F00                   ;io.sbyte send byte to window
        addq.w  #1,d4                   ;increment byte counter
        bra.s   L0EDE                   ;continue until done

L0EEC   moveq   #err.bp,d0              ;invalid operation
L0EEE   move.w  d4,d1                   ;bytes sent/fetched
        movea.l a4,a1                   ;update buffer pointer
        bclr    #sd..gchr,sd_cattr(a0)  ;reset graphics positioned characters
        rts

L0EFA   moveq   #io.sbyte,d0            ;send a byte to window
L0EFC   jmp     L1992(pc)               ;perform screen operation

;CON/SCR Trap #3 D0=$05 IO.SBYTE - Send a byte

L0F00   moveq   #err.ok,d0              ;[!]
        cmpi.b  #10,d1                  ;LF character ?
        beq.s   L0F36                   ;yes
        tst.b   sd_nlsta(a0)            ;new line status ?
        beq.s   L0F14                   ;none pending
        move.b  d1,-(a7)                ;preserve byte to send
        bsr.s   L0F46                   ;perform newline
        move.b  (a7)+,d1
L0F14   bsr.s   L0EFA                   ;send byte to window
        beq.s   L0F44                   ;successful operation
        move.w  sd_xinc(a0),d0          ;else err.or so get cursor width
        add.w   d0,sd_xpos(a0)          ;increment cursor x position
        btst    #sd..gchr,sd_cattr(a0)  ;graphics positioned characters ?
        bne.s   L0F42                   ;yes, return complete
        move.b  d0,sd_nlsta(a0)         ;(sd_xinc) newline implicit
        tst.b   sd_curf(a0)             ;cursor status ?
        beq.s   L0F42                   ;suppressed, return complete
        bsr.s   L0F46                   ;else perform newline
        bra.s   L0F42

L0F36   tst.b   sd_nlsta(a0)            ;new line status ?
        bge.s   L0F3E                   ;implicit or none pending
        bsr.s   L0F46                   ;perform newline
L0F3E   st      sd_nlsta(a0)            ;set pending newline
L0F42   moveq   #err.ok,d0              ;return operation complete
L0F44   rts

L0F46   tst.b   sd_curf(a0)             ;cursor status ?
        ble.s   L0F50                   ;invisible or suppressed
        jsr     L1BA2(pc)               ;invert the visible cursor
L0F50   jmp     L1BF8(pc)               ;do the pending newline

;CON/SCR Trap #3 D0=$02 IO.FLINE - Fetch a line

L0F54   tst.l   d3                      ;entry condition ?
        beq.s   L0F6C                   ;first attempt at operation (d4=d1=0, d6=0)
        bra.s   L0F5E                   ;else previous call was incomplete (d4=d1=?)

;CON/SCR Trap #3 D0=$04 IO.EDLIN - Edit a line

L0F5A   moveq   #-1,d4                  ;flag operation is io.edlin
        move.w  d1,d4                   ;length of line to edit
L0F5E   swap    d1                      ;upper word is/was
        move.w  d1,d6                   ;cursor position within line
        suba.w  d4,a4                   ;backtrack to start of line then
        adda.w  d6,a4                   ;point to cursor position
        bne.s   L0F84                   ;cursor was not at start of line
        tst.l   d3                      ;entry condition ?
        blt.s   L0F84                   ;previous call was incomplete

;Print line from cursor to end of line (or within window limits).

L0F6C   tst.b   sd_curf(a0)             ;cursor status ?
        ble.s   L0F76                   ;invisible or suppressed
        jsr     L1BA2(pc)               ;invert the visible cursor
L0F76   jsr     L1BF2(pc)               ;sd.donl, do any pending newline
        bsr     L10DC                   ;print line from cursor position to end
        move.w  d4,d6                   ;length of line less any characters
        sub.w   d3,d6                   ;not printed is new cursor position
        suba.w  d3,a4                   ;pointer to end of line

;Deal with input key/character for line being edited/fetched.

L0F84   move.b  sd_curf(a0),-(a7)       ;preserve entry cursor status
L0F88   movea.l a5,a2                   ;channel keyboard queue
        jsr     L385E(pc)               ;io.qout, read a byte from queue
        blt     L1018                   ;queue empty or at EOF
        tst.b   sd_curf(a0)             ;current cursor status ?
        ble.s   L0FA0                   ;invisible or suppressed
        move.b  d1,-(a7)                ;preserve byte fetched
        jsr     L1BA2(pc)               ;invert the visible cursor
        move.b  (a7)+,d1                ;was byte fetched a
L0FA0   cmpi.b  #10,d1                  ;LF character ?
        beq.s   L1002                   ;yes, a termination key
        cmpi.b  #$1F,d1                 ;unprintable character ?
        bls.s   L0F88                   ;yes, ignore and get another
        cmpi.b  #$BF,d1                 ;action key ?
        bhi.s   L0FDA                   ;yes, respond to it accordingly
        move.w  d4,d0                   ;line length
        sub.w   d6,d0                   ;less cursor position
        bra.s   L0FBE                   ;is characters to shift
L0FB8   move.b  0(a4,d0.w),1(a4,d0.w)   ;shift character forward one place
L0FBE   dbf     d0,L0FB8                ;until end of line
        addq.w  #1,d6                   ;move cursor position one place
        addq.w  #1,d4                   ;increment line length
        move.b  d1,(a4)+                ;insert new character into buffer
        moveq   #-1,d7                  ;flag scroll window if required
        bsr     L10AC                   ;print character in window
        bsr     L10CE                   ;print rest of (shifted) line
        cmp.w   d4,d5                   ;within buffer length ?
        bhi.s   L0F88                   ;yes, get next key/character
        moveq   #err.bo,d0              ;else buffer overflow
        bra.s   L1022                   ;return from operation

;Action key pressed.

L0FDA   tst.l   d4                      ;called from io.fline ?
        bge.s   L0FEA                   ;yes, so ignore up/down key as terminator
        cmpi.b  #$D0,d1                 ;up arrow key ?
        beq.s   L1002                   ;yes, a io.edlin termination key
        cmpi.b  #$D8,d1                 ;down arrow key ?
        beq.s   L1002                   ;yes, a io.edlin termination key
L0FEA   subi.b  #$C0,d1                 ;left arrow key ?
        beq.s   L1030                   ;yes, move cursor left
        subq.b  #2,d1                   ;CTRL + left arrow ?
        beq     L107C                   ;yes, delete character left
        subq.b  #6,d1                   ;right arrow key ?
        beq.s   L1038                   ;yes, move cursor right
        subq.b  #2,d1                   ;CTRL + right arrow key ?
        beq     L1080                   ;yes, delete character right
L1000   bra.s   L0F88                   ;else ignore rest, get next key/character

;Termination key pressed (LF or up/down cursor key).

L1002   move.b  d1,-(a7)                ;preserve termination key
        moveq   #-1,d7                  ;flag scroll window if required
        bsr     L10E0                   ;print line from cursor position to end
        move.b  (a7)+,(a4)+             ;put termination key at end of line
        addq.w  #1,d4                   ;and add it to line length
        jsr     L1B94(pc)               ;sd.curs, disable cursor
        bsr     L0F36                   ;set newline is now pending
        bra.s   L102A                   ;return from operation

;Return edit/fetch line operation complete or incomplete.

L1018   tst.b   (a7)                    ;entry cursor status ?
        blt.s   L1022                   ;was invisible (return err.nc or err.ef)
        jsr     L1B86(pc)               ;sd.cure, enable the cursor (make visible)
        moveq   #err.nc,d0              ;operation incomplete this call
L1022   suba.w  d6,a4                   ;back to start of line
        adda.w  d4,a4                   ;then to very end (pointer returned value)
        move.w  d6,d1                   ;current cursor position
        swap    d1                      ;for subsequent call if this incomplete
L102A   addq.w  #2,a7                   ;discard entry cursor status
        bra     L0EEE                   ;return parameters from operation

;Cursor left key pressed

L1030   bsr.s   L1054                   ;cursor to next character left in line
        beq.s   L1000                   ;successful, get next key/character
L1034   addq.w  #1,d6                   ;else correct invalid cursor movement
        bra.s   L1000                   ;get next key/character

;Cursor right key pressed.

L1038   addq.w  #1,d6                   ;cursor to next character right in line
        cmp.w   d4,d6                   ;too far right exceeds line length ?
        bhi.s   L1050                   ;yes, redress and return
        addq.w  #1,a4                   ;update pointer to cursor within line
        jsr     L1C48(pc)               ;sd.ncol, cursor to next column
        beq.s   L1000                   ;successful, get next key/character
        jsr     L1BF8(pc)               ;else, do the pending newline (forced)
        bsr     L10CE                   ;print rest of line
        bra.s   L1000                   ;get next key/character
L1050   subq.w  #1,d6                   ;correct invalid cursor movement
L1052   bra.s   L1000                   ;get next key/character

L1054   subq.w  #1,d6                   ;cursor position moved left one place
        blt.s   L107A                   ;if too far left then correct and return
        jsr     L1C3E(pc)               ;sd.pcol, cursor to previous column
        beq.s   L1078                   ;successful, else err.or
        jsr     L1C56(pc)               ;sd.prow, cursor to previous row
        bne.s   L10DA                   ;if err.or, ignore redress and return
        move.w  sd_xsize(a0),d0         ;window width
        divu.w  sd_xinc(a0),d0          ;divided by cursor width
        subq.w  #1,d0                   ;less one column
        mulu.w  sd_xinc(a0),d0          ;multiplied by cursor width
        move.w  d0,sd_xpos(a0)          ;is new cursor x position
        moveq   #err.ok,d0
L1078   subq.w  #1,a4                   ;update pointer to cursor within line
L107A   bra.s   L10DA                   ;return

;CTRL & cursor left keys pressed.

L107C   bsr.s   L1054                   ;cursor to next character left in line
        bne.s   L1034                   ;if unsuccessful redress and return

;CTRL & cursor right keys pressed.

L1080   cmp.w   d6,d4                   ;cursor at end of line ?
        beq.s   L1052                   ;yes, get next key/character
        subq.w  #1,d4                   ;line length is reduced accordingly
        move.w  d4,d0                   ;new line length
        sub.w   d6,d0                   ;less new cursor position
        move.w  d0,d1                   ;is counter of
        bra.s   L1092                   ;characters to shift
L108E   move.b  1(a4),(a4)+             ;shift character back one place
L1092   dbf     d0,L108E                ;from cursor to end of line
        suba.w  d1,a4                   ;pointer to cursor position (deletion)
        move.l  sd_xpos(a0),-(a7)       ;preserve cursor x/y position
        moveq   #0,d7                   ;flag do not scroll window
        bsr.s   L10E0                   ;print from cursor to end of line
        bne.s   L10A6                   ;if err.or then skip
        moveq   #' ',d1                 ;pad out character displaced
        bsr.s   L10AC                   ;print character to window
L10A6   move.l  (a7)+,d7
        bsr.s   L10D0                   ;restore window cursor position
        bra.s   L1052                   ;get next key/character

;Print character in window, deal with newline depending on circumstance.

L10AC   bsr     L0EFA                   ;send d1.b to window
        beq.s   L10DA                   ;successful, else err.or so
        jsr     L1C32(pc)               ;sd.nl, try to do a newline within window
        beq.s   L10DA                   ;successful, else err.or so
        tst.w   d7                      ;scroll window if required ?
        blt.s   L10C8                   ;yes, so force newline
        sub.w   sd_yinc(a0),d7          ;was start of text on first line of window ?
        bge.s   L10C8                   ;no, so ok to force newline
        add.w   sd_yinc(a0),d7          ;else redress to original position
        bra.s   L1100                   ;and return the error

L10C8   jsr     L1BF8(pc)               ;force newline by scrolling window
        bra.s   L10D8                   ;return ok

L10CE   bsr.s   L10DC                   ;print rest of line from cursor
L10D0   suba.w  d4,a4                   ;back to start of line
        adda.w  d6,a4                   ;pointer to cursor position
        move.l  d7,sd_xpos(a0)          ;update cursor x/y position
L10D8   moveq   #err.ok,d0
L10DA   rts

;Print from cursor to end of line. Return d3.w as out-of-window characters.

L10DC   move.l  sd_xpos(a0),d7          ;window cursor x/y position
L10E0   moveq   #err.ok,d0              ;(in case no text to print)
        move.w  d4,d3                   ;length of line
        sub.w   d6,d3                   ;less cursor position
        bra.s   L10F4                   ;is number of characters to print
L10E8   move.b  (a4)+,d1                ;get character
        move.w  d3,-(a7)                ;preserve counter
        bsr.s   L10AC                   ;print character in window
        move.w  (a7)+,d3
        tst.l   d0                      ;within window limits ?
        bne.s   L10FC                   ;no, report result
L10F4   dbf     d3,L10E8                ;next character
        moveq   #0,d3                   ;no characters missed
        rts

L10FC   adda.w  d3,a4                   ;pointer to end of line, skipping
        addq.w  #1,d3                   ;number of missed characters
L1100   tst.l   d0                      ;set CCR on error
        rts

;Linkage block for NET device driver (fourth and last in internal link)

L1104   dc.l    0                       ;last in list
        dc.l    L1186                   ;Network I/O
        dc.l    L1114                   ;   "    OPEN
        dc.l    L1158                   ;   "    CLOSE

;NET driver OPEN

L1114   subq.w  #4,a7                   ;make room
        movea.l a7,a3                   ;for 2 parameters
        jsr     L372C(pc)               ;io.name
        bra.s   L1154                   ;err.nf
        bra.s   L1154                   ;err.bp
        bra.s   L1132                   ;OK

        dc.w    3                       ;device name string
        dc.b    'NET',0
        dc.w    2                       ;two parameters
        dc.w    2
        dc.b    'OI'                    ;direction
        dc.b    ' _'
        dc.w    0                       ;default station number

;Set up a NET channel

L1132   move.w  #net_end,d1                     ;length of NET definition
        jsr     L2FAE(pc)                       ;mm_alchp
        bne.s   L1154                           ;no memory for channel definition
        move.b  3(a7),net_dest(a0)              ;destination station number
        move.b  sv_netnr(a6),net_self(a0)       ;station number which opened channel
        move.b  1(a7),net_type(a0)              ;packet type is used for
        subq.b  #2,net_type(a0)                 ;out=-1, in=0 (data)
        moveq   #err.ok,d0                      ;open complete
L1154   addq.w  #4,a7                           ;discard parameters
        rts

;NET driver CLOSE

L1158   tst.b   net_type(a0)                    ;channel direction ?
        bge.s   L117E                           ;was input, clear up
        move.b  #1,net_type(a0)                 ;set type to EOF
        move.b  net_rpnt(a0),net_nbyt(a0)       ;remaining bytes to send
L116A   tst.b   sv_mdrun(a6)                    ;microdrive running ?
        bne.s   L116A                           ;yes, keep waiting
        move.w  #1400,d4                        ;attempts to
L1174   dc.w    $4EB9,0,L5538    *** jsr L5538  ;send last block to network
        dbeq    d4,L1174                        ;until complete or attempts expired
L117E   jmp     L305E(pc)                       ;mm_rechp, reclaim channel definition

L1182   moveq   #err.bp,d0                      ;invalid operation requested
        rts

;NET driver I/O

L1186   jsr     L388C(pc)                       ;io.serio
        dc.l    L1198                           ;pending test
        dc.l    L11C4                           ;fetch byte
        dc.l    L11D0                           ;send byte
        rts

;Test for pending NET input

L1198   move.b  net_type(a0),d0                 ;channel direction ?
        blt.s   L1182                           ;was output, abort
        moveq   #0,d2
        move.b  net_rpnt(a0),d2                 ;data block running pointer
        move.b  net_data(a0,d2.w),d1            ;get corressponding data byte
        sub.b   net_nbyt(a0),d2                 ;number of bytes in data block
        bcs.s   L11CC                           ;is greater than those retrieved
        tst.b   d0                              ;packet type ?
        beq.s   L11B6                           ;was intermediate data block
        moveq   #err.ef,d0                      ;else was EOF block
        rts

L11B6   sf      net_rpnt(a0)                    ;reset block running pointer
        dc.w    $4EB9,0,L54A2    *** jsr L54A2  ;get a block from network
        beq.s   L1198                           ;successful, perform check again
        rts                                     ;else return error

;Fetch a byte from NET channel

L11C4   bsr.s   L1198                           ;test/get pending input
        bne.s   L11CE                           ;nothing, return error
        addq.b  #1,net_rpnt(a0)                 ;byte was taken and is returned
L11CC   moveq   #err.ok,d0                      ;successfully
L11CE   rts

;Send a byte to NET channel

L11D0   tst.b   net_type(a0)                    ;channel direction ?
        bge.s   L1182                           ;was input, abort
        moveq   #1,d2                           ;one byte to send
        add.b   net_rpnt(a0),d2                 ;location in block
        bcc.s   L11F8                           ;is free for storage
        move.b  d1,-(a7)                        ;preserve data byte
        move.w  #$00FF,net_type(a0)             ;data block of 255 bytes
        dc.w    $4EB9,0,L5538    *** jsr L5538  ;send block a block to network
        move.b  (a7)+,d1                        ;get data byte back
        st      net_type(a0)                    ;restore output type
        tst.l   d0                              ;error sending block ?
        bne.s   L11CE                           ;yes, return with error
        moveq   #1,d2                           ;new block has one byte in it
L11F8   move.b  d1,net_hchk(a0,d2.w)            ;put byte in data block
        move.b  d2,net_rpnt(a0)                 ;update running pointer
        bra.s   L11CC                           ;operation complete

;First linkage in list of internal scheduler tasks

L1202   dc.l    L2D00                   ;next in list (transmit)
        dc.l    L120A                   ;flash cursor

;Flash cursor in window of current job

L120A   move.l  sv_keyq(a6),d4          ;current keyboard queue
        beq.s   L122E                   ;there isn't one, return
        movea.l d4,a0
        lea     -sd_kbd(a0),a0          ;start of window channel definition
        move.w  sv_fstat(a6),d4         ;cursor flash counter
        tst.b   sd_curf(a0)             ;cursor status ?
        beq.s   L1228                   ;suppressed
        sub.w   d3,d4                   ;subtract poll interrrupts missed
        bgt.s   L122A                   ;flash timeout not expired
        jsr     L1BA2(pc)               ;invert cursor
L1228   moveq   #12,d4                  ;renew cursor flash counter
L122A   move.w  d4,sv_fstat(a6)         ;update cursor flash counter
L122E   rts

;Linkage block for MDV directory driver (first and last in internal link)

L1230   dc.l    0                       ;last in list
        dc.l    L1274                   ;I/O
        dc.l    L1730                   ;OPEN
        dc.l    L18B8                   ;CLOSE
        dc.l    L29FC                   ;SLAVING
        dc.l    0                       ;RENAME
        dc.l    0                       ;TRUNCATE
        dc.l    L5000                   ;FORMAT
        dc.l    $0428                   ;length physical definition block
        dc.w    3                       ;device name string
        dc.b    'MDV',0

;Perform I/O operation (routine only called by OPEN/CLOSE).

L125A   movem.l d0/d2/d4-d7/a4-a5,-(a7) ;preserve registers
L125E   movem.l (a7),d0/d2              ;restore IOSS values (repeat attempt)
        moveq   #0,d3                   ;always first attempt for OPEN/CLOSE
        bsr.s   L1274                   ;do I/O operation
        addq.l  #1,d0                   ;operation incomplete ?
        beq.s   L125E                   ;yes, keep trying
        subq.l  #1,d0                   ;redress error code returned
        addq.w  #4,a7                   ;discard d0 operation code
        movem.l (a7)+,d2/d4-d7/a4-a5    ;but restore rest of registers
        rts

;MDV driver I/O

L1274   moveq   #0,d6
        move.b  fs_drive(a0),d6         ;FS drive ID (0-15)
        lsl.b   #2,d6                   ;form offset into
        lea     sv_fsdef(a6),a2         ;physical definition table
        movea.l 0(a2,d6.w),a2           ;get pointer to drive definition
        lsl.b   #2,d6                   ;drive ID to upper nibble
        cmpi.b  #fs_check,d0            ;FS operation ?
        bcs     L135E                   ;no, branch ahead for IO / SD
        cmpi.b  #fs_save,d0             ;unsupported operation ?
        bhi.s   L12A6                   ;yes, return bad parameter
        move.b  L129C-$40(pc,d0.w),d0   ;else use operation code as
        jmp     L129C(pc,d0.w)          ;offset to routine

L129C   dc.b    L12AA-L129C             ;$40 fs.check
        dc.b    L12B0-L129C             ;$41 fs.flush
        dc.b    L12EA-L129C             ;$42 fs.posab
        dc.b    L12F0-L129C             ;$43 fs.posre
        dc.b    L12A6-L129C             ;$44 (undefined)
        dc.b    L12FC-L129C             ;$45 fs.mdinf
        dc.b    L134C-L129C             ;$46 fs.heads
        dc.b    L133E-L129C             ;$47 fs.headr
        dc.b    L1326-L129C             ;$48 fs.load
        dc.b    L133A-L129C             ;$49 fs.save

;MDV Trap #3 undefined operation

L12A6   moveq   #err.bp,d0              ;unsupported operation
        rts

;MDV Trap #3 D0=$40 FS.CHECK - Check pending operations (read & write)

L12AA   moveq   #3,d4                   ;check for empty/pending read [!]
        moveq   #$FF,d5                 ;eor mask for bt_stat
        bra.s   L12B4

;MDV Trap #3 D0=$41 FS.FLUSH - Flush all buffers (write)

L12B0   moveq   #bt.actn,d4             ;check for pending write/read [!]
        moveq   #$00,d5                 ;eor mask for bt_stat
L12B4   move.b  fs_drive(a0),d3         ;FS drive ID (0-15)
        lsl.b   #4,d3                   ;to upper nibble
        bset    #bt..file,d3            ;search for a file block
        move.w  fs_filnr(a0),d2         ;with this file number
        movea.l sv_btbas(a6),a4         ;base of memory block table
L12C6   moveq   #$F1,d0                 ;mask out FS ID and availability
        and.b   (a4),d0                 ;of bt_stat for entry
        cmp.b   d3,d0                   ;is it in use by our FS ?
        bne.s   L12DE                   ;no, try next entry
        cmp.w   bt_filnr(a4),d2         ;is it our file number ?
        bne.s   L12DE                   ;no, try next entry
        move.b  (a4),d0                 ;get bt_stat again
        eor.b   d5,d0                   ;restrict search criteria
        and.w   d4,d0                   ;any outstanding action ?
        bne     L151C                   ;yes,  call slaving operation
L12DE   addq.w  #8,a4                   ;to next table entry
        cmpa.l  sv_bttop(a6),a4         ;top of table ?
        blt.s   L12C6                   ;no, continue searching
        moveq   #err.ok,d0              ;operation complete
        rts

;MDV Trap #3 D0=$42 FS.POSAB - Position absolute file pointer

L12EA   jsr     L1926(pc)               ;update file position absolute
        bra.s   L12F8

;MDV Trap #3 D0=$43 FS.POSRE - Position relative file pointer

L12F0   tst.l   d3                      ;subsequent call ?
        bne.s   L12F8                   ;yes, skip positioning
        jsr     L192C(pc)               ;update file position relative
L12F8   moveq   #io.pend,d0             ;fudge to return
        bra.s   L1362                   ;whether EOF or not

;MDV Trap #3 D0=$45 FS.MDINF - Get information about medium

L12FC   lea     md_mname(a2),a3         ;pointer to medium name
        move.l  (a3)+,(a1)+             ;copy it to buffer
        move.l  (a3)+,(a1)+
        move.w  (a3)+,(a1)+
        move.w  #md_lsect-md_map-2,d0   ;search whole map
        moveq   #0,d1                   ;unused sectors couter
        moveq   #1,d2                   ;good sectors counter
L130E   cmpi.b  #md.svac,md_map(a2,d0.w)
        bhi.s   L131C                   ;bad sector
        bne.s   L131A                   ;used sector
        addq.w  #1,d1                   ;vacant sector
L131A   addq.w  #1,d2                   ;total good sectors
L131C   subq.w  #2,d0                   ;next map entry
        bne.s   L130E                   ;until all checked
        swap    d1                      ;report result as
        move.w  d2,d1                   ;unused/good sector
        rts

;MDV Trap #3 D0=$48 FS.LOAD - Load a file

L1326   moveq   #io.fstrg,d0            ;treat as fetch string
        cmpi.l  #4096,d2                ;length less than 4K ?
        blt.s   L1362                   ;yes, load as string
        bsr     L12B0                   ;else flush all buffers
        beq     L15DA                   ;already done, so load whole file
        rts                             ;else return not complete error

;MDV Trap #3 D0=$49 FS.SAVE - Save a file

L133A   moveq   #io.sstrg,d0            ;treat file as big string
        bra.s   L1362                   ;and operation as IO

;MDV Trap #3 D0=$47 FS.HEADR - Read file header

L133E   moveq   #io.fstrg,d0            ;operation is fetch
        move.l  a1,-(a7)                ;preserve buffer pointer
        bsr.s   L1350                   ;read header
        moveq   #md_deend,d4            ;length of file header
        movea.l (a7)+,a2                ;pointer to file length
        sub.l   d4,(a2)                 ;reduce it accordingly
        rts

;MDV Trap #3 D0=$46 FS.HEADS - Set file header

L134C   moveq   #io.sstrg,d0            ;operation is send
        moveq   #md_denam,d2            ;header set is restricted
L1350   clr.l   fs_nblok(a0)            ;file position to header
        bsr.s   L135E                   ;write or read header
        move.w  #md_deend,fs_nbyte(a0)  ;reposition post header
        rts

;MDV Trap #3 for operations D0 < $40

L135E   ext.l   d1                      ;IOSS parameter
        ext.l   d2                      ;IOSS parameter
L1362   cmpi.b  #io.sstrg,d0            ;SD type operation ?
        bhi     L12A6                   ;yes, return bad parameter
        moveq   #0,d7                   ;offset from a1 to start of buffer
        tst.l   d3                      ;first attempt at call ?
        beq.s   L1372                   ;yes
        sub.l   d1,d7                   ;offset of bytes already done
L1372   subq.b  #io.edlin,d0            ;edit a line operation ?
        beq     L12A6                   ;is not supported
        blt.s   L138E                   ;fetch operations $00-$03
        cmpi.b  #io.share,fs_acces(a0)  ;access mode shared input ?
        beq.s   L13BE                   ;yes, return error read only
        moveq   #-1,d3                  ;signifies operation is send
        subq.w  #2,d0                   ;and is
        beq     L12A6                   ;undefined $06
        blt.s   L13AE                   ;io.sbyte
        bgt.s   L13A0                   ;else io.sstrg

;Fetch operation io.pend/io.fbyte/io.fline/io.fstrg

L138E   moveq   #0,d3
        addq.b  #io.edlin,d0            ;redress operation code
        beq.s   L13AE                   ;io.edlin previously aborted [!]
        move.w  #256,d3                 ;code for no terminator
        subq.b  #io.fline,d0            ;fetch operation is
        blt.s   L13AE                   ;io.pend or io.fbyte
        bgt.s   L13A0                   ;io.fstrg
        moveq   #10,d3                  ;else io.fline terminator is LF

;Multiple operation io.sstrg (io.fstrg/io.fline)

L13A0   add.l   a1,d7                   ;pointer to start of buffer
        move.l  d7,-(a7)                ;preserve it
        add.l   d2,d7                   ;pointer to end of buffer
        bsr.s   L13C2                   ;perform I/O operation
        move.l  a1,d1                   ;updated buffer pointer
        sub.l   (a7)+,d1                ;total bytes sent/fetched
        rts

;Single operation io.sbyte  (io.pend/io.fbyte)

L13AE   move.l  d1,-(a7)                ;preserve long word
        lea     3(a7),a1                ;but point to byte
        move.l  a1,d7                   ;used as one byte buffer
        addq.l  #1,d7                   ;and this is end of buffer
        bsr.s   L13C2                   ;perform I/O operation
        move.l  (a7)+,d1                ;byte read
        rts

L13BE   moveq   #err.ro,d0              ;read only error
        rts

;Generic MDV I/O routine (deals with all fetch and send operations).

L13C2   tst.b   md_estat(a2)            ;error status ?
        bge.s   L13CC                   ;none or 'open pending'
L13C8   moveq   #err.fe,d0              ;return file error
        rts

L13CC   move.l  fs_filnr(a0),d5         ;file number / block
        move.l  fs_nblok(a0),d4         ;file position block / byte
        cmp.l   fs_eblok(a0),d4         ;at EOF ?
        blt.s   L13E8                   ;no
        bgt.s   L13E0                   ;exceeded
        tst.b   d3                      ;else is it a send operation ?
        blt.s   L13E4                   ;yes
L13E0   moveq   #err.ef,d0              ;return end of file
L13E2   rts

L13E4   tst.w   d4                      ;at end of block ?
        beq.s   L140C                   ;yes
L13E8   bsr     L14D6                   ;get pointer to current block
        bne.s   L13E2                   ;return if error / awaiting read
        tst.w   d4                      ;at end of block ?
        bne.s   L1422                   ;no
        move.l  a4,-(a7)                ;preserve slave block pointer
        addq.w  #1,d5                   ;increment to next block
        moveq   #0,d2
        move.w  d5,d2                   ;compare with
        swap    d2                      ;upper word EOF block
        cmp.l   fs_eblok(a0),d2         ;exceeded last block in file ?
        bge.s   L1406                   ;yes
        bsr     L14E2                   ;else get next slave block
L1406   movea.l (a7)+,a4                ;restore slave block pointer
        subq.w  #1,d5                   ;decrement block number
        bra.s   L1422

L140C   cmp.l   a1,d7                   ;end of buffer ?
        bls     L1496                   ;yes, return complete
        bsr     L159E                   ;find a free slave block
        bsr     L1524                   ;procure new sector for file
        move.w  d0,bt_sectr(a4)         ;new sector number
        ori.b   #bt.true,(a4)           ;as it's new it'll be true
L1422   move.l  a4,fs_cblok(a0)         ;current slave block
        btst    #bt..accs,(a4)          ;is block valid ?
        beq     L151C                   ;no, do slaving and return incomplete
        tst.w   d3                      ;operation done ?
        beq.s   L1496                   ;yes, return complete
        move.l  a4,d0                   ;current slave entry
        sub.l   sv_btbas(a6),d0         ;less table base
        lsl.l   #6,d0                   ;x 64
        movea.l d0,a5                   ;forms pointer to
        adda.l  a6,a5                   ;base of corressponding memory block
        adda.w  d4,a5                   ;offset to current byte position
        tst.w   d3                      ;fetch operation ?
        bgt.s   L1464                   ;yes
L1444   cmp.l   a1,d7                   ;end of buffer ?
        bls.s   L1452                   ;yes
        move.b  (a1)+,(a5)+             ;copy byte from slave block to buffer
        bsr.s   L14A2                   ;update block / byte position
        bne.s   L1444                   ;last block not empty, try next
        jsr     L29FC(pc)               ;else, do MDV slaving
L1452   st      fs_updt(a0)             ;file has been updated
        bsr.s   L14B6                   ;set pending slave operation
        cmp.l   fs_eblok(a0),d4         ;at end of file position ?
        blt.s   L147E                   ;no
        move.l  d4,fs_eblok(a0)         ;else this is new end of file position
        bra.s   L147E

L1464   moveq   #0,d0                   ;(word operation on byte fetch)
L1466   cmp.l   a1,d7                   ;end of buffer ?
        bls.s   L147E                   ;yes
        cmp.l   fs_eblok(a0),d4         ;at end of file ?
        bge.s   L149A                   ;yes
        move.b  (a5)+,d0                ;get byte from slave block
        move.b  d0,(a1)+                ;put copy in buffer
        cmp.w   d0,d3                   ;for io.fline is it linefeed ?
        bne.s   L147A                   ;no
        move.l  a1,d7                   ;new end of buffer
L147A   bsr.s   L14A2                   ;update block / byte position
        bne.s   L1466                   ;block not full, get next
L147E   move.l  d4,fs_nblok(a0)         ;set current file position
        cmp.l   a1,d7                   ;end of buffer ?
        bhi     L13C2                   ;no, so do operation again
        cmpi.w  #10,d3                  ;operation io.fline ?
        bne.s   L1496                   ;no, return operation complete
        cmp.b   d0,d3                   ;linefeed character ?
        beq.s   L1496                   ;yes, return complete
        moveq   #err.bo,d0              ;else return buffer overflow
        rts

L1496   moveq   #err.ok,d0              ;operation complete
        rts

L149A   move.l  d4,fs_nblok(a0)         ;update file position
        bra     L13E0                   ;return error end of file

L14A2   addq.w  #1,d4                   ;increment byte count in block
        btst    #9,d4                   ;exceeded block size ?
        beq.s   L14B2                   ;no
        addq.w  #1,d5                   ;increment block
        addi.l  #$0000FE00,d4           ;reduce byte count accordingly
L14B2   tst.w   d4                      ;return byte counter status
        rts

;Set file slave block is awaiting write and sector has pending operation.

L14B6   moveq   #bt.updt,d0             ;file is awaiting write
        or.b    d6,d0                   ;add FS drive ID
        move.b  d0,(a4)                 ;set bt_stat for slave block
L14BC   move.l  a4,d1                   ;slave table entry
        sub.l   sv_btbas(a6),d1         ;less base of table
        lsr.l   #3,d1                   ;reduce to index
        adda.w  bt_sectr(a4),a2         ;offset within FS definition to set
        move.w  d1,md_pendg(a2)         ;pending operation as slave block
        suba.w  bt_sectr(a4),a2         ;redress pointer to FS definition
        sf      md_fail(a2)             ;reset failure counter
        rts

;Find slave block for file current block.

L14D6   movea.l fs_cblok(a0),a4         ;pointer to slave block table entry
        move.l  a4,d0                   ;is there a current slave block ?
        bne.s   L14E2                   ;yes
        movea.l sv_btbas(a6),a4         ;else start at base of table
L14E2   movea.l a4,a5                   ;finished when back here
L14E4   moveq   #bt.inuse,d0            ;mask for current usage
        and.b   (a4),d0                 ;is block empty ?
        beq.s   L14FA                   ;yes, try next
        moveq   #$F0,d0                 ;mask for drive ID
        and.b   (a4),d0                 ;get bt_stat
        cmp.b   d0,d6                   ;is it our FS drive ID ?
        bne.s   L14FA                   ;no, try next
        moveq   #err.ok,d0              ;successful if
        cmp.l   bt_filnr(a4),d5         ;is it our file number / block ?
        beq.s   L1522                   ;yes, return it
L14FA   addq.l  #8,a4                   ;to next entry
        cmpa.l  sv_bttop(a6),a4         ;at top of table ?
        blt.s   L1506                   ;no
        movea.l sv_btbas(a6),a4         ;else start again at base of table
L1506   cmpa.l  a4,a5                   ;whole table searched ?
        bne.s   L14E4                   ;no, try next entry

;Required slave block not found, so prime for loading.

        bsr     L1598                   ;search map for file number / block
        bsr     L159E                   ;find a free slave block
        move.w  d0,bt_sectr(a4)         ;assign sector number to slave block
        ori.b   #bt.rreq,(a4)           ;set entry as awaiting read
        bsr.s   L14BC                   ;and a pending operation
L151C   jsr     L29FC(pc)               ;do MDV slaving
L1520   moveq   #err.nc,d0              ;operation is incomplete
L1522   rts

;Procure new sector for file.

L1524   bsr.s   L157A                       ;set up registers
        subq.b  #1,d2                       ;is it first block in file ?
        bcc.s   L1532                       ;no, else first block is
        moveq   #-40,d0                     ;never < 20 sectors away from
        add.w   md_lsect(a2),d0             ;last sector number allocated
        bra.s   L1534
L1532   bsr.s   L1586                       ;find previous block entry in map
L1534   subi.w  #24,d0                      ;at least 12 sectors between blocks
        bge.s   L154A                       ;sector number is valid
        move.w  #md_lsect-md_map,d1         ;else scan whole map
L153E   subq.w  #2,d1                       ;to next entry
        cmpi.b  #md.sinv,md_filmp(a2,d1.w)  ;invalid sector ?
        beq.s   L153E                       ;yes, try next
        add.w   d1,d0                       ;else this is next contiguous sector
L154A   move.w  d0,-(a7)                    ;preserve prospective sector number
L154C   subq.w  #2,d0                       ;reached sector 0 ? (map)
        bpl.s   L1554                       ;no
        move.w  #md_lsect-md_map-2,d0       ;else point to last sector
L1554   cmpi.b  #md.svac,md_filmp(a2,d0.w)  ;sector vacant ?
        beq.s   L1566                       ;yes
        cmp.w   (a7),d0                     ;whole map scanned ?
        bne.s   L154C                       ;no, try next
        addq.w  #6,a7                       ;discard data & return address
        moveq   #err.df,d0                  ;return drive full
        rts

L1566   addq.b  #1,d2                       ;redress block number
        move.w  d2,md_map(a2,d0.w)          ;update map entry with file/block
        move.w  d0,md_lsect(a2)             ;last sector allocated
        move.w  #-1,md_pendg(a2)            ;pending operation is to write map
        addq.w  #2,a7                       ;discard data
        rts

;Search map for file number / block.

L157A   move.w  #md_lsect-md_map,d0     ;search all map entries
        move.l  d5,d2                   ;file number/block
        lsl.w   #8,d2                   ;rearrange word parameters
        lsr.l   #8,d2                   ;into byte parameters
        rts

L1586   subq.w  #2,d0                   ;next map entry
        blt.s   L1592                   ;not found in map
        cmp.w   md_map(a2,d0.w),d2      ;this entry ?
        bne.s   L1586                   ;no, look at next
        rts

L1592   addq.w  #8,a7                   ;discard return addresses
        bra     L13C8                   ;return file error

L1598   bsr.s   L157A                   ;set up registers
        bsr.s   L1586                   ;find entry in map
        rts

;Find a free slave block.

L159E   movea.l sv_btpnt(a6),a4         ;most recent slave block entry
        movea.l a4,a5                   ;finished when back here
L15A4   addq.w  #8,a4                   ;to next table entry
        cmpa.l  sv_bttop(a6),a4         ;reached top of table ?
        blt.s   L15B0                   ;no
        movea.l sv_btbas(a6),a4         ;else start back at the base
L15B0   moveq   #$0F,d1                 ;mask out status bits
        and.b   (a4),d1                 ;bt_stat
        subq.b  #1,d1                   ;is block empty ?
        beq.s   L15C6                   ;yes, use it
        subq.b  #2,d1                   ;else is block true representation ?
        beq.s   L15C6                   ;yes, use it
        cmpa.l  a5,a4                   ;finished search ?
        bne.s   L15A4                   ;no, try next entry
        addq.w  #4,a7                   ;discard return address
        bra     L1520                   ;return not complete

;Assign free slave block to FS and file/block, set as empty.

L15C6   move.l  a4,sv_btpnt(a6)         ;this is now most recent entry
        move.l  a4,fs_cblok(a0)         ;and channel current slave block
        move.b  d6,(a4)                 ;set FS drive ID for block
        ori.b   #bt.empty,(a4)          ;currently empty
        move.l  d5,bt_filnr(a4)         ;owned by this file number/block
        rts

;Scatter load a whole file (non-slaved operation).

L15DA   jsr     L159E(pc)                         ;find free slave block (d5.l=0)
        subq.l  #bt_end,sv_btpnt(a6)              ;redress current entry pointer
        move.l  a4,d0                             ;free entry pointer less
        sub.l   sv_btbas(a6),d0                   ;base of table gives 8 x index
        lsl.l   #6,d0                             ;x 64 = index x 512
        movea.l d0,a4                             ;which forms offset for
        adda.l  a6,a4                             ;pointer to memory block
        jsr     L29FC(pc)                         ;do MDV slaving
        lea     pc_mctrl,a3                       ;microdrive control port
        andi.b  #$DF,sv_pcint(a6)                 ;disable gap interrupts
        move.b  sv_pcint(a6),pc_intr-pc_mctrl(a3) ;write new setting to hardware port
        lsl.l   #8,d0                             ;(momentarily wait)
        cmp.b   sv_mdrun(a6),d1                   ;required drive turning ?
        beq.s   L1612                             ;yes
        moveq   #err.nc,d0                        ;else return not complete
        bra     L1724                             ;via enable gap interrupts

L1612   moveq   #7,d0                             ;clear 32 bytes (used as bit flags)
        lea     fs_spare(a0),a5                   ;in spare area of channel definition
L1618   clr.l   (a5)+                             ;to be used as bit map of blocks loaded
        dbf     d0,L1618
        move.l  fs_eblok(a0),d2                   ;EOF block/byte (file length)
        tst.w   d2                                ;is last block empty ?
        bne.s   L162C                             ;no, else reduce
        subi.l  #$0000FE00,d2                     ;block count by 1, byte in block is 512
L162C   move.l  d2,fs_nblok(a0)                   ;set file position to EOF
        swap    d2                                ;full blocks to load
        move.w  d2,d0
        lsr.w   #3,d0                             ;per byte flag
        lea     fs_spare(a0),a5                   ;load blocks flags stored here
        bra.s   L163E
L163C   st      (a5)+                             ;set all 8 bits in byte flag
L163E   dbf     d0,L163C
        move.w  #$FF01,d1                         ;rotating bit mask for block flags
        moveq   #$07,d0                           ;mask out
        and.w   d2,d0                             ;remainder of blocks
        rol.w   d0,d1                             ;set bit in byte for each block
        move.b  d1,(a5)                           ;forms last/partial load block flag
        suba.w  #fs.hdlen,a1                      ;offset buffer pointer by header
        movem.l a0-a2/a4,-(a7)
L1656   andi.w  #$F8FF,sr                         ;interrupts on (housekeeping aperture)
        ori.w   #$0700,sr                         ;interrupts off
L165E   movea.l (a7),a0                           ;restore channel definition pointer
        lea     fs_spare+$20(a0),a1               ;temporary buffer area for
        dc.w    $4EB9,0,L523A   *** jsr L523A     ;md_sectr (read header)
        bra.s   L16D6                             ;bad medium, return file error
        bra.s   L1656                             ;bad header, try next
        movem.l (a7),a0-a2/a4
        add.w   d7,d7                             ;sector number
        beq     L16FC                             ;was 0, the map (increment md_fail)
        move.b  md_filmp(a2,d7.w),d0              ;file number of sector
        cmp.b   fs_filnr+1(a0),d0                 ;match required file number ?
        bne.s   L1656                             ;no, get next header
        moveq   #0,d4
        move.b  md_blkmp(a2,d7.w),d4              ;get block number of file
        move.l  d4,d7
        moveq   #$07,d5                           ;mask
        and.w   d4,d5                             ;block number remainder
        lsr.w   #3,d4                             ;block number
        btst    d5,fs_spare(a0,d4.w)              ;this block required to be loaded ?
        beq.s   L1656                             ;no, try next
        movea.w d4,a5                             ;preserve index to load bit map
        movea.l a4,a1                             ;memory block pointer
        dc.w    $4EB9,0,L525C    *** jsr L525C    ;md_read, read sector data
        bra.s   L165E                             ;failed, try next
        movem.l (a7),a0-a2/a4                     ;else read OK
        bclr    d5,fs_spare(a0,a5.w)              ;this block successfully loaded
        move.l  #128,d0                           ;(long words in full sector)
        movea.l a1,a5                             ;load buffer running pointer
        move.l  d7,d1                             ;sector number read
        lsl.w   #8,d1                             ;x 256
        add.l   d1,d1                             ;x 2 (= 512 byte sectors)
        adda.l  d1,a5                             ;offset pointer accordingly
        cmp.w   fs_nblok(a0),d7                   ;last block/sector ?
        bne.s   L16C6                             ;no
        move.w  fs_nbyte(a0),d0                   ;else last sector bytes
        ror.l   #2,d0                             ;form counter for bytes/long words
L16C6   tst.w   d7                                ;sector 0 ?
        bne.s   L16DA                             ;no
        moveq   #fs.hdlen,d1                      ;else skip file header
        adda.w  d1,a4                             ;in read buffer
        adda.w  d1,a5                             ;and destination buffer
        subi.w  #16,d0                            ;reduce counter
        bra.s   L16DA
L16D6   bra.s   L170A

L16D8   move.l  (a4)+,(a5)+             ;copy data read to buffer
L16DA   dbf     d0,L16D8                ;long word at a time
        clr.w   d0                      ;and then
        rol.l   #2,d0                   ;any remaining odd bytes
        bra.s   L16E6
L16E4   move.b  (a4)+,(a5)+             ;copy data read to buffer
L16E6   dbf     d0,L16E4                ;one byte at a time

        moveq   #8,d0                   ;32 bytes to check
        lea     fs_spare(a0),a5         ;starting here
L16F0   tst.l   (a5)+                   ;any bit flags still set ?
        bne     L165E                   ;yes, blocks outstanding
        subq.w  #1,d0                   ;until
        bgt.s   L16F0                   ;d0 zero, err.ok and
        bra.s   L170C                   ;load operation complete

L16FC   addq.b  #1,md_fail(a2)          ;increment failure counter
        cmpi.b  #8,md_fail(a2)          ;expired ?
        blt     L1656                   ;no, get next header
L170A   moveq   #err.fe,d0              ;file error
L170C   movem.l (a7)+,a0-a2/a4
        sf      md_fail(a2)             ;clear failure counter
        moveq   #0,d7
        move.w  fs_eblok(a0),d7         ;EOF block
        lsl.l   #8,d7                   ;x 256
        add.l   d7,d7                   ;x 2 (= 512 byte sectors)
        adda.l  d7,a1                   ;offset buffer pointer
        adda.w  fs_ebyte(a0),a1         ;to very end of file within buffer
L1724   ori.b   #pc.maskg,sv_pcint(a6)  ;enable gap interrupts
        andi.w  #$F8FF,sr               ;interrupts on
        rts

;MDV driver OPEN

L1730   movea.l a1,a2                   ;pointer to physical definition
        moveq   #0,d1
        move.b  fs_drivn(a2),d1         ;drive number (1-8)
        moveq   #0,d0
        move.b  fs_drive(a0),d0         ;FS drive ID (0-15)
        lsl.b   #2,d0                   ;multiply by 4
        lea     sv_mdrun(a6),a4
        move.b  d0,1(a4,d1.w)           ;sv_mddid, set FS drive ID for drive
        cmp.b   (a4),d1                 ;currently turning MDV the required drive ?
        beq.s   L1766                   ;yes
        tst.b   7(a4,d1.w)              ;sv_mdsta, status of required drive ?
        bne.s   L1766                   ;has operations pending, else
        move.b  #1,md_estat(a2)         ;set error status as 'open pending'
        jsr     L2A00(pc)               ;do MDV slaving
L175C   tst.b   md_estat(a2)            ;keep checking drive error status
        bgt.s   L175C                   ;until finished waiting
        bmi     L1844                   ;result was error, return not found
L1766   lea     fs_spare(a0),a4         ;buffer for reading directory
        moveq   #md_deend,d2            ;bytes to read is directory entry
        move.l  d2,fs_eblok(a0)         ;temporarily restrict EOF
        moveq   #forever,d3             ;timeout
        bsr     L184C                   ;fetch file header for directory
        move.l  (a4),d4                 ;md_delen, file length
        move.l  d4,d0                   ;rearrange file size so that
        lsl.l   #7,d0                   ;upper word is block number
        lsr.w   #7,d0                   ;lower word is byte in block
        move.l  d0,fs_eblok(a0)         ;update true EOF position
        cmpi.b  #io.dir,fs_acces(a0)    ;was open to directory ?
        beq     L1840                   ;yes, return complete
        lsr.l   #6,d4                   ;highest file number
        moveq   #0,d5                   ;current file number
        moveq   #0,d6                   ;free entry number in directory
        bra.s   L17BE                   ;search directory headers

;Fetch file header from directory and check for name match.

L1794   bsr     L184C                   ;fetch next file header from directory
        tst.l   (a4)                    ;md_delen, file size
        beq.s   L17B8                   ;file entry is unused, free for use
        move.l  a0,-(a7)                ;preserve pointer to channel definition
        move.l  a6,-(a7)                ;and pointer to system variables
        suba.l  a6,a6                   ;prepare for a6 relative operation
        lea     md_denam(a4),a1         ;pointer to file name in directory
        lea     fs_fname(a0),a0         ;pointer to requested open file name
        moveq   #1,d0                   ;comparison type is ignore case
        jsr     L3A9C(pc)               ;ut_cstr, compare two strings
        movea.l (a7)+,a6                ;restore pointer to system variable
        movea.l (a7)+,a0                ;and pointer to channel definition
        beq.s   L1822                   ;filename strings matched
        bra.s   L17BE                   ;else try next file in direcotory

;Search directory file headers, find existing or set up new file.

L17B8   tst.w   d6                      ;free entry in directory ?
        bne.s   L17BE                   ;one already found
        move.w  d5,d6                   ;else this one is potentially useable
L17BE   addq.w  #1,d5                   ;increment file number
        cmp.w   d4,d5                   ;last in directory ?
        blt.s   L1794                   ;no, check directory entry for name match
        move.b  fs_acces(a0),d0         ;open type
        blt.s   L1840                   ;was delete, never found, ignore and return
        cmpi.b  #io.new,d0              ;was open type new or overwrite file ?
        blt.s   L1844                   ;no, return not found
        tst.w   d6                      ;free entry in directory found ?
        beq.s   L17E2                   ;no, use a new file number
        move.l  d6,d0                   ;existing free entry number
        lsl.l   #6,d0                   ;create offset into directory file
        lsl.l   #7,d0                   ;upper word is block number
        lsr.w   #7,d0                   ;lower word is byte in block
        move.l  d0,fs_nblok(a0)         ;this is new directory file position
        move.l  d6,d5                   ;existing free file number in directory
L17E2   move.l  d2,(a4)+                ;prepare new directory entry, write md_delen
        clr.w   (a4)+                   ;clear md_deacs and md_detyp
        clr.l   (a4)+                   ;clear md_deinf
        clr.l   (a4)+
        moveq   #9,d0                   ;40 bytes to copy (2+36 and 2 spare)
        lea     fs_fname(a0),a5         ;from requested file name string
L17F0   move.l  (a5)+,(a4)+             ;to md_denam, directory copy of file name
        dbf     d0,L17F0
        lea     fs_spare(a0),a4         ;back to start of buffer directory entry
        bsr.s   L1850                   ;write empty header file into directory
        tst.w   d6                      ;did new file use existing free entry ?
        bne.s   L1810                   ;yes, so skip setting up new one
        addq.w  #1,d4                   ;increment highest file number
        lsl.l   #6,d4                   ;create offset into directory
        move.l  d4,(a4)                 ;new directory file length
        clr.l   fs_nblok(a0)            ;current file position to start
        moveq   #4,d2                   ;four bytes to write
        bsr.s   L1850                   ;write new directory file length
        moveq   #fs.hdlen,d2            ;new file header
L1810   move.l  d2,(a4)                 ;initial file length is just header
        clr.l   fs_nblok(a0)            ;header is at start of file
        clr.l   fs_eblok(a0)            ;file is initially empty
        move.w  d5,fs_filnr(a0)         ;set file number in channel definition
        bsr.s   L1850                   ;write file header
        bra.s   L1840                   ;operation complete

;File name match found when searching directory for open file name.

L1822   move.b  fs_acces(a0),d0         ;open access mode
        blt.s   L185E                   ;was delete
        cmpi.b  #io.new,d0              ;was open new, overwrite or directory ?
        bge.s   L1848                   ;yes, so return already exists
        move.l  (a4),d0                 ;get existing file size
        lsl.l   #7,d0                   ;upper word is block number
        lsr.w   #7,d0                   ;lower word is byte in block
        move.l  d0,fs_eblok(a0)         ;set EOF position
        move.w  d5,fs_filnr(a0)         ;set file number
        move.l  d2,fs_nblok(a0)         ;current file position is after header
L1840   moveq   #err.ok,d0              ;operation successful
        bra.s   L184A                   ;return complete
L1844   moveq   #err.nf,d0              ;file not found
        bra.s   L184A
L1848   moveq   #err.ex,d0              ;file already exists
L184A   rts

L184C   moveq   #io.fstrg,d0            ;fetch bytes
        bra.s   L1852
L1850   moveq   #io.sstrg,d0            ;send bytes
L1852   movea.l a4,a1                   ;pointer to buffer
        jsr     L125A(pc)               ;perform I/O operation
        beq.s   L184A                   ;successful operation
        addq.w  #4,a7                   ;discard caller return address
        rts                             ;return error directly

;Clear up resources used by file to be deleted.

L185E   moveq   #-md_deend,d1           ;backtrack to directory entry
        jsr     L192C(pc)               ;update file position relative
        clr.l   (a4)                    ;md_delen, zero the file size
        clr.w   md_denam(a4)            ;zero file name string length
        bsr.s   L1850                   ;write deleted entry to directory
        move.w  #md_lsect-md_map,d0     ;offset to word containing
        lea     md_map(a2,d0.w),a4      ;last sector allocated (file/block)
L1874   cmp.b   (a4),d5                 ;does it match the deleted file number ?
        bne.s   L1880                   ;no
        move.w  #$FD00,(a4)             ;else make it vacant entry
        clr.w   md_pendg-md_map(a4)     ;with no pending operations
L1880   subq.w  #2,a4                   ;back one sector
        subq.w  #2,d0                   ;offset reduced to sector 0 ?
        bne.s   L1874                   ;no, make spare all sectors of deleted file
        move.b  fs_drive(a0),d1         ;FS drive ID (0-15)
        lsl.b   #4,d1                   ;to upper nibble
        addq.w  #1,d1                   ;set bt..file bit
        movea.l sv_btbas(a6),a4         ;base of slave block table
L1892   moveq   #$F1,d0                 ;mask out
        and.b   (a4),d0                 ;FS drive ID and bt..file bit
        cmp.b   d0,d1                   ;is it used by FS of deleted file ?
        bne.s   L18A4                   ;no
        cmp.w   bt_filnr(a4),d5         ;used by deleted file ?
        bne.s   L18A4                   ;no
        move.b  #bt.empty,(a4)          ;release the block
L18A4   addq.w  #bt_end,a4              ;to next table entry
        cmpa.l  sv_bttop(a6),a4         ;at top of table ?
        blt.s   L1892                   ;no, check next entry
        move.w  #-1,md_pendg(a2)        ;pending operations for this FS
        jsr     L29FC(pc)               ;do MDV slaving
        bra.s   L1840                   ;return operation complete

;MDV driver CLOSE

L18B8   tst.b   fs_updt(a0)             ;has file been updated ?
        beq.s   L18F0                   ;no
        move.l  fs_eblok(a0),d0         ;EOF block/byte position
        lsl.w   #7,d0                   ;shuffling bits
        lsr.l   #7,d0                   ;yields absolute EOF position
        lea     fs_spare(a0),a1         ;temporary buffer for header
        move.l  d0,(a1)                 ;md_delen, update new file length
        clr.l   fs_nblok(a0)            ;point to file header md_delen
        bsr.s   L191A                   ;write new length to file
        moveq   #0,d0
        move.w  fs_filnr(a0),d0         ;file number on drive
        lsl.l   #6,d0                   ;derive directory header offest
        lsl.l   #7,d0                   ;upper word is block number
        lsr.w   #7,d0                   ;lower word is byte in block
        move.l  d0,fs_nblok(a0)         ;align file position
        clr.w   fs_filnr(a0)            ;fudge access to file 0, the directory
        st      fs_eblok+1(a0)          ;and fudge directory EOF >127K
        bsr.s   L191A                   ;write new file length into directory
        jsr     L29FC(pc)               ;do MDV slaving
L18F0   moveq   #0,d0
        move.b  fs_drive(a0),d0         ;FS drive ID (0-15)
        lsl.b   #2,d0                   ;create offset into
        lea     sv_fsdef(a6),a2         ;physical definitions table
        movea.l 0(a2,d0.w),a2           ;get pointer to one used by drive
        subq.b  #1,fs_files(a2)         ;one less file open
        lea     sv_fslst(a6),a1         ;pointer to list of file channel definitions
        lea     fs_next(a0),a0          ;link point in channel definition for file
        jsr     L39E2(pc)               ;ut_unlnk, unlink it from list
        lea     -sv_lio(a0),a0          ;base of heap for channel definition
        jsr     L305E(pc)               ;mm_rechp, reclaim memory used
        rts                             ;operation complete

L191A   moveq   #io.sstrg,d0            ;send bytes
        moveq   #4,d2                   ;bytes to send is long word file length
        jsr     L125A(pc)               ;perform I/O operation
        suba.w  d1,a1                   ;restore buffer pointer
        rts

;Align file position d1.l absolute.

L1926   moveq   #md_deend,d2            ;minimum file position
        add.l   d2,d1                   ;positioning starts post header
        bra.s   L1938

;Align file position d1.l relative.

L192C   moveq   #md_deend,d2            ;minimum file position
        move.l  fs_nblok(a0),d0         ;current block/byte file position
L1932   lsl.w   #7,d0                   ;shuffling bits
        lsr.l   #7,d0                   ;yields absolute file position
        add.l   d0,d1                   ;add required offset
L1938   move.l  d1,d0                   ;new file position
        lsl.l   #7,d0                   ;block number
        lsr.w   #7,d0                   ;byte in block
        cmp.l   fs_eblok(a0),d0         ;does this exceed EOF position ?
        ble.s   L194C                   ;no
        move.l  fs_eblok(a0),d0         ;else use EOF position
        moveq   #0,d1                   ;this time no offset, just
        bra.s   L1932                   ;get absolute EOF position

L194C   sub.l   d2,d1                   ;is file position post header ?
        bge.s   L1954                   ;yes
        move.l  d2,d1                   ;else minimum position is post header
        bra.s   L1938                   ;get new position in block/byte format

L1954   move.l  d0,fs_nblok(a0)         ;update file position
        rts

;Trap #1 D0=$13 MT.RCLCK - Read clock
;Trap #1 D0=$14 MT.SCLCK - Set clock
;Trap #1 D0=$15 MT.ACLCK - Adjust clock

L195A   lea     pc_clock,a0             ;real time clock port
L1960   move.l  (a0),d2                 ;get date counter
        cmp.l   (a0),d2                 ;and again until both same
        bne.s   L1960                   ;in case pending internal carry
        cmpi.b  #mt_sclck,d0            ;operation was...
        bgt.s   L1972                   ;adjust clock
        beq.s   L1974                   ;set clock
        move.l  d2,d1                   ;else time read in seconds
        bra.s   L198E                   ;operation is complete

L1972   add.l   d2,d1                   ;add reading to required adjustment
L1974   sf      (a0)+                   ;initiate clock setting
        moveq   #$EF,d0                 ;addressing mask
        moveq   #3,d3                   ;four bytes to set
L197A   rol.l   #8,d1                   ;in order top byte to bottom
        moveq   #0,d2                   ;word counter
        move.b  d1,d2                   ;of byte value
        bra.s   L1984
L1982   move.b  d0,(a0)                 ;write d0 mask d2 times
L1984   dbf     d2,L1982
        ror.b   #1,d0                   ;address next byte in clock register
        dbf     d3,L197A                ;until long word written
L198E   bra     L03A4                   ;return from trap err.ok

;CON/SCR Trap #3 for SD/FS operations D0 > $07 (and indirectly io.sbyte)

L1992   move.b  sd_curf(a0),-(a7)       ;cursor status ?
        ble.s   L19A4                   ;invisible or suppressed
        movem.w d0-d2,-(a7)
        jsr     L1BA2(pc)               ;invert the visible cursor
        movem.w (a7)+,d0-d2
L19A4   cmpi.b  #io.sbyte,d0            ;indirect send byte call ?
        bne.s   L19B0                   ;no, SD or FS operation only
        jsr     L1A4C(pc)               ;send byte to window
        bra.s   L19CE

L19B0   cmpi.w  #sd.gcur,d0             ;any higher than $36 (FS)
        bhi.s   L19EE                   ;is bad operation code
        cmpi.w  #sd.extop,d0            ;any lower than $09
        blt.s   L19EE                   ;is bad operation code
        bgt.s   L19C2                   ;do SD operation
        jsr     (a2)                    ;else call user routine
        bra.s   L19CE

L19C2   add.b   d0,d0                   ;operation code forms offset
        movea.w L19F2-$14(pc,d0.w),a3   ;to offset to required routine
        lsr.l   #1,d0                   ;restore operation code value
        jsr     L1992(pc,a3.w)          ;and call operation routine
L19CE   tst.b   (a7)+                   ;previous cursor status ?
        ble.s   L19E8                   ;was invisible or supressed so return
        tst.l   d0                      ;else was there an error in operation ?
        bne.s   L19E8                   ;yes, return
        tst.b   sd_curf(a0)             ;present cursor status ?
        bge.s   L19E8                   ;visible or suppressed, return
        movem.l d0-d2,-(a7)
        jsr     L1B86(pc)               ;sd.cure, reenable the cursor
        movem.l (a7)+,d0-d2
L19E8   tst.l   d0                      ;error code from operation
        rts

;CON/SCR Trap #3 undefined operation

L19EC   addq.l  #4,a7                   ;discard return address
L19EE   moveq   #err.bp,d0              ;unsupported operation
        bra.s   L19CE

;CON/SCR Trap #3 operations $0A to $36

L19F2   dc.w    L1A78-L1992             ;$0A sd.pxenq
        dc.w    L1A8A-L1992             ;$0B sd.chenq
        dc.w    L1AF8-L1992             ;$0C sd.bordr
        dc.w    L1AB6-L1992             ;$0D sd.wdef
        dc.w    L1B86-L1992             ;$0E sd.cure
        dc.w    L1B94-L1992             ;$0F sd.curs
        dc.w    L1C22-L1992             ;$10 sd.pos
        dc.w    L1C28-L1992             ;$11 sd.tab
        dc.w    L1C32-L1992             ;$12 sd.nl
        dc.w    L1C3E-L1992             ;$13 sd.pcol
        dc.w    L1C48-L1992             ;$14 sd.ncol
        dc.w    L1C56-L1992             ;$15 sd.prow
        dc.w    L1C60-L1992             ;$16 sd.nrow
        dc.w    L1C6C-L1992             ;$17 sd.pixp
        dc.w    L1CBE-L1992             ;$18 sd.scrol
        dc.w    L1CBE-L1992             ;$19 sd.scrtp
        dc.w    L1CBE-L1992             ;$1A sd.scrbt
        dc.w    L1CC8-L1992             ;$1B sd.pan
        dc.w    L19EC-L1992             ;$1C (undefined)
        dc.w    L19EC-L1992             ;$1D (undefined)
        dc.w    L1CC8-L1992             ;$1E sd.panln
        dc.w    L1CC8-L1992             ;$1F sd.panrt
        dc.w    L1CAE-L1992             ;$20 sd.clear
        dc.w    L1CB4-L1992             ;$21 sd.clrtp
        dc.w    L1CB4-L1992             ;$22 sd.clrbt
        dc.w    L1CB4-L1992             ;$23 sd.clrln
        dc.w    L1CB4-L1992             ;$24 sd.clrrt
        dc.w    L1D8C-L1992             ;$25 sd.fount
        dc.w    L1CA6-L1992             ;$26 sd.recol
        dc.w    L1DAA-L1992             ;$27 sd.setpa
        dc.w    L1DAA-L1992             ;$28 sd.setst
        dc.w    L1DAA-L1992             ;$29 sd.setin
        dc.w    L1DBE-L1992             ;$2A sd.setfl
        dc.w    L1DD2-L1992             ;$2B sd.setul
        dc.w    L1DCC-L1992             ;$2C sd.setmd
        dc.w    L1DDA-L1992             ;$2D sd.setsz
        dc.w    L1D2C-L1992             ;$2E sd.fill
        dc.w    L1BF2-L1992             ;$2F sd.donl
        dc.w    L1F0A-L1992             ;$30 sd.point
        dc.w    L1F18-L1992             ;$31 sd.line
        dc.w    L1F46-L1992             ;$32 sd.arc
        dc.w    L1FEE-L1992             ;$33 sd.elips
        dc.w    L1EA4-L1992             ;$34 sd.scale
        dc.w    L1EB2-L1992             ;$35 sd.flood
        dc.w    L1E54-L1992             ;$36 sd.gcur

;Send d1.b to window of CON/SCR channel.

L1A4C   jsr     L1BCE(pc)               ;cursor within window limits ?
        blt.s   L1A76                   ;no, return out of range
        move.b  d1,d2                   ;byte to send
        move.l  sd_xmin(a0),d0          ;screen x/y position of window
        add.l   sd_xpos(a0),d0          ;yields screen position of cursor
        move.w  d0,d1                   ;y position
        swap    d0                      ;x position
        move.b  sd_cattr(a0),d3         ;windows character attributes
        lea     sd_smask(a0),a1         ;pointer to strip colour mask
        movem.l sd_font(a0),a2-a3       ;pointers to character fonts
        jsr     L2840(pc)               ;print byte character on screen
        bra     L1C48                   ;sd.ncol, to next cursor position

L1A76   rts

;CON/SCR Trap #3 D0=$0A SD.PXENQ - Read window size/cursor position (pixel)

L1A78   jsr     L1BF2(pc)               ;sd.donl, do any pending newline
        move.l  sd_xsize(a0),(a1)       ;window x/y size
        move.l  sd_xpos(a0),4(a1)       ;cursor x/y position
        moveq   #err.ok,d0              ;operation complete
        rts

;CON/SCR Trap #3 D0=$0B SD.CHENQ - Read window size/position (character)

L1A8A   bsr.s   L1A78                   ;sd.pxenq, get cursor position/window size
        move.l  d1,-(a7)                ;preserve
        move.w  sd_xinc(a0),d0          ;cursor width
        bsr.s   L1AAA                   ;divide into window width
        bsr.s   L1AAA                   ;and cursor x position
        subq.w  #6,a1                   ;backtrack to depth parameters
        move.w  sd_yinc(a0),d0          ;cursor depth
        bsr.s   L1AAA                   ;divide into window depth
        bsr.s   L1AAA                   ;and cursor y position
        move.l  (a7)+,d1                ;restore
        suba.w  #10,a1                  ;start of buffer (undocumented)
        moveq   #err.ok,d0              ;operation complete
        rts

L1AAA   moveq   #0,d1                   ;(long divide)
        move.w  (a1),d1                 ;get width/depth
        divu.w  d0,d1                   ;divide by cursor width/depth
        move.w  d1,(a1)                 ;update parameter
        addq.w  #4,a1                   ;skip to next
        rts

;CON/SCR Trap #3 D0=$0D SD.WDEF - Redefine window

L1AB6   movem.w d1-d4,-(a7)
        jsr     L1C1C(pc)               ;reset cursor and newline status
        movem.w (a1),d0-d3              ;window width, depth, x/y position 
        exg     d0,d2
        exg     d1,d3
        moveq   #0,d4
        bclr    d4,d0                   ;make even window x position
        bclr    d4,d2                   ;make even window width
        tst.w   d2                      ;any width ?
        beq     L1B64                   ;no, return out of range
        tst.w   d3                      ;any depth ?
        beq     L1B64                   ;no, return out of range
        move.w  d0,d4                   ;x position
        add.w   d2,d4                   ;plus width
        bcs     L1B64                   ; > 65535 is out of range
        cmpi.w  #hw_xpix,d4             ;hardware limit exceeded ?
        bhi.s   L1B64                   ;yes, return out of range
        move.w  d1,d4                   ;y position
        add.w   d3,d4                   ;plus depth
        bcs.s   L1B64                   ; > 65535 is out of range
        cmpi.w  #hw_ypix,d4             ;hardware limit exceeded ?
        bhi.s   L1B64                   ;yes, return out of range
        clr.w   sd_borwd(a0)            ;zero border width
        bra.s   L1B12

;CON/SCR Trap #3 D0=$0C SD.BORDR - Set window border

L1AF8   move.b  d1,sd_bcolr(a0)         ;border colour byte
L1AFC   movem.w d1-d4,-(a7)
        cmp.w   sd_borwd(a0),d2         ;border width
        beq.s   L1B0A                   ;is zero
        jsr     L1C1C(pc)               ;reset cursor and newline status
L1B0A   movem.w sd_xmin(a0),d0-d4       ;window position, size and border width
        bsr.s   L1B6C                   ;remove border from window
L1B12   move.w  2(a7),d4                ;requested border width
        cmpi.w  #hw_ypix,d4             ;too big ? [!]
        bhi.s   L1B64                   ;yes, return out of range
        bsr.s   L1B6E                   ;add border to window
        movem.w d0-d4,sd_xmin(a0)       ;update window position, size and border width
        beq.s   L1B60                   ;border width was zero
        move.w  (a7),d1                 ;border colour
        cmpi.b  #$80,d1                 ;transparent ?
        beq.s   L1B60                   ;yes, done
        movea.l a7,a1                   ;temporary buffer to contain
        jsr     L27D8(pc)               ;get colour mask for colour d1.b
        move.w  sd_ymin(a0),d1          ;restore window y position
        bsr.s   L1B6C                   ;remove border from window (registers only)
        neg.w   d4                      ;restore positive border width
        exg     d4,d3                   ;respecify depth as border width to
        jsr     L25BE(pc)               ;draw top block
        add.w   d4,d1                   ;offset y position by window depth
        sub.w   d3,d1                   ;then subtract border width to
        jsr     L25BE(pc)               ;draw bottom block
        add.w   d3,d1                   ;y position to bottom of window
        sub.w   d4,d1                   ;then back to top of window
        exg     d4,d3                   ;restore window depth
        add.w   d4,d4                   ;border is twice as wide at left/right
        exg     d4,d2                   ;respecify width as border width to
        jsr     L25BE(pc)               ;draw lefthand block
        add.w   d4,d0                   ;offset x position by window width
        sub.w   d2,d0                   ;then subtract border width to
        jsr     L25BE(pc)               ;draw righthand block
L1B60   moveq   #err.ok,d0              ;operation complete
        bra.s   L1B66

L1B64   moveq   #err.or,d0              ;out of range
L1B66   movem.w (a7)+,d1-d4
        rts

;Remove/add border to window.

L1B6C   neg.w   d4                      ;border to be removed
L1B6E   add.w   d4,d1                   ;update y position
        add.w   d4,d4                   ;twice for
        sub.w   d4,d3                   ;update window depth
        ble.s   L1B82                   ;bad window size
        add.w   d4,d0                   ;update x position
        add.w   d4,d4                   ;twice again for
        sub.w   d4,d2                   ;update window width
        ble.s   L1B82                   ;bad window width
        asr.w   #2,d4                   ;redress border width
        rts

L1B82   addq.w  #4,a7                   ;discard return address
        bra.s   L1B64                   ;return out of range

;CON/SCR Trap #3 D0=$0E SD.CURE - Enable cursor

L1B86   moveq   #1,d2                   ;cursor to be visible
        tst.b   sd_curf(a0)             ;cursor status ?
        bgt.s   L1BCA                   ;visible, operation complete
        jsr     L1BF2(pc)               ;sd.donl, do any pending newline
        bra.s   L1BAA                   ;update cursor status

;CON/SCR Trap #3 D0=$0F SD.CURS - Disable cursor

L1B94   moveq   #0,d2                   ;cursor to be suppressed
        tst.b   sd_curf(a0)             ;cursor status ?
        bgt.s   L1BAA                   ;visible
        move.b  d2,sd_curf(a0)          ;suppress cursor
        bra.s   L1BCA                   ;operation complete

;Invert cursor and, if not suppressed, update cursor on screen. 

L1BA2   move.b  sd_curf(a0),d2          ;cursor status ?
        beq.s   L1BCA                   ;is suppressed, return
        neg.b   d2                      ;toggle visibility
L1BAA   jsr     L1BCE(pc)               ;cursor within window limits ?
        blt.s   L1BCC                   ;no, return out of range
        move.b  d2,sd_curf(a0)          ;update cursor status
        move.l  sd_xmin(a0),d0          ;window x/y position in screen
        add.l   sd_xpos(a0),d0          ;cursor position within window
        move.w  d0,d1                   ;cursor y position in screen
        swap    d0                      ;cursor x position in screen
        movem.w sd_xinc(a0),d2-d3       ;cursor size x/y
        jsr     L2548(pc)               ;invert cursor block on screen
L1BCA   moveq   #err.ok,d0
L1BCC   rts

;Check if cursor is within window limits

L1BCE   move.l  sd_xpos(a0),d0          ;cursor x/y position
        bmi.s   L1BEE                   ;bad x position
        tst.w   d0
        bmi.s   L1BEE                   ;bad y position 
        add.l   sd_xinc(a0),d0          ;cursor x/y size
        cmp.w   sd_ysize(a0),d0         ;window y size
        bhi.s   L1BEE                   ;exceeds depth
        swap    d0
        cmp.w   sd_xsize(a0),d0         ;window x size
        bhi.s   L1BEE                   ;exceeds width
        moveq   #err.ok,d0              ;else cursor is within window
        rts

L1BEE   moveq   #err.or,d0              ;out of range
        rts

;CON/SCR Trap #3 D0=$2F SD.DONL - Do pending newline

L1BF2   tst.b   sd_nlsta(a0)            ;newline status ?
        beq.s   L1C1A                   ;none pending, return
L1BF8   movem.l d0-d2/a1,-(a7)
        jsr     L1C32(pc)               ;sd.nl, do a newline
        beq.s   L1C12                   ;operation complete
        moveq   #sd_scrol,d0            ;else err.or so scroll window
        move.w  sd_yinc(a0),d1          ;by depth of one cursor line
        neg.w   d1                      ;upwards
        jsr     L1CBE(pc)               ;sd.scrol, scroll entire window
        clr.w   sd_xpos(a0)             ;cursor position to start of line
L1C12   sf      sd_nlsta(a0)            ;clear the pending newline
        movem.l (a7)+,d0-d2/a1
L1C1A   rts

L1C1C   clr.l   sd_xpos(a0)             ;cursor to top lefthand of window
        bra.s   L1C9A

;CON/SCR Trap #3 D0=$10 SD.POS - Set cursor position

L1C22   mulu.w  sd_yinc(a0),d2          ;cursor depth
        bra.s   L1C2C

;CON/SCR Trap #3 D0=$11 SD.TAB - Set horizontal position

L1C28   move.w  sd_ypos(a0),d2          ;cursor y position
L1C2C   mulu.w  sd_xinc(a0),d1          ;cursor width
        bra.s   L1C6C                   ;sd.pixp, set pixel position

;CON/SCR Trap #3 D0=$12 SD.NL - Do newline

L1C32   moveq   #0,d1                   ;x position leftmost
        move.w  sd_ypos(a0),d2          ;y position
        add.w   sd_yinc(a0),d2          ;to next line down
        bra.s   L1C6C                   ;sd.pixp, set pixel position

;CON/SCR Trap #3 D0=$13 SD.PCOL - Set to previous column

L1C3E   move.w  sd_xpos(a0),d1          ;current cursor x position
        sub.w   sd_xinc(a0),d1          ;decrement one column left
        bra.s   L1C50

;CON/SCR Trap #3 D0=$14 SD.NCOL - Set to next column

L1C48   move.w  sd_xpos(a0),d1          ;current cursor x position
        add.w   sd_xinc(a0),d1          ;increment to next column right
L1C50   move.w  sd_ypos(a0),d2          ;on same line
        bra.s   L1C6C                   ;sd.pixp, set pixel position

;CON/SCR Trap #3 D0=$15 SD.PROW - Set to previous row

L1C56   move.w  sd_ypos(a0),d2          ;current cursor y position
        sub.w   sd_yinc(a0),d2          ;decrement one row up
        bra.s   L1C68

;CON/SCR Trap #3 D0=$16 SD.NROW - Set to next row

L1C60   move.w  sd_ypos(a0),d2          ;current cursor y position
        add.w   sd_yinc(a0),d2          ;increment to next row down
L1C68   move.w  sd_xpos(a0),d1          ;within same column

;CON/SCR Trap #3 D0=$17 SD.PIXP - Set to pixel position

L1C6C   move.w  d1,d0                   ;new x position
        blt.s   L1CA2                   ;bad position
        add.w   sd_xinc(a0),d0          ;would cursor still be
        cmp.w   sd_xsize(a0),d0         ;within window width ?
        bhi.s   L1CA2                   ;no, cannot set position
        move.w  d2,d0                   ;new y position
        blt.s   L1CA2                   ;bad position
        add.w   sd_yinc(a0),d0          ;would cursor still be
        cmp.w   sd_ysize(a0),d0         ;within window depth ?
        bhi.s   L1CA2                   ;no, cannot set position
L1C88   btst    #mc..m256,sv_mcsta(a6)  ;display mode ?
        beq.s   L1C94                   ;is mode 4, otherwise
        bclr    #0,d1                   ;mode 8 requires even pixel position
L1C94   movem.w d1-d2,sd_xpos(a0)       ;update cursor position
L1C9A   sf      sd_nlsta(a0)            ;clear any pending newline
        moveq   #err.ok,d0              ;operation complete
        rts

L1CA2   moveq   #err.or,d0              ;position would be out of range
        rts

;CON/SCR Trap #3 D0=$26 SD.RECOL - Recolour a window

L1CA6   moveq   #0,d0                   ;whole window operation
        lea     L25D6(pc),a2            ;routine is recolour block
        bra.s   L1CE0                   ;perform operation

;CON/SCR Trap #3 D0=$20 SD.CLEAR - Clear whole window

L1CAE   jsr     L1C1C(pc)               ;reset cursor and newline status
        moveq   #sd.clear,d0            ;restore operation code

;CON/SCR Trap #3 D0=$21 SD.CLRTP - Clear top of window
;CON/SCR Trap #3 D0=$22 SD.CLRBT - Clear bottom of window
;CON/SCR Trap #3 D0=$23 SD.CLRLN - Clear cursor line
;CON/SCR Trap #3 D0=$24 SD.CLRRT - Clear right of cursor

L1CB4   subi.w  #sd.clear,d0            ;whole window if sd.clear
        lea     L25BE(pc),a2            ;routine is draw solid block
        bra.s   L1CDC                   ;perform operation

;CON/SCR Trap #3 D0=$18 SD.SCROL - Scroll entire window
;CON/SCR Trap #3 D0=$19 SD.SCRTP - Scroll top of window
;CON/SCR Trap #3 D0=$1A SD.SCRBT - Scroll bottom of window

L1CBE   subi.w  #sd.scrol,d0            ;whole window if sd.scrol
        lea     L25FE(pc),a2            ;routine is scroll block
        bra.s   L1CDC                   ;perform operation

;CON/SCR Trap #3 D0=$1B SD.PAN   - Pan whole window
;CON/SCR Trap #3 D0=$1E SD.PANLN - Pan cursor line
;CON/SCR Trap #3 D0=$1F SD.PANRT - Pan righthand side of cursor line

L1CC8   subi.w  #sd.pan,d0              ;whole window if sd.pan
        lea     L2648(pc),a2            ;routine is pan block
        btst    #mc..m256,sv_mcsta(a6)  ;mode 4
        beq.s   L1CDC                   ;yes
        bclr    #0,d1                   ;else make distance to pan even
L1CDC   lea     sd_pmask(a0),a1         ;pointer to paper mask
L1CE0   movem.l d4-d5,-(a7)
        move.w  d1,d4                   ;pan/scroll distance
        move.w  d0,d5                   ;operation code
        movem.w sd_xmin(a0),d0-d3       ;block area is window position and size
        subq.w  #1,d5                   ;operation encompasses...
        blt.s   L1D22                   ;whole window, perform operation
        bgt.s   L1CFA                   ;cursor line or below
        move.w  sd_ypos(a0),d3          ;else restrict block to top of window
        bra.s   L1D22                   ;perform operation

L1CFA   add.w   sd_ypos(a0),d1          ;restrict to cursor line and below
        subq.w  #2,d5                   ;operation within cursor line ?
        bge.s   L1D12                   ;yes
        sub.w   sd_ypos(a0),d3          ;restrict block to bottom of window
        move.w  sd_yinc(a0),d5          ;cursor line depth
        add.w   d5,d1                   ;starting line below cursor line
        sub.w   d5,d3                   ;block less cursor line depth
        ble.s   L1D24                   ;cursor line was last in window
        bra.s   L1D22                   ;perform operation

L1D12   move.w  sd_yinc(a0),d3          ;block depth is cursor line depth
        tst.w   d5                      ;operation whole cursor line ?
        beq.s   L1D22                   ;yes
        add.w   sd_xpos(a0),d0          ;block starts at cursor position
        sub.w   sd_xpos(a0),d2          ;reduce block size accordingly
L1D22   jsr     (a2)                    ;perform operation
L1D24   moveq   #err.ok,d0              ;complete
        movem.l (a7)+,d4-d5
        rts

;CON/SCR Trap #3 D0=$2E SD.FILL - Fill a block

L1D2C   lea     L25BE(pc),a3            ;routine is draw solid block
        btst    #sd..xor,sd_cattr(a0)   ;overwrite mode xor ?
        beq.s   L1D3C                   ;no
        lea     L25CA(pc),a3            ;routine is eor block
L1D3C   movea.l a1,a2                   ;parameter block
        subq.w  #4,a7                   ;make room for
        movea.l a7,a1                   ;pointer to
        jsr     L27D8(pc)               ;set colour mask for d1.b
        move.l  #$01FF01FF,d3           ;mask restricts parameters to 511 [d]
        btst    #mc..m256,sv_mcsta(a6)  ;mode 4 ?
        beq.s   L1D58                   ;yes
        bclr    #16,d3                  ;else make even width or x position
L1D58   movem.l (a2),d0/d2              ;get block width/depth & x/y position
        exg     d0,d2
        and.l   d3,d0                   ;restrict x/y position
        and.l   d3,d2                   ;restrict width/depth
        move.l  d0,d3
        add.l   d2,d3                   ;block extent within window
        cmp.w   sd_ysize(a0),d3         ;exceed window depth ?
        bgt.s   L1D88                   ;yes
        swap    d3
        cmp.w   sd_xsize(a0),d3         ;exceed window width ?
        bgt.s   L1D88                   ;yes
        add.l   sd_xmin(a0),d0          ;block x/y position within screen
        move.w  d2,d3                   ;block depth
        swap    d2                      ;      width
        move.w  d0,d1                   ;      y position
        swap    d0                      ;      x position
        jsr     (a3)                    ;perform block fill
        moveq   #err.ok,d0              ;operation complete
L1D84   addq.w  #4,a7                   ;discard colour mask
        rts

L1D88   moveq   #err.or,d0              ;block size out of range
        bra.s   L1D84

;CON/SCR Trap #3 D0=$25 SD.FOUNT - Set character fonts

L1D8C   move.l  a1,d0                   ;font #1 address
        bgt.s   L1D96                   ;has been supplied, else
        lea     MAD9A,a1                ;character font table #1
L1D96   move.l  a2,d0                   ;font #2 address
        bgt.s   L1DA0                   ;has been supplied, else
        lea     MB106,a2                ;character font table #2
L1DA0   movem.l a1-a2,sd_font(a0)       ;set font addresses
        moveq   #err.ok,d0              ;operation complete
        rts

;CON/SCR Trap #3 D0=$27 SD.SETPA - Set paper colour
;CON/SCR Trap #3 D0=$28 SD.SETST - Set strip colour
;CON/SCR Trap #3 D0=$29 SD.SETIN - Set ink colour

L1DAA   subi.w  #sd_setpa,d0            ;use as offset to
        move.b  d1,sd_pcolr(a0,d0.w)    ;set colour byte
        lsl.w   #2,d0                   ;create offset to
        lea     sd_pmask(a0,d0.w),a1    ;point to
        moveq   #err.ok,d0
        jmp     L27D8(pc)               ;set colour mask

;CON/SCR Trap #3 D0=$2A SD.SETFL - Set flash mode

L1DBE   btst    #mc..m256,sv_mcsta(a6)  ;mode 4 ?
        beq     L1E3C                   ;yes, ignore and return
        moveq   #$02,d0                 ;sd.cattr mask for sd..flsh
        bra.s   L1DD4                   ;update sd.cattr

;CON/SCR Trap #3 D0=$2C SD.SETMD - Set write mode

L1DCC   moveq   #$0C,d0                 ;sd.cattr mask for sd..xor and sd..trns
        lsl.b   #2,d1                   ;position value appropriately
        bra.s   L1E3E                   ;update sd.cattr

;CON/SCR Trap #3 D0=$2B SD.SETUL - Set underline mode

L1DD2   moveq   #$01,d0                 ;sd.cattr mask for sd..ulin
L1DD4   tst.b   d1                      ;if <> 0
        sne     d1                      ;means turn on
        bra.s   L1E3E                   ;update sd.cattr

;CON/SCR Trap #3 D0=$2D SD.SETSZ - Set character size

L1DDA   jsr     L1BF2(pc)                    ;sd.donl, do any pending newline
        andi.w  #$0003,d1                    ;restrict requested character width
        andi.w  #$0001,d2                    ;and height
        btst    #mc..m256,sv_mcsta(a6)       ;mode 4 ?
        beq.s   L1DF2                        ;yes
        bset    #1,d1                        ;else force double width
L1DF2   move.b  L1E4E(pc,d1.w),sd_xinc+1(a0) ;set cursor width in pixels
        move.b  L1E52(pc,d2.w),sd_yinc+1(a0) ;set cursor height in pixles
        lsl.b   #1,d1                        ;rearrange bits
        or.b    d2,d1                        ;to form
        lsl.b   #4,d1                        ;setting required in sd.cattr
        moveq   #$70,d0                      ;mask for sd..dbwd/sd..exwd/sd..dbht
        bsr.s   L1E3E                        ;update sd.cattr appropriately
        move.w  sd_xpos(a0),d0               ;cursor x position
        add.w   sd_xinc(a0),d0               ;if next column
        cmp.w   sd_xsize(a0),d0              ;within window width
        bls.s   L1E1A                        ;then skip
        jsr     L1BF8(pc)                    ;do a newline
L1E1A   move.w  sd_ypos(a0),d0               ;cursor y position
        add.w   sd_yinc(a0),d0               ;if next row
        cmp.w   sd_ysize(a0),d0              ;within window depth
        bls.s   L1E4A                        ;then operation complete
        moveq   #sd.scrol,d0                 ;else scroll up whole window
        moveq   #-10,d1                      ;by normal height
        jsr     L1CBE(pc)                    ;sd.scrol, scroll entire window
        subi.w  #10,sd_ypos(a0)              ;update cursor y position
        bge.s   L1E4A                        ;within range, return
        clr.w   sd_ypos(a0)                  ;else set at top of window
L1E3C   bra.s   L1E4A                        ;operation complete

;Update sd.cattr according to requested attribute settings.

L1E3E   and.b   d0,d1                        ;mask out pertinent bits
        not.b   d0                           ;invert to preserve others
        and.b   d0,sd_cattr(a0)              ;first clear the bits
        or.b    d1,sd_cattr(a0)              ;then set as requested
L1E4A   moveq   #err.ok,d0                   ;operation complete
        rts

L1E4E   dc.b    6,8,12,16                    ;character pixel widths
L1E52   dc.b    10,20                        ;character pixel heights

;CON/SCR Trap #3 D0=$36 SD.GCUR - Set text cursor with graphic coordinates

L1E54   lea     $18(a1),a4              ;top of ri stack
        jsr     L21DC(pc)               ;copy graphics coords to ri stack
        move.l  L2242(pc),-(a1)         ;fixed H/V scaling factor is
        move.w  L2240(pc),-(a1)         ;fp[1.355]
        jsr     L20D8(pc)               ;put fp sd_ysize & sd_scal on ri stack
        lea     L1E90(pc),a3            ;list of operations
        jsr     L419E(pc)               ;perform ri.execb
        lea     $18(a1),a1
        bsr.s   L1E8A
        move.w  (a1)+,d1
        bsr.s   L1E8A
        move.w  sd_ysize(a0),d2
        sub.w   (a1)+,d2
        bset    #sd..gchr,sd_cattr(a0)
        jmp     L1C88(pc)

L1E8A   moveq   #ri.nint,d0
        jmp     L4196(pc)               ;perform ri.exec

L1E90   dc.b    ri.abs,ri.div,ri.dup
        dc.b    ri.load+ri.var02
        dc.b    ri.load+ri.var05
        dc.b    ri.sub,ri.mult
        dc.b    ri.load+ri.var00
        dc.b    ri.sub
        dc.b    ri.store+ri.var00
        dc.b    ri.load+ri.var03
        dc.b    ri.load+ri.var04
        dc.b    ri.sub,ri.mult,ri.mult
        dc.b    ri.load+ri.var01
        dc.b    ri.add
        dc.b    ri.store+ri.var01
        dc.b    ri.term,0

;CON/SCR Trap #3 D0=$34 SD.SCALE - Set graphic scale and origin

L1EA4   lea     sd_yorg(a0),a2
        moveq   #18,d0
L1EAA   move.w  (a1)+,(a2)+
        subq.w  #2,d0
        bne.s   L1EAA
        rts

;CON/SCR Trap #3 D0=$35 SD.FLOOD - Set fill mode/user vectors

L1EB2   movea.l a0,a4
        tst.l   d1
        bne.s   L1EC2
        bsr.s   L1EFA
        move.b  #0,sd_fmod(a4)
        bra.s   L1EF4

L1EC2   cmpi.l  #1,d1
        bne.s   L1EE4
        bsr.s   L1EFA
        move.b  #1,sd_fmod(a4)
        move.l  #1040,d1
        jsr     L2FAE(pc)               ;mm_alchp
        bne.s   L1EF6
        move.l  a0,sd_fbuf(a4)
        bra.s   L1EF4

L1EE4   btst    #0,d1
        beq.s   L1EF0
        clr.l   sd_fuse(a4)
        bra.s   L1EF4

L1EF0   move.l  d1,sd_fuse(a4)
L1EF4   moveq   #err.ok,d0
L1EF6   movea.l a4,a0
        rts

L1EFA   tst.b   sd_fmod(a4)
        beq.s   L1F08
        movea.l sd_fbuf(a4),a0
        jsr     L305E(pc)               ;mm_rechp
L1F08   rts

;CON/SCR Trap #3 D0=$30 SD.POINT - Plot point

L1F0A   move.l  a4,-(a7)
        lea     12(a1),a4               ;top of ri stack
        move.l  -(a4),-(a1)
        move.l  -(a4),-(a1)
        move.l  -(a4),-(a1)
        movea.l (a7)+,a4

;CON/SCR Trap #3 D0=$31 SD.LINE - Draw line

L1F18   movem.l a3/a5,-(a7)
        lea     L1F26(pc),a3
        subq.l  #6,a1
        bra     L2100

L1F26   dc.w    1
        dc.w    L1F45-*
        dc.w    L205C-*
        dc.w    L1F30-*
        dc.w    L2136-*

L1F30   dc.b    ri.load+ri.var05
        dc.b    ri.dup
        dc.b    ri.load+ri.var07
        dc.b    ri.sub,ri.dup,ri.dup,ri.dup,ri.dup
        dc.b    ri.load+ri.var03
        dc.b    ri.load+ri.var01
        dc.b    ri.sub
        dc.b    ri.load+ri.var10
        dc.b    ri.mult
        dc.b    ri.load+ri.var00
        dc.b    ri.load+ri.var02
        dc.b    ri.sub
        dc.b    ri.load+ri.var10
        dc.b    ri.mult
        dc.b    ri.load+ri.var05
        dc.b    ri.dup
L1F44   dc.b    ri.dup
L1F45   dc.b    ri.term

;CON/SCR Trap #3 D0=$32 SD.ARC - Plot arc

L1F46   movem.l a3/a5,-(a7)
        lea     2(a1),a4
        tst.l   (a4)+
        bgt.s   L1F64
        moveq   #ri.neg,d0               ;perform ri.exec
        jsr     L4196(pc)
        movem.l (a4)+,d2-d7
        movem.l d2-d4,-(a4)
        movem.l d5-d7,-(a4)
L1F64   lea     L1F6C(pc),a3
        bra     L2100

L1F6C   dc.w    3
        dc.w    L1F45-*
        dc.w    L205C-*
        dc.w    L1F7E-*
        dc.w    L1FA8-*
        dc.w    L1FD0-*
        dc.w    L1FCE-*
        dc.w    L21EA-*
        dc.w    L2136-*

L1F7E   dc.b    ri.load+ri.var00
        dc.b    ri.load+ri.var02
        dc.b    ri.sub
        dc.b    ri.load+ri.var04
        dc.b    ri.load+ri.var09
        dc.b    ri.mult
        dc.b    ri.load+ri.var11
        dc.b    ri.div
        dc.b    ri.load+ri.var07
        dc.b    ri.add
        dc.b    ri.load+ri.var01
        dc.b    ri.load+ri.var03
        dc.b    ri.sub,ri.dup,ri.dup,ri.mult
        dc.b    ri.load+ri.var14
        dc.b    ri.dup,ri.mult,ri.add
        dc.b    ri.load+ri.var04
        dc.b    ri.load+ri.var08
        dc.b    ri.div,ri.sin,ri.dup,ri.add,ri.dup,ri.mult
        dc.b    ri.div,ri.dup,ri.sqrt
        dc.b    ri.load+ri.var16
        dc.b    ri.load+ri.var14
        dc.b    ri.load+ri.var12
        dc.b    ri.add,ri.div,ri.atan
        dc.b    ri.load+ri.var04
        dc.b    ri.load+ri.var08
        dc.b    ri.div,ri.sub,ri.term

L1FA8   tst.b   -$58(a4)
        bmi.s   L1FBC
        move.l  L224E(pc),-(a1)         ;PI mantissa
        move.w  L224C(pc),-(a1)         ;PI exponent
        moveq   #ri.add,d0
        bsr     L4196                   ;perform ri.exec
L1FBC   move.l  -$1E(a4),d0
        swap    d0
        subq.w  #1,d0
        swap    d0
        cmp.l   L224C(pc),d0            ;PI exponent & first word of mantissa
        blt.s   L1FCE
        clr.l   d2
L1FCE   rts

L1FD0   dc.b    ri.dup
        dc.b    ri.load+ri.var04
        dc.b    ri.add,ri.dup,ri.add,ri.cos
        dc.b    ri.load+ri.var07
        dc.b    ri.add
        dc.b    ri.store+ri.var14
        dc.b    ri.dup,ri.cos,ri.dup
        dc.b    ri.load+ri.var18
        dc.b    ri.mult
        dc.b    ri.load+ri.var08
        dc.b    ri.load+ri.var18
        dc.b    ri.div,ri.dup
        dc.b    ri.store+ri.var11
        dc.b    ri.store+ri.var12
        dc.b    ri.load+ri.var19
        dc.b    ri.sin,ri.dup
        dc.b    ri.load+ri.var18
        dc.b    ri.mult
        dc.b    ri.store+ri.var19
        dc.b    ri.store+ri.var18
        dc.b    ri.load+ri.var17
        dc.b    ri.store+ri.var16
        dc.b    ri.term

;CON/SCR Trap #3 D0=$33 SD.ELIPS - Draw circle/ellipse

L1FEE   movem.l a3/a5,-(a7)
        lea     L1FFC(pc),a3
        bra     L2100

L1FFA   rts

L1FFC   dc.w    3
        dc.w    L200E-*
        dc.w    L2018-*
        dc.w    L2017-*
        dc.w    L205C-*
        dc.w    L203A-*
        dc.w    L1FFA-*
        dc.w    L21EA-*
        dc.w    L2136-*

L200E   dc.b    ri.load+ri.var03
        dc.b    ri.abs
        dc.b    ri.store+ri.var03
        dc.b    ri.load+ri.var02
        dc.b    ri.load+ri.var02
        dc.b    ri.abs
        dc.b    ri.load+ri.var03
        dc.b    ri.mult
        dc.b    ri.store+ri.var02
L2017   dc.b    ri.term

L2018   cmpi.w  #$0801,(a1)
        addq.w  #6,a1
        bge.s   L2032
        move.l  #$6487ED51,-(a1)
        move.w  #$0801,-(a1)            ;PI/2 fp[1.570796]
        lea     L2034(pc),a3            ;list of operations
        jsr     L419E(pc)               ;perform ri.execb
L2032   rts

L2034   dc.b    ri.add
        dc.b    ri.load+ri.var02
        dc.b    ri.load+ri.var03
        dc.b    ri.store+ri.var02
        dc.b    ri.store+ri.var03
        dc.b    ri.term

L203A   dc.b    ri.load+ri.var05
        dc.b    ri.load+ri.var09
        dc.b    ri.dup,ri.add
        dc.b    ri.load+ri.var03
        dc.b    ri.dup,ri.mult
        dc.b    ri.load+ri.var02
        dc.b    ri.dup,ri.mult
        dc.b    ri.load+ri.var04
        dc.b    ri.sin,ri.dup
        dc.b    ri.load+ri.var03
        dc.b    ri.mult,ri.dup
        dc.b    ri.load+ri.var00
        dc.b    ri.add,ri.dup
        dc.b    ri.store+ri.var00
        dc.b    ri.store+ri.var02
        dc.b    ri.load+ri.var04
        dc.b    ri.cos,ri.dup
        dc.b    ri.load+ri.var03
        dc.b    ri.mult,ri.dup,ri.neg
        dc.b    ri.load+ri.var01
        dc.b    ri.add,ri.dup
        dc.b    ri.store+ri.var01
        dc.b    ri.store+ri.var03
        dc.b    ri.term

L205C   movem.l d0-d1/a4,-(a7)
        clr.l   -(a1)
        clr.w   -(a1)
        moveq   #1,d0
        ror.l   #2,d0
        move.w  #$0800,d1
        moveq   #4,d2
L206E   move.l  d0,-(a1)
        move.w  d1,-(a1)
        addq.w  #1,d1
        dbf     d2,L206E
        addq.w  #1,(a1)
        lea     L2252(pc),a3
        moveq   #8,d2                   ;three floats (PI, 0.001 & 1.355)
L2080   move.w  -(a3),-(a1)
        dbf     d2,L2080
        move.w  sd_xsize(a0),-$1A(a6)   ;window width
        move.w  sd_ysize(a0),-$1E(a6)
        clr.w   -$1C(a6)
        clr.w   -$20(a6)
        move.w  sd_xmin(a0),-$34(a6)
        move.w  sd_ymin(a0),-$38(a6)
        clr.w   -$36(a6)
        clr.w   -$3A(a6)
        move.b  sd_cattr(a0),-$23(a6)
        move.b  sd_fmod(a0),-$32(a6)
        move.l  sd_fbuf(a0),-$2A(a6)    ;location of fill buffer
        move.l  sd_fuse(a0),-$2E(a6)    ;user-defined fill vector
        moveq   #1,d2
        bsr.s   L20D8                   ;put fp sd_ysize & sd_scal on ri stack
        lea     L20EE(pc),a3            ;list of operations
        bsr     L419E                   ;perform ri.execb
        movem.l (a7)+,d0-d1/a4
        rts

;Put fp sd_ysize and fp sd_scal on ri stack

L20D8   move.w  sd_ysize(a0),-(a1)
        subq.w  #1,(a1)
        moveq   #ri.float,d0
        bsr     L4196                   ;perform ri.exec
        move.l  sd_scal+2(a0),-(a1)
        move.w  sd_scal(a0),-(a1)
        rts

L20EE   dc.b    ri.abs,ri.div,ri.dup
        dc.b    ri.load+ri.var00
        dc.b    ri.mult
        dc.b    ri.store+ri.var00
        dc.b    ri.dup
        dc.b    ri.load+ri.var01
        dc.b    ri.mult
        dc.b    ri.store+ri.var01
        dc.b    ri.dup
        dc.b    ri.load+ri.var02
        dc.b    ri.mult
        dc.b    ri.store+ri.var02
        dc.b    ri.load+ri.var03
        dc.b    ri.mult
        dc.b    ri.store+ri.var03
        dc.b    ri.term

L2100   movem.l d1-d7/a0/a2/a4-a6,-(a7)
        link    a6,#-58
        lea     $1E(a1),a4
        move.w  (a3)+,d1
L210E   pea     2(a3)
        adda.w  (a3),a3                 ;list of operations
        bsr     L419E                   ;perform ri.execb
        movea.l (a7)+,a3
        pea     2(a3)
        adda.w  (a3),a3
        jsr     (a3)
        movea.l (a7)+,a3
        dbf     d1,L210E
        unlk    a6
        movem.l (a7)+,d1-d7/a0/a2/a4-a6
        movem.l (a7)+,a3/a5
        moveq   #err.ok,d0
        rts

L2136   movem.l d0-d1/a3,-(a7)
        tst.l   d2
        beq     L21C4
        move.l  d2,-16(a6)
        moveq   #-1,d3
        btst    #mc..m256,sv_mcsta+sv_base
        beq.s   L2158
        add.l   d2,d2
        lsl.l   #1,d3
        addq.w  #1,-48(a4)
L2158   move.w  d3,-34(a6)
        neg.l   d2
        move.l  d2,-4(a6)
        move.l  d2,-12(a6)
        clr.l   -8(a6)
        bsr     L20D8                   ;put fp sd_ysize & sd_scal on ri stack
        moveq   #ri.div,d0
        bsr     L4196                   ;perform ri.exec
        bsr.s   L21DC                   ;copy graphics coords to ri stack
        lea     L2218(pc),a3            ;list of operations
        bsr     L419E                   ;perform ri.execb
        bsr.s   L21CA
        move.l  d1,d2
        bsr.s   L21CA
        move.l  d1,d3
        bsr.s   L21CA
        move.l  d1,d4
        bsr.s   L21CA
        move.l  d1,d6
        bsr.s   L21CA
        move.l  d1,d7
        lea     -$60(a4),a1
        bsr.s   L21D6
        movea.w (a1)+,a5
        lea     -$18(a4),a1
        bsr.s   L21D2
        move.l  (a1)+,-24(a6)
        bsr.s   L21D2
        move.l  (a1)+,d1
        and.w   -34(a6),d1
        move.l  d1,-20(a6)
        bsr.s   L21D2
        move.l  (a1)+,d1
        bsr.s   L21D2
        move.l  (a1)+,d0
        and.w   -34(a6),d0
        bsr     L2252
        bsr     L22CA
L21C4   movem.l (a7)+,d0-d1/a3
        rts

L21CA   addq.w  #4,(a1)
        bsr.s   L21D2
        move.l  (a1)+,d1
        rts

L21D2   moveq   #6,d0
        bra.s   L21D8

L21D6   moveq   #ri.nint,d0
L21D8   jmp     L4196(pc)               ;perform ri.exec

;Copy graphics fp coords to ri stack

L21DC   move.l  sd_xorg+2(a0),-(a1)
        move.l  sd_xorg-2(a0),-(a1)
        move.l  sd_yorg(a0),-(a1)
        rts

L21EA   dc.b    ri.load+ri.var17
        dc.b    ri.load+ri.var16
        dc.b    ri.sub
        dc.b    ri.load+ri.var20
        dc.b    ri.mult
        dc.b    ri.load+ri.var18
        dc.b    ri.mult
        dc.b    ri.load+ri.var18
        dc.b    ri.dup,ri.mult,ri.dup
        dc.b    ri.load+ri.var16
        dc.b    ri.mult
        dc.b    ri.load+ri.var20
        dc.b    ri.dup,ri.mult,ri.dup
        dc.b    ri.load+ri.var16
        dc.b    ri.mult
        dc.b    ri.load+ri.var23
        dc.b    ri.load+ri.var17
        dc.b    ri.mult,ri.add
        dc.b    ri.store+ri.var23
        dc.b    ri.load+ri.var17
        dc.b    ri.mult,ri.add,ri.dup
        dc.b    ri.load+ri.var21
        dc.b    ri.mult
        dc.b    ri.load+ri.var22
        dc.b    ri.load+ri.var19
        dc.b    ri.mult,ri.add,ri.neg
        dc.b    ri.load+ri.var23
        dc.b    ri.load+ri.var19
        dc.b    ri.mult
        dc.b    ri.load+ri.var22
        dc.b    ri.load+ri.var21
        dc.b    ri.mult,ri.add
        dc.b    ri.store+ri.var20
        dc.b    ri.store+ri.var21
        dc.b    ri.term,0

L2218   dc.b    ri.load+ri.var25
        dc.b    ri.mult,ri.neg,ri.dup
        dc.b    ri.load+ri.var01
        dc.b    ri.add
        dc.b    ri.store+ri.var01
        dc.b    ri.load+ri.var03
        dc.b    ri.add
        dc.b    ri.store+ri.var03
        dc.b    ri.mult,ri.neg,ri.dup
        dc.b    ri.load+ri.var00
        dc.b    ri.add
        dc.b    ri.load+ri.var13
        dc.b    ri.mult
        dc.b    ri.store+ri.var00
        dc.b    ri.load+ri.var02
        dc.b    ri.add
        dc.b    ri.load+ri.var13
        dc.b    ri.mult
        dc.b    ri.store+ri.var02
        dc.b    ri.load+ri.var07
        dc.b    ri.load+ri.var13
        dc.b    ri.div,ri.dup
        dc.b    ri.load+ri.var22
        dc.b    ri.mult
        dc.b    ri.store+ri.var22
        dc.b    ri.dup
        dc.b    ri.load+ri.var20
        dc.b    ri.mult
        dc.b    ri.store+ri.var20
        dc.b    ri.dup,ri.mult
        dc.b    ri.load+ri.var23
        dc.b    ri.mult
        dc.b    ri.store+ri.var23
        dc.b    ri.term

;fixed value floats

L2240   dc.w    $0801
L2242   dc.l    $56B851EC               ;fp[1.355] screen H/V ratio for scaling
        dc.w    $07F7
        dc.l    $4189374C               ;fp[1E-3] which is 1/1000
L224C   dc.w    $0802
L224E   dc.l    $6487ED51               ;fp[3.141593] which is PI

L2252   cmpa.l  #-1,a5
        beq.s   L2264
        tst.l   d2
        beq.s   L22C0
        tst.l   d3
        beq.s   L22C0
        bra.s   L2266

L2264   suba.l  a5,a5
L2266   movea.l d3,a1
        movea.l d4,a2
        adda.l  a1,a2
        movea.l d2,a3
        adda.l  d4,a3
        adda.l  a2,a3
        add.l   d4,d3
        neg.l   d3
        asr.l   #1,d3
        add.l   d7,d3
        move.l  d6,d4
        sub.l   d3,d4
        asr.l   #2,d2
        neg.l   d2
        sub.l   d6,d2
        asr.l   #1,d2
        add.l   d3,d2
        move.l  a5,-(a7)
        move.l  d2,-(a7)
        move.l  d3,d7
        move.l  d4,d6
        jsr     L2432(pc)
        movea.l (a7)+,a0
        movea.w #8,a4
        move.b  #$FF,-36(a6)
L22A0   tst.l   d7
        blt.s   L22A8
        tst.l   d6
        bge.s   L22C6
L22A8   tst.b   -36(a6)
        bge.s   L22B4
        bsr     L238A
        bra.s   L22B8

L22B4   bsr     L23D0
L22B8   cmpa.w  #0,a4
        bgt.s   L22A0
        addq.l  #4,a7
L22C0   addq.l  #4,a7
        jmp     L21C4(pc)

L22C6   movea.l (a7)+,a4
        rts

L22CA   bsr     L248A
        bsr.s   L2330
        move.l  a0,d4
        bge.s   L22E4
        move.l  -4(a6),d4
        move.l  -8(a6),d5
        sub.l   a1,d7
        add.l   a2,d6
        adda.l  d7,a0
        bra.s   L22F2

L22E4   move.l  -12(a6),d4
        move.l  -16(a6),d5
        sub.l   a2,d7
        add.l   a3,d6
        suba.l  d6,a0
L22F2   add.l   d4,d0
        add.l   d5,d1
        tst.l   d5
        bge.s   L2302
        adda.l  #hw_scrll,a5
        bra.s   L230A

L2302   beq.s   L230C
        suba.l  #hw_scrll,a5
L230A   swap    d3
L230C   moveq   #1,d5
        btst    #mc..m256,sv_mcsta+sv_base
        beq.s   L231A
        moveq   #2,d5
L231A   tst.l   d4
        bge.s   L2326
        rol.w   d5,d2
        bcc.s   L232E
        subq.l  #2,a5
        bra.s   L232E

L2326   beq.s   L232E
        ror.w   d5,d2
        bcc.s   L232E
        addq.l  #2,a5
L232E   bra.s   L22CA

L2330   move.w  a4,d4
        blt.s   L2386
        subq.w  #2,d4
        bgt.s   L2360
        move.l  d1,d4
        sub.l   -24(a6),d4
        bge.s   L2342
        neg.l   d4
L2342   subq.l  #1,d4
        bgt.s   L2360
        move.l  d0,d4
        sub.l   -20(a6),d4
        bge.s   L2350
        neg.l   d4
L2350   btst    #mc..m256,sv_mcsta+sv_base
        beq.s   L235C
        subq.l  #1,d4
L235C   subq.l  #1,d4
        ble.s   L2386
L2360   tst.l   d7
        bne.s   L2368
        tst.l   d6
        beq.s   L2386
L2368   bge.s   L2376
        tst.b   -36(a6)
        bgt.s   L2376
        bsr     L238A
        bra.s   L2330

L2376   tst.l   d6
        bge.s   L2388
        tst.b   -36(a6)
        blt.s   L2388
        bsr     L23D0
        bra.s   L2330

L2386   addq.l  #4,a7
L2388   rts

L238A   tst.l   -4(a6)
        bne.s   L2394
        neg.l   -12(a6)
L2394   tst.l   -8(a6)
        bne.s   L239E
        neg.l   -16(a6)
L239E   move.l  a1,d4
        neg.l   d4
        movea.l d4,a1
        add.l   a2,d4
        movea.l d4,a2
        adda.l  a1,a2
        move.l  d4,d5
        lsl.l   #2,d5
        sub.l   a3,d5
        movea.l d5,a3
        neg.l   d7
        sub.l   d4,d7
        move.l  a0,d5
        neg.l   d5
        sub.l   d6,d5
        add.l   d7,d5
        movea.l d5,a0
        sub.l   d7,d6
        sub.l   d7,d6
        sub.l   d4,d6
        subq.l  #1,a4
        move.b  #1,-36(a6)
        rts

L23D0   tst.l   -8(a6)
        bne.s   L23E2
        clr.l   -4(a6)
        move.l  -16(a6),-8(a6)
        bra.s   L23F2

L23E2   tst.l   -4(a6)
        bne.s   L23F2
        clr.l   -8(a6)
        move.l  -12(a6),-4(a6)
L23F2   move.l  a2,d4
        lsl.l   #1,d4
        sub.l   a3,d4
        move.l  d4,d5
        sub.l   a1,d5
        movea.l d5,a1
        suba.l  a3,a2
        move.l  a3,d5
        neg.l   d5
        movea.l d5,a3
        move.l  a2,d5
        asr.l   #1,d5
        neg.l   d5
        add.l   d6,d5
        add.l   d5,d7
        move.l  d6,d5
        asr.l   #1,d5
        neg.l   d5
        sub.l   a0,d5
        add.l   d7,d5
        movea.l d5,a0
        move.l  a3,d5
        asr.l   #3,d5
        adda.l  d5,a0
        neg.l   d6
        asr.l   #1,d4
        add.l   d4,d6
        subq.l  #1,a4
        move.b  #$FF,-36(a6)
        rts

L2432   movem.l d0-d1,-(a7)
        clr.l   d2
        neg.l   d1
        subq.l  #1,d1
        move.w  sd_ysize(a0),d2
        add.w   sd_ymin(a0),d2
        add.l   d2,d1
        move.w  sd_xmin(a0),d2
        add.l   d2,d0
        move.l  sd_imask(a0),d3                   ;ink colour mask
        btst    #0,d1
        beq.s   L2458
        swap    d3
L2458   movea.l #hw_scrn,a5
        lsl.l   #hw_scrlb,d1
        adda.l  d1,a5
        move.l  d0,d1
        lsr.l   #3,d1
        lsl.l   #1,d1
        adda.l  d1,a5
        andi.w  #$000F,d0
        moveq   #0,d2
        move.w  #$8080,d2
        btst    #mc..m256,sv_mcsta+sv_base
        beq.s   L2482
        move.w  #$C0C0,d2
L2482   ror.w   d0,d2
        movem.l (a7)+,d0-d1
        rts

L248A   cmp.l   -32(a6),d1
        bcc     L2546
        tst.b   -50(a6)
        beq     L251C
        movem.l d0-d3/a0-a1,-(a7)
        movea.l d1,a0
        adda.l  a0,a0
        adda.l  a0,a0
        adda.l  #16,a0
        adda.l  -$2A(a6),a0
        move.l  (a0),d2
        bset    #31,d0
        move.l  d0,(a0)
        lsl.l   #1,d0
        asr.l   #1,d0
        lsl.l   #1,d2
        bcc.s   L2516
        asr.l   #1,d2
        move.l  d2,d4
        sub.l   d0,d4
        bge.s   L24C8
        exg     d0,d2
L24C8   tst.l   d0
        bge.s   L24CE
        moveq   #0,d0
L24CE   tst.l   d2
        blt.s   L2516
        cmp.l   -$1C(a6),d0
        bge.s   L2516
        cmp.l   -$1C(a6),d2
        blt.s   L24E2
        move.l  -$1C(a6),d2
L24E2   sub.l   d0,d2
        add.l   -$36(a6),d0
        neg.l   d1
        add.l   -$3A(a6),d1
        add.l   -32(a6),d1
        subq.l  #1,d1
        move.w  d3,-(a7)
        move.w  d3,-(a7)
        swap    d3
        move.w  d3,-(a7)
        move.w  d3,-(a7)
        movea.l a7,a1
        moveq   #1,d3
        btst    #3,-$23(a6)
        bne.s   L2510
        jsr     L25BE(pc)               ;draw a solid block
        bra.s   L2514

L2510   jsr     L25CA(pc)               ;eor block to screen
L2514   addq.l  #8,a7
L2516   movem.l (a7)+,d0-d3/a0-a1
        rts

L251C   cmp.l   -$1C(a6),d0
        bcc.s   L2546
        swap    d2
        swap    d3
        move.l  d3,d4
        and.l   d2,d4
        btst    #3,-$23(a6)
        bne.s   L2540
        move.l  (a5),d5
        not.l   d2
        and.l   d2,d5
        not.l   d2
        or.l    d4,d5
        move.l  d5,(a5)
        bra.s   L2542

L2540   eor.l   d4,(a5)
L2542   swap    d2
        swap    d3
L2546   rts

;Invert cursor block on screen.

L2548   movem.l d0-d7/a0-a6,-(a7)
        bsr.s   L256A                   ;set up registers for cursor block
        move.l  #$00FF00FF,d6           ;invert red component in pixels
        move.l  d6,d7                   ;both masks are same for odd/even ;ines
        bra.s   L25D0                   ;eor cursor block on screen

;Set up masks for block operation.

L2558   move.l  (a1),d6                 ;block colour mask odd/even lines
        move.w  d6,d7                   ;even line mask
        swap    d7                      ;both words
        move.w  d6,d7                   ;are same
        move.w  (a1),d6                 ;odd line mask
        btst    #0,d1                   ;even line ?
        bne.s   L256A                   ;no
        exg     d6,d7                   ;else swap masks

;Set up registers for block operation. Entry d0-3.w position x/y and size x/y
;Exit a1/a2 top/bottom long word containing block lefthand edge, a3 line increment,
;d3.w block width in long words, a5 is four times d3.w, d4/d5.l left/right edge
;masks of block pixels.

L256A   lsl.w   #hw_scrlb,d1            ;block y position * line length
        movea.l #hw_scrn,a1             ;screen base
        adda.w  d1,a1                   ;raster line of top of block
        lsl.w   #hw_scrlb-1,d3          ;block y size
        movea.w d3,a2
        adda.l  a2,a2                   ;block depth * line length
        move.w  d0,d1                   ;block x position
        lsr.w   #4,d1                   ; / pixels per long word
        lsl.w   #2,d1                   ;bytes per long word
        adda.w  d1,a1                   ;block top left long word
        adda.l  a1,a2                   ;block bottom left long word
        movea.w #hw_scrll,a3            ;line length increment
        lsr.w   #2,d1                   ;long words offset of block within line
        add.w   d0,d2                   ;block x position + block width
        move.w  d2,d3
        subq.w  #1,d3                   ;righthand extent of block
        asr.w   #4,d3                   ; / pixels per long word
        sub.w   d1,d3                   ;block width in long words
        movea.w d3,a5
        adda.w  a5,a5
        adda.w  a5,a5                   ;block width in bytes
        bsr.s   L25AA                   ;form edge mask
        move.l  d5,d4                   ;for lefthand of block
        move.w  d2,d0                   ;extent of block
        bsr.s   L25AA                   ;form edge mask
        not.l   d5                      ;for righthand of block, but
        bne.s   L25A8                   ;in case whole long word
        moveq   #$FF,d5                 ;then all pixels apply
L25A8   rts

;Form block edge mask based on pixel position within long word.

L25AA   moveq   #$FF,d5
        andi.w  #$000F,d0               ;pixel within long word
        lsr.w   d0,d5                   ;displace by pixels
        move.w  d5,d0                   ;preserve
        lsl.l   #8,d5                   ;byte to upper word
        move.w  d0,d5                   ;restore
        lsl.l   #8,d5                   ;copy byte to upper word
        move.b  d0,d5                   ;copy byte lower word
        rts

;Draw a solid block.

L25BE   movem.l d0-d7/a0-a6,-(a7)
        bsr.s   L2558                   ;set up registers
L25C4   lea     L26EC(pc),a6            ;block is to be drawn
        bra.s   L25F4                   ;draw block

;Draw a block by eor'ing.

L25CA   movem.l d0-d7/a0-a6,-(a7)
        bsr.s   L2558                   ;set up registers
L25D0   lea     L26F4(pc),a6            ;block is to be eor'd
        bra.s   L25F4                   ;draw block

;Recolour a window.

L25D6   movem.l d0-d7/a0-a6,-(a7)
        movea.l a1,a4                   ;pointer to parameters
        bsr.s   L256A                   ;set up registers for window
        add.w   d3,d3                   ;width in long words x2
        addq.w  #1,d3                   ;used as dbra counter
        btst    #mc..m256,sv_mcsta(a6)  ;mode 8 ?
        bne.s   L25F0                   ;yes
        lea     L273E(pc),a6            ;mode 4 recolour routine
        bra.s   L25F4
L25F0   lea     L2708(pc),a6            ;mode 8 recolour routine
L25F4   bsr     L26E6                   ;draw block
        movem.l (a7)+,d0-d7/a0-a6
        rts

;Scroll a block.

L25FE   movem.l d0-d7/a0-a6,-(a7)
        bsr     L2558                   ;set up registers
        move.w  18(a7),d2               ;scroll distance
        neg.w   d2                      ;make negative for downwards
        lsl.w   #hw_scrlb,d2            ;greater/equal to screen depth ?
        bvs.s   L25C4                   ;yes, so just draw whole block
        bgt.s   L2630                   ;direction is upwards
        exg     a2,a1                   ;top is bottom and vice versa
        movea.w #-hw_scrll,a3           ;work backwards
        adda.l  a3,a1                   ;point to actual bottom line
        adda.l  a3,a2                   ;point to line past top
        move.w  a1,d0
        sub.w   a2,d0                   ;block depth in bytes
        tst.b   d0                      ;odd number of lines ? [d]
        bne.s   L2626                   ;yes
        exg     d6,d7                   ;swap masks
L2626   lea     0(a1,d2.w),a4           ;pointer to line displaced downwards
        cmpa.l  a2,a4                   ;is it higher than top of block ?
        bls.s   L25C4                   ;yes, so just draw whole block
        bra.s   L2638                   ;else displace and draw as required

L2630   lea     0(a1,d2.w),a4           ;pointer to line displaced upwards
        cmpa.l  a4,a2                   ;is it lower than bottom of block ?
        bls.s   L25C4                   ;yes, so just draw whole block
L2638   lea     L26FC(pc),a6            ;routine to displace a block line
        suba.w  d2,a2                   ;done when here
        bsr     L26E6                   ;scroll block
        adda.w  d2,a2                   ;redress for possible
        bra     L25C4                   ;draw block

;Pan a block.

L2648   movem.l d0-d7/a0-a6,-(a7)
        bsr     L2558                   ;set up registers
        swap    d3                      ;preserve long words in block
        move.w  18(a7),d2               ;pan distance
        neg.w   d2                      ;direction ?
        bgt.s   L268A                   ;is left
        neg.w   d2                      ;else right
        move.w  d2,d3                   ;isolate
        andi.w  #$000F,d3               ;pixels in partial long word
        addi.w  #16,d3                  ;rotate counter
        swap    d3                      ;preserve/restore
        lsr.w   #4,d2                   ;long words to pan
        cmp.w   d3,d2                   ;greater than block width ?
        bhi     L25C4                   ;yes, so just draw whole block
        sub.w   d2,d3                   ;long words in block to pan
        movea.w #-4,a0                  ;work from right to left
        adda.w  a5,a1                   ;start at end of top line
        adda.w  a5,a2                   ;and end of bottom line
        movea.l a1,a4
        lsl.w   #2,d2                   ;bytes to pan
        suba.w  d2,a4                   ;pan from here in line
        move.l  a5,d2                   ;bytes in a block line
        neg.l   d2                      ;to be
        movea.l d2,a5                   ;negative
        exg     d4,d5                   ;swap edge masks
        bra.s   L26AC

L268A   movea.w #4,a0                   ;work from left to right
        move.w  d2,d3                   ;isolate
        andi.w  #$000F,d3               ;pixels in partial long word
        neg.w   d3                      ;pan is left, so rotate accordingly
        addi.w  #16,d3                  ;rotate counter
        swap    d3                      ;preserve/restore
        lsr.w   #4,d2                   ;long words to pan
        cmp.w   d3,d2                   ;greater than block width
        bhi     L25C4                   ;yes, so just draw whole block
        sub.w   d2,d3                   ;long words to pan block
        lsl.w   #2,d2                   ;bytes to pan
        lea     0(a1,d2.w),a4           ;pan from here in line
L26AC   addq.w  #1,d3                   ;pan width in long words
        lea     L2770(pc),a6            ;routine to pan a block line
        bra     L25F4                   ;perform panning

;Perform block operation.

L26B6   move.w  d3,d0                   ;block width in long words
        bmi.s   L26EA                   ;none
        exg     d6,d7                   ;swap odd/even line masks
        move.l  (a1),-(a7)              ;preserve lefthand and
        move.l  0(a1,a5.w),-(a7)        ;righthand long words
        jmp     (a6)                    ;perform block operation

L26C4   subq.w  #4,a1                   ;backtract to righthand edge
L26C6   move.l  (a1),d0                 ;get altered long word
        and.l   d5,d0                   ;apply righthand mask
        move.l  d5,d1
        not.l   d1                      ;invert mask
        and.l   (a7)+,d1                ;apply to original
        or.l    d1,d0                   ;combine both
        move.l  d0,(a1)                 ;set righthand edge pixels
        suba.l  a5,a1                   ;block width
        move.l  (a1),d0                 ;lefthand edge
        and.l   d4,d0                   ;apply lefthand mask
        move.l  d4,d1
        not.l   d1                      ;invert mask
        and.l   (a7)+,d1                ;apply to orginal
        or.l    d1,d0                   ;combine both
        move.l  d0,(a1)                 ;set lefthand edge pixels
        adda.l  a3,a1                   ;increment by line length
L26E6   cmpa.l  a2,a1                   ;block depth done ?
        bne.s   L26B6                   ;no, do next line
L26EA   rts

;Draw block line.

L26EC   move.l  d6,(a1)+                ;set long words
        dbf     d0,L26EC                ;until all done
        bra.s   L26C4                   ;then correct for any edges

;EOR block line.

L26F4   eor.l   d6,(a1)+                ;invert long words
        dbf     d0,L26F4                ;until all done
        bra.s   L26C4                   ;then correct for any edges

;Displace block line.

L26FC   lea     0(a1,d2.w),a4           ;pointer offset by scroll distance
L2700   move.l  (a4)+,(a1)+             ;shift long word
        dbf     d0,L2700                ;until all done
        bra.s   L26C4                   ;then correct for any edges

;Recolour a mode 8 window.

L2708   move.b  (a1),d6                 ;green components
        move.b  1(a1),d7                ;red/blue components
        moveq   #3,d1                   ;four pixels to do
L2710   moveq   #0,d2
        move.b  d6,d2                   ;green components
        lsl.b   #6,d2                   ;isolate required bit
        lsl.w   #1,d2                   ;preserve whilst
        move.b  d7,d2                   ;red/blue components
        lsl.b   #6,d2                   ;isolate required bits
        lsr.w   #6,d2                   ;form offset to
        move.b  0(a4,d2.w),d2           ;get recolour parameter
        roxr.b  #1,d2                   ;blue bit to extend flag
        roxr.b  #1,d7                   ;update blue component
        roxr.b  #1,d2                   ;red bit to extend flag
        roxr.b  #1,d7                   ;update red component
        ror.b   #1,d6                   ;preserve flash bit
        roxr.b  #1,d2                   ;green bit to extend
        roxr.b  #1,d6                   ;update green component
        dbf     d1,L2710                ;until all pixels done
        move.b  d6,(a1)+                ;update green components
        move.b  d7,(a1)+                ;update red/blue components
        dbf     d0,L2708                ;until all words in line done
        bra.s   L26C4                   ;then correct for any edges

;Recolour a mode 4 window.

L273E   move.b  (a1),d6                 ;green components
        move.b  1(a1),d7                ;red components
        moveq   #7,d1                   ;eight pixels to do
L2746   moveq   #0,d2
        move.b  d6,d2                   ;green components
        lsl.b   #7,d2                   ;isolate required bit
        lsl.w   #1,d2                   ;preserve whilst
        move.b  d7,d2                   ;red components
        lsl.b   #7,d2                   ;isolate required bit
        lsr.w   #6,d2                   ;form offset to
        move.b  0(a4,d2.w),d2           ;get recolour parameter
        roxr.b  #2,d2                   ;red bit to extend flag
        roxr.b  #1,d7                   ;update red component
        roxr.b  #1,d2                   ;green bit to extend flag
        roxr.b  #1,d6                   ;update green component
        dbf     d1,L2746                ;until all pixels done
        move.b  d6,(a1)+                ;update green components
        move.b  d7,(a1)+                ;update red components
        dbf     d0,L273E                ;until all words in line done
        bra     L26C4                   ;then correct for any edges

;Pan block line.

L2770   move.w  a5,-(a7)                ;preserve block width in bytes
        move.l  a4,-(a7)                ;preserve pan from pointer
        move.l  d6,-(a7)                ;preserve mask
        adda.l  a1,a5                   ;point to first long word to pan
        move.l  (a5),d2                 ;get it
        and.l   d5,d2                   ;edge mask
        move.l  d5,d1                   ;procure
        not.l   d1                      ;inverse mask
        and.l   d6,d1                   ;paper mask
        or.l    d1,d2                   ;add original
        move.l  d2,(a5)                 ;update edge
        adda.l  a0,a5                   ;to next in line
        swap    d3                      ;get rotate counter
        movep.w 0(a4),d1                ;get green bytes in long word
        movep.w 1(a4),d2                ;get red bytes in long word
        bra.s   L27B4

L2794   adda.l  a0,a4                   ;to next long word in line
L2796   swap    d1
        swap    d2
        movep.w 0(a4),d1                ;get green bytes in long word
        movep.w 1(a4),d2                ;get red bytes in long word
        move.l  d1,d6
        ror.l   d3,d6
        movep.w d6,0(a1)                ;update green bytes in long word
        move.l  d2,d6
        ror.l   d3,d6
        movep.w d6,1(a1)                ;update red bytes in long word
        adda.l  a0,a1
L27B4   subq.w  #1,d0                   ;decrement pan width counter
        bgt.s   L2794
        blt.s   L27BE
        movea.l a7,a4
        bra.s   L2796

L27BE   move.l  (a7)+,d6                ;restore
        movea.l (a7)+,a4                ;restore
        swap    d3                      ;preserve rotate counter
        bra.s   L27CA

L27C6   move.l  d6,(a1)
        adda.l  a0,a1                   ;to next long word
L27CA   cmpa.l  a1,a5                   ;finished line ?
        bne.s   L27C6                   ;no
        movea.w (a7)+,a5
        adda.l  a3,a4
        suba.l  a0,a1
        bra     L26C6

;Set colour mask at (a1).l according to colour byte d1.b

L27D8   movem.l d1-d2,-(a7)
        bsr.s   L281E                   ;form display colour word
        move.w  d2,(a1)                 ;set colour mask for odd lines
        move.w  d2,2(a1)                ; "     "    "    "  even lines
        lsr.b   #3,d1                   ;any contrast colour or stipple ?
        beq.s   L2818                   ;no, operation complete
        bsr.s   L281E                   ;form display colour word
        lsr.b   #3,d1                   ;reduce to stipple data bits
        btst    #0,d1                   ;stipple dots or vertical stripes ?
        beq.s   L27F6                   ;yes
        eor.w   d2,2(a1)                ;set contrast effect for even lines
L27F6   cmpi.b  #1,d1                   ;stipple effect horizontal stripes ?
        beq.s   L2818                   ;yes, operation complete
        btst    #mc..m256,sv_mcsta(a6)  ;mode setting ?
        beq.s   L280A                   ;is mode 4
        andi.w  #$3333,d2               ;mask out every other mode 8 pixel
        bra.s   L280E
L280A   andi.w  #$5555,d2               ;mask out every other mode 4 pixel
L280E   eor.w   d2,(a1)                 ;set contrast effect for odd lines
        tst.b   d1                      ;stipple effect dots ?
        beq.s   L2818                   ;yes, operation complete
        eor.w   d2,2(a1)                ;set contrast effect for even lines
L2818   movem.l (a7)+,d1-d2
        rts

L281E   move.b  d1,d2                   ;colour byte
        andi.w  #$0007,d2               ;mask colour bits 00000GRB
        ror.l   #2,d2
        lsl.w   #7,d2
        rol.l   #2,d2                   ;000000G0 000000RB
        btst    #mc..m256,sv_mcsta(a6)  ;mode setting ?
        beq.s   L2838                   ;is mode 4
        mulu.w  #$55,d2                 ;0G0G0G0G RBRBRBRB
        bra.s   L283E
L2838   lsr.w   #1,d2                   ;0000000G 0000000R
        mulu.w  #$FF,d2                 ;GGGGGGGG RRRRRRRR
L283E   rts

;Print d2.b to screen at position x/y d0-d1.w (d3.b sd.cattr, a1 sd.smask, a2-3 font)

L2840   movem.l d0-d7/a0-a6,-(a7)
        movem.l (a1),d6-d7              ;strip/ink masks
        btst    #0,d1                   ;y position on odd line ?
        bne.s   L2852                   ;yes
        swap    d6                      ;reverse strip mask
        swap    d7                      ;reverse ink mask
L2852   andi.w  #$00FF,d2               ;limit to byte
        movea.l a2,a4                   ;font #1 address
        sub.b   (a4)+,d2                ;start character
        cmp.b   (a4)+,d2                ;byte in this character set ?
        bls.s   L286A                   ;yes
        add.b   (a2),d2                 ;redress
        movea.l a3,a4                   ;font #2 address
        sub.b   (a4)+,d2                ;start character
        cmp.b   (a4)+,d2                ;byte in this character set ?
        bls.s   L286A                   ;yes
        moveq   #0,d2                   ;else use first as default (blob)
L286A   adda.w  d2,a4                   ;use byte as index
        lsl.w   #3,d2                   ;each entry is
        adda.w  d2,a4                   ;9 bytes
        movea.l #hw_scrn,a1             ;screen base
        lsl.w   #hw_scrlb,d1            ;y position * line length
        adda.w  d1,a1                   ;raster line top of character
        move.w  d0,d1                   ;x position
        lsr.w   #3,d0                   ;pixels per word
        add.w   d0,d0                   ;align on word boundary
        adda.w  d0,a1                   ;offset within line
        andi.w  #$0007,d1               ;pixel within word
        movea.w #$FFFF,a5               ;null line counter value for
        btst    #sd..ulin,d3            ;underline ?
        beq.s   L2892                   ;no, else
        addq.w  #2,a5                   ;last line but one is underlined
L2892   moveq   #0,d0                   ;flag normal width
        moveq   #$7E,d2                 ;(zero upper word also)
        add.w   d2,d2                   ;$00FC, 6 bit character mask
        moveq   #0,d5                   ;no flashing
        btst    #sd..dbwd,d3            ;double width ?
        beq.s   L28B4                   ;no
        moveq   #$FF,d0                 ;flag double width
        move.w  #$FFF0,d2               ;12 bit character mask
        addq.w  #8,d1                   ;adjust bit displacer
        btst    #sd..flsh,d3            ;flash ?
        beq.s   L28B4                   ;no
        move.w  #$4010,d5               ;flash on/off bits bound character
        ror.l   d1,d5                   ;position appropriately
L28B4   btst    #sd..exwd,d3            ;extended width ?
        beq.s   L28BC                   ;no
        asr.b   #4,d2                   ;extend width of character mask
L28BC   moveq   #0,d4                   ;normal height
        btst    #sd..dbht,d3            ;double height ?
        beq.s   L28C6                   ;no
        moveq   #$FF,d4                 ;flag negative
L28C6   ror.l   d1,d2                   ;position and split mask as required
        lea     L2924(pc),a6            ;OVER 0 printing
        btst    #sd..trns,d3            ;transparent background ?
        beq.s   L28E0                   ;no
        lea     L293A(pc),a6            ;OVER 1 printing
        btst    #sd..xor,d3             ;xor character/graphics ?
        beq.s   L28E0                   ;no
        lea     L2936(pc),a6            ;OVER -1 printing
L28E0   movea.l a1,a2                   ;preserve pointer top raster line
        move.b  d2,d0                   ;replicate
        lsl.w   #8,d2                   ;...bytes
        move.b  d0,d2                   ;in word
        lsl.w   #8,d5                   ;align flash bits
        move.w  #9,d0                   ;line depth of character bitmap
        moveq   #0,d3                   ;top line always blank
        bra.s   L291C                   ;print it

L28F2   moveq   #0,d3                   ;character bitmap
        cmp.w   a5,d0                   ;underline required ?
        bne.s   L28FE                   ;no
        moveq   #-1,d3                  ;else whole line (never flash)
        addq.w  #1,a4                   ;skip line in character bitmap
        bra.s   L2914                   ;print it

L28FE   move.b  (a4)+,d3                ;get line from character bitmap
        beq.s   L291C                   ;blank, so just print it
        tst.l   d0                      ;normal width ?
        bge.s   L290C                   ;yes
        lsr.b   #1,d3                   ;else use as offset
        move.w  L297C(pc,d3.w),d3       ;get mode 8 version
L290C   ror.w   d1,d3                   ;align to pixel position
        move.b  d3,d4                   ;replicate
        lsl.w   #8,d3                   ;...bytes
        move.b  d4,d3                   ;in display word
L2914   and.w   d2,d3                   ;mask out relevant bits
        move.w  d3,d4                   ;replicate
        swap    d3                      ;...words
        move.w  d4,d3                   ;in case of double height
L291C   move.w  d3,d4                   ;bits to print
        and.w   d7,d4                   ;apply ink mask
        or.w    d5,d4                   ;apply any flash on/off bit
        jmp     (a6)                    ;print it

;Print as OVER 0

L2924   eor.w   d2,d3                   ;isolate paper bits
        and.w   d6,d3                   ;strip mask
        or.w    d4,d3                   ;add ink bits
        move.w  d2,d4                   ;copy
        not.w   d4                      ;derive mask
        and.w   (a1),d4                 ;get original
        or.w    d3,d4                   ;add new bits
        move.w  d4,(a1)                 ;print character line
        bra.s   L2942

;Print as OVER -1

L2936   eor.w   d4,(a1)                 ;xor character line
        bra.s   L2942

;Print as OVER 1

L293A   not.w   d3                      ;mask to
        and.w   (a1),d3                 ;get original
        or.w    d4,d3                   ;add new bits
        move.w  d3,(a1)                 ;print character line

L2942   swap    d6                      ;reverse strip mask
        swap    d7                      ;reverse ink mask
        adda.w  #hw_scrll,a1            ;to next raster line
        tst.l   d4                      ;double height
        bge.s   L2956                   ;no
        swap    d3                      ;get duplicate line
        bchg    #30,d4                  ;doubling done ?
        bne.s   L291C                   ;no, so print again
L2956   dbf     d0,L28F2                ;until all lines printed
        clr.w   d2                      ;clear part of mask completed
        rol.l   #8,d2                   ;any split across words ?
        beq.s   L2976                   ;no, so printing complete
        clr.w   d5                      ;clear any flash on bit
        rol.l   #8,d5                   ;align any flash off bit
        subq.b  #8,d1                   ;update bit displacer
        andi.b  #$0F,d1                 ;restrict remainder
        movea.l a2,a1                   ;restore to top line
        addq.w  #2,a1                   ;but to next word in line
        suba.w  #9,a4                   ;back to start of character bitmap
        bra     L28E0                   ;print adjacent pixels

L2976   movem.l (a7)+,d0-d7/a0-a6
        rts

;Double width versions of character font bitmap lines.

L297C   dc.w    $0000,$0030,$00C0,$00F0,$0300,$0330,$03C0,$03F0
        dc.w    $0C00,$0C30,$0CC0,$0CF0,$0F00,$0F30,$0FC0,$0FF0
        dc.w    $3000,$3030,$30C0,$30F0,$3300,$3330,$33C0,$33F0
        dc.w    $3C00,$3C30,$3CC0,$3CF0,$3F00,$3F30,$3FC0,$3FF0
        dc.w    $C000,$C030,$C0C0,$C0F0,$C300,$C330,$C3C0,$C3F0
        dc.w    $CC00,$CC30,$CCC0,$CCF0,$CF00,$CF30,$CFC0,$CFF0
        dc.w    $F000,$F030,$F0C0,$F0F0,$F300,$F330,$F3C0,$F3F0
        dc.w    $FC00,$FC30,$FCC0,$FCF0,$FF00,$FF30,$FFC0,$FFF0

;MDV driver SLAVING

L29FC   sf      md_estat(a2)                    ;clear error status
L2A00   moveq   #0,d1
        move.b  md_drivn(a2),d1                 ;drive number
        lea     sv_mdrun(a6),a3                 ;pointer to
        st      sv_mdsta-sv_mdrun-1(a3,d1.w)    ;set status for drive
        tst.b   (a3)                            ;turning microdrive ?
        bne.s   L2A38                           ;yes, return
        moveq   #pc.mdvmd,d0                    ;microdrive mode
        jsr     L0420(pc)                       ;set hardware output mode
        move.b  d1,(a3)                         ;set current microdrive
        move.b  #$FA,sv_mdcnt(a6)               ;-6 to gap counter
        lea     pc_mctrl,a3                     ;microdrive control port
        jsr     L2C56(pc)                       ;turn on MDV motor d1.b
        ori.b   #pc.maskg,sv_pcint(a6)          ;enable gap interrupts
        move.b  sv_pcint(a6),pc_intr            ;copy interrupt register to port
L2A38   rts                                     ;return (wait for gap interrupt)

;Purge slave table of all entries associated with bad medium.

L2A3A   move.w  8(a7),d0                        ;FS drive ID of turning microdrive
        lsl.w   #2,d0                           ;to upper nibble
        addq.w  #1,d0                           ;set bt..file bit
        movea.l sv_btbas(a6),a1                 ;base of slave block table
L2A46   moveq   #$F1,d1                         ;mask out
        and.b   (a1),d1                         ;FS drive ID and bt..file bit
        cmp.b   d1,d0                           ;is it ours ?
        bne.s   L2A52                           ;no
        move.b  #bt.empty,(a1)                  ;else release it
L2A52   addq.w  #8,a1                           ;to next table entry
        cmpa.l  sv_bttop(a6),a1                 ;at top ?
        blt.s   L2A46                           ;no, continue search
        lea     md_pendg(a5),a1                 ;pending operations map
        moveq   #127,d0                         ;all entries
L2A60   clr.l   (a1)+                           ;to be cleared
        dbf     d0,L2A60                        ;until done
        rts

;Medium name mismatch

L2A68   tst.b   md_estat(a5)                    ;drive error status ?
        ble.s   L2A9C                           ;is not 'open pending'
        tst.b   md_files(a5)                    ;any files open ?
        bne.s   L2A9C                           ;yes
        tst.b   d7                              ;sector number read ?
        bne.s   L2A98                           ;was not zero, the drive map
        lea     md_map(a5),a1                   ;pointer to copy of drive map
        dc.w    $4EB9,0,L525C    *** jsr L525C  ;md_read
        bra.s   L2A98                           ;failed
        lea     8(a7),a1                        ;point to new medium name
        lea     md_mname(a5),a2                 ;to update
        move.l  (a1)+,(a2)+                     ;name in physical definition
        move.l  (a1)+,(a2)+                     ;including
        move.l  (a1)+,(a2)+                     ;random number
        bsr.s   L2A3A                           ;purge slave table entries
L2A94   sf      md_estat(a5)                    ;new medium, so clear error
L2A98   bra     L2B84                           ;clear interrupt and return

;Report bad medium

L2A9C   lea     md_mname(a5),a1                 ;medium name string
        tst.b   (a1)                            ;is it unset ?
        beq.s   L2AB2                           ;yes
        suba.l  a0,a0                           ;channel #0
        moveq   #10,d2                          ;string length fixed
        jsr     L39B4(pc)                       ;ut_mtext, send message to channel
        moveq   #err.fe,d0                      ;report file error
        jsr     L3968(pc)                       ;ut_err, error message to channel
L2AB2   bsr.s   L2A3A                           ;purge slave table entries
        st      md_estat(a5)                    ;drive status is error
        bra     L2BE2                           ;exit prematurely

;Gap interrupt handler

L2ABC   movem.l d0-d6/a0-a4,-(a7)
        lea     pc_mctrl,a3                     ;microdrive control port
        moveq   #0,d0
        move.b  sv_mdrun(a6),d0                 ;currently turning microdrive
        beq     L2B88                           ;none, clear interrupt and return
        suba.w  #14,a7                          ;make space for sector header
        movea.l a7,a1                           ;pointer to buffer
        lea     sv_mddid(a6),a5                 ;position pointer to
        sf      7(a5,d0.w)                      ;clear drive status (sv_mdsta)
        move.b  -1(a5,d0.w),d0                  ;get FS drive ID for turning drive
        move.w  d0,-(a7)                        ;preserve it
        movea.l sv_fsdef-sv_mddid(a5,d0.w),a5   ;pointer to physical definition
        move.l  a5,-(a7)                        ;preserve it
        move.b  sv_mdcnt(a6),d2                 ;gap counter
        bge.s   L2AF2                           ;write delay has lapsed
        addq.b  #1,d2                           ;else reduce it
L2AF2   dc.w    $4EB9,0,L523A    *** jsr L523A  ;md_sectr (read header)
L2AF8   bra.s   L2A9C                           ;bad medium, report and tidy up
        bra.s   L2A98                           ;bad header, clear interrupt and return
        moveq   #11,d0                          ;length medium name + random number
        lea     md_files(a5),a2                 ;starting at the end
L2B02   move.b  -(a2),d1                        ;check data in definition with
        cmp.b   -(a1),d1                        ;data from sector read
        bne     L2A68                           ;no match, reset medium settings
        dbf     d0,L2B02                        ;check until complete
        tst.b   md_estat(a5)                    ;error status ?
        bgt.s   L2A94                           ;'open pending', clear it and return
        add.w   d7,d7                           ;sector number
        bne.s   L2B24                           ;was not zero, the drive map
        addq.b  #1,md_fail(a5)                  ;increment failure counter
        cmpi.b  #8,md_fail(a5)                  ;expired ?
        bgt.s   L2AF8                           ;yes, report bad medium and tidy up
L2B24   adda.w  d7,a5                           ;offset pointer into FS definition
        moveq   #0,d1
        move.w  md_pendg(a5),d1                 ;get sector pending operation
        beq     L2BC4                           ;none
        tst.b   d2                              ;gap counter ?
        ble.s   L2B36                           ;still in write delay phase
        sf      d2                              ;else reset it
L2B36   move.b  d2,sv_mdcnt(a6)                 ;update gap counter
        tst.w   d1                              ;pending operation ?
        blt     L2C1C                           ;was write map or verify map
        lsl.l   #3,d1                           ;else slave block number
        movea.l sv_btbas(a6),a4                 ;table base
        adda.l  d1,a4                           ;offset to entry
        move.l  a4,-(a7)                        ;preserve it
        lsl.l   #6,d1                           ;create offset to memory block
        lea     0(a6,d1.l),a1                   ;pointer to memory block
        btst    #bt..wreq,(a4)                  ;was pending operation write ?
        bne.s   L2BA6                           ;yes
        btst    #bt..accs,(a4)                  ;else is block valid ?
        bne.s   L2B66                           ;yes, so verify it
        dc.w    $4EB9,0,L525C    *** jsr L525C  ;md_read
        bra.s   L2B82                           ;failed, clear interrupt and return
        bra.s   L2B6E                           ;read sector OK

L2B66   dc.w    $4EB9,0,L5262    *** jsr L5262  ;md_verify
        bra.s   L2B7E                           ;failed
L2B6E   moveq   #bt.true,d0                     ;status to be 'block is true'
        clr.w   md_pendg(a5)                    ;pending operation completed
        movea.l 4(a7),a1                        ;restore pointer to base FS definition
        sf      md_fail(a1)                     ;clear failure counter
        bra.s   L2BB8                           ;update table entry
L2B7E   moveq   #bt.updt,d0                     ;status to be 'awaiting write'
        bra.s   L2BB8                           ;update table entry

L2B82   addq.w  #4,a7                              ;discard pointer to FS definition
L2B84   adda.w  #20,a7                             ;discard stacked registers/buffer
L2B88   move.b  sv_pcint(a6),d7                    ;interrupt register setting
        ori.b   #pc.intrg,d7                       ;clear gap interrupt
        andi.b  #$DF,d7                            ;disable gap interrupts
        move.b  d7,pc_intr-pc_mctrl(a3)            ;write to hardware port, then
        move.b  sv_pcint(a6),pc_intr-pc_mctrl(a3)  ;restore previous setting
        movem.l (a7)+,d0-d6/a0-a4
        bra     L03B6                              ;restore trap level registers and rte

L2BA6   tst.b   d2                              ;gap counter ?
        bmi.s   L2B82                           ;has not expired write delay yet
        move.w  md_map(a5),-(a7)                ;file/block number of sector
        dc.w    $4EB9,0,L51B0    *** jsr L51B0  ;md_write
        addq.w  #2,a7                           ;discard data
        moveq   #bt.aver,d0                     ;status to be 'awaiting verify'
L2BB8   moveq   #$F0,d1                         ;mask for FS drive ID
        movea.l (a7)+,a4                        ;pointer to slave block table entry
        and.b   (a4),d1                         ;bt_stat
        or.b    d0,d1                           ;new attributes
        move.b  d1,(a4)                         ;update block status
        moveq   #5,d2                           ;pending operation done so renew
L2BC4   tst.b   d2                              ;gap counter ?
        bmi.s   L2C14                           ;has not expired, update and return
        addq.b  #1,d2                           ;else increment
        cmpi.b  #8,d2                           ;if inactivity ceiling not reached then
        blt.s   L2C14                           ;update and return
        moveq   #0,d2                           ;else restart counter
        moveq   #0,d0                           ;zero index
        movea.l (a7),a5                         ;pointer to FS definition
        lea     md_pendg(a5),a5                 ;offset to pending table
L2BDA   tst.w   (a5)+                           ;pending operation ?
        bne.s   L2C14                           ;yes, update up/down counter and return
        addq.b  #1,d0                           ;increment index
        bne.s   L2BDA                           ;until 256 entries searched
L2BE2   dc.w    $4EB9,0,L2C50    *** jsr L2C50  ;turn off all MDV motors
        moveq   #8,d1                           ;starting at MDV8_
        lea     sv_mdsta(a6),a5                 ;check all
L2BEE   tst.b   -1(a5,d1.w)                     ;drive status ?
        bne.s   L2C08                           ;has operations pending
        subq.b  #1,d1                           ;else next drive
        bne.s   L2BEE                           ;until all done
        clr.b   sv_mdrun(a6)                    ;no microdrive turning
        andi.b  #$DF,sv_pcint(a6)               ;disable gap interrupts
        jsr     L0452(pc)                       ;prepare for serial transmit mode
        bra.s   L2C18                           ;clear interrupt and return

L2C08   move.b  d1,sv_mdrun(a6)                 ;currently turning microdrive
        dc.w    $4EB9,0,L2C56    *** jsr L2C56  ;turn on MDV motor d1.b
        moveq   #-6,d2                          ;reinitialise write delay
L2C14   move.b  d2,sv_mdcnt(a6)                 ;update gap conuter
L2C18   bra     L2B84                           ;clear interrupt and return

;Perform pending operation write map or verify map.

L2C1C   lea     md_map(a5),a1                   ;pointer to map
        addq.w  #1,d1                           ;verify map ?
        bne.s   L2C3A                           ;yes
        tst.b   d2                              ;gap counter ?
        bmi     L2B84                           ;has not expired write delay yet
        move.w  #$8000,-(a7)                    ;file/block number of sector
        dc.w    $4EB9,0,L51B0    *** jsr L51B0  ;md_write
        addq.w  #2,a7                           ;discard data
        moveq   #-2,d1                          ;pending operation is now verify
        bra.s   L2C48                           ;update entry
L2C3A   dc.w    $4EB9,0,L5262    *** jsr L5262  ;md_verify
        bra.s   L2C46                           ;failed, try write map again
        moveq   #0,d1                           ;pending operation completed
        bra.s   L2C48                           ;update entry
L2C46   moveq   #-1,d1                          ;pending operation is write map
L2C48   move.w  d1,md_pendg(a5)                 ;update entry
        bra     L2B84                           ;clear interrupt and return

;Turn off all MDV motors (MD.DESEL)

L2C50   moveq   #pc.desel,d2            ;no select bit
        moveq   #7,d1                   ;clear all
        bra.s   L2C5A

;Turn on MDV motor d1.b (MD.SELECT)

L2C56   moveq   #pc.selec,d2            ;select bit turns motor on
        subq.w  #1,d1                   ;decrement drive number (dbra)
L2C5A   move.b  d2,(a3)                 ;write to  control port
        moveq   #$39,d0                 ;fixed timing dummy
        ror.l   d0,d0                   ;   "     "     "
        bclr    #pc..sclk,d2            ;clock low
        move.b  d2,(a3)                 ;write to control port
        moveq   #$39,d0                 ;fixed timing dummy
        ror.l   d0,d0                   ;   "     "     "
        moveq   #pc.desel,d2            ;deselect other drives
        dbf     d1,L2C5A                ;till select bit at required motor
        rts

;Perform IPC dialogue (a3 points to command)

L2C72   move.w  sr,-(a7)                ;(rte used in place of rts)
        ori.w   #$0700,sr               ;interrupts off
        jsr     L2F6E(pc)               ;assign IPC hardware registers
        move.b  (a3)+,d0                ;get command byte
        bsr.s   L2CC8                   ;send it
        move.b  (a3)+,d7                ;number of parameters
        move.l  (a3)+,d4                ;parameter definition bits
L2C84   subq.b  #1,d7                   ;last parameter ?
        blt.s   L2CA4                   ;yes
        move.b  (a3)+,d0                ;get parameter
        btst    #0,d4                   ;anything to be sent ?
        bne.s   L2CA0                   ;no
        btst    #1,d4                   ;size 4 or 8 bits ?
        beq.s   L2C9E                   ;4 bits
        move.w  d0,d5                   ;preserve while
        ror.w   #4,d0                   ;upper nibble first
        bsr.s   L2CC8                   ;sent to IPC
        move.w  d5,d0                   ;lower nibble
L2C9E   bsr.s   L2CC8                   ;sent to IPC
L2CA0   ror.l   #2,d4                   ;parameter definition bits for
        bra.s   L2C84                   ;next parameter

L2CA4   move.b  (a3)+,d4                ;size of reply
        btst    #0,d4                   ;anything expected ?
        bne.s   L2CBC                   ;no, operation complete
        btst    #1,d4                   ;size 4 or 8 bits ?
        bne.s   L2CB8                   ;8 bits
        jsr     L2F96(pc)               ;get 4 bit reply from IPC into d1.b
        bra.s   L2CBC
L2CB8   jsr     L2F9A(pc)               ;get 8 bit reply from IPC into d1.b
L2CBC   moveq   #pc.intri,d7            ;clear interface interrupt
        or.b    sv_pcint(a6),d7         ;include current status then
        move.b  d7,pc_intr-pc_ipcrd(a0) ;copy to interrupt register port
        rte                             ;(restore stacked SR and do rts)

L2CC8   jmp     L2F7C(pc)               ;send d0.b to IPC

;Interface interrupt handler

L2CCC   movem.l d0-d6/a0-a4,-(a7)
        moveq   #0,d3                   ;this is not a poll call (so no polls missed!)
        bsr.s   L2D20                   ;perform interface tasks
        moveq   #pc.intri,d7            ;interface interrupt to be cleared
        bra.s   L2CE6                   ;return from interrupt

;Transmit interrupt handler

L2CD8   movem.l d0-d6/a0-a4,-(a7)
        moveq   #0,d3                   ;this is not a poll call
        st      d4                      ;flag call is from interrupt
        bsr     L2D74                   ;service any pending serial transmission
        moveq   #pc.intrt,d7            ;clear transmit interrupt
L2CE6   or.b    sv_pcint(a6),d7         ;add interrupt enable mask bit
        move.b  d7,pc_intr              ;and write it to hardware register port
        movem.l (a7)+,d0-d6/a0-a4
        bra     L03B6                   ;restore interrupt level registers and rte

;First linkage in list of internal polled tasks.

L2CF8   dc.l    0                       ;end of list
        dc.l    L2D08                   ;interface tasks (keyboard, SER)

;Second linkage in list of internal scheduler tasks

L2D00   dc.l    L3480                   ;next in list (waiting I/O)
        dc.l    L2D14                   ;transmit

L2D08   move.w  sr,-(a7)                ;(rte used in place of rts)
        ori.w   #$0700,sr               ;interrupts off
        bsr.s   L2D20                   ;perform interface tasks
        jmp     L2CBC(pc)               ;return from interrupt

L2D14   move.w  sr,-(a7)                ;(rte used in place of rts)
        ori.w   #$0700,sr               ;interrupts off
        sf      d4                      ;flag call is from scheduler
        bsr.s   L2D74                   ;service any pending serial transmission
        rte                             ;(restore stacked SR and do rts)

;Perform IPC interface tasks (read status, keyboard and any serial input).

L2D20   jsr     L2F6E(pc)               ;assign IPC hardware registers
        moveq   #stat_cmd,d0            ;IPC command report input status
        jsr     L2F7C(pc)               ;send d0.b to IPC
        jsr     L2F9A(pc)               ;get 8 bit reply from IPC into d1.b
        move.b  d1,d7                   ;(preserve status during any routine)
        btst    #6,d7                   ;IPC input P2.6 ? (unused MDV write protect)
        seq     sv_wp(a6)               ;set system variable accordingly
        btst    #1,d7                   ;sound being produced ?
        sne     sv_sound(a6)            ;set system variable if yes
        btst    #0,d7                   ;key being pressed ?
        beq.s   L2D4A                   ;no
        jsr     L2E58(pc)               ;else deal with it
L2D4A   move.l  sv_ser1c(a6),d0         ;SER1 receive queue ?
        beq.s   L2D5E                   ;there isn't one
        btst    #4,d7                   ;IPC SER1 still open ?
        beq.s   L2D5E                   ;no
        movea.l d0,a2                   ;input queue location
        moveq   #rds1_cmd,d5            ;IPC command read SER1
        jsr     L2E18(pc)               ;fetch any bytes and put in queue
L2D5E   move.l  sv_ser2c(a6),d0         ;SER2 receive queue
        beq.s   L2D72                   ;there isn't one
        btst    #5,d7                   ;IPC SER2 still open ?
        beq.s   L2D72                   ;no
        movea.l d0,a2                   ;input queue location
        moveq   #rds2_cmd,d5            ;IPC command read SER2
        jsr     L2E18(pc)               ;fetch any bytes and put in queue
L2D72   rts

;Deal with any pending serial transmission.

L2D74   lea     sv_tmode(a6),a4             ;current transmit mode
        btst    #pc..serb,(a4)              ;serial mode ?
        bne.s   L2D88                       ;no (effectively bne.s L2E02)
        lea     pc_mctrl,a1                 ;microdrive control port
        btst    #pc..txfl,(a1)              ;transmit buffer full ?
L2D88   bne.s   L2E02                       ;yes, so try again later
        moveq   #0,d6
        move.b  (a4),d6                     ;get current transmit mode
        lsl.b   #4,d6                       ;isolate
        lsr.b   #7,d6                       ;current serial port number bit
        move.w  d6,d7                       ;then some manipulations yield
        addq.b  #4,d7                       ;bit set for checking pc..dtr1 or pc..cts2
        lsl.b   #2,d6                       ;and offset to either
        lea     sv_ser1c(a6),a5             ;SER1 receive queue pointer
        adda.w  d6,a5                       ;or SER2
        move.l  (a5),d0                     ;is there an active queue ?
        beq.s   L2E04                       ;no, check for next port
        movea.w #ser_txq-ser_rxq,a2         ;else offset forms
        adda.l  d0,a2                       ;pointer to transmit queue header
        tst.b   ser_txhs+1-ser_txq(a2)      ;transmit handshake ?
        beq.s   L2DB2                       ;is not required
        btst    d7,(a1)                     ;DTR SER1 or CTS SER2 ?
        bne.s   L2DBE                       ;are busy
L2DB2   jsr     L385E(pc)                   ;io.qout, read a byte from queue
        beq.s   L2DF6                       ;send data to serial port
        addq.l  #1,d0                       ;else was queue empty ?
        beq.s   L2E04                       ;yes, check for next port
        bra.s   L2DD4                       ;else at EOF
L2DBE   jsr     L380A(pc)                   ;io.qtest, test queue status
        cmpi.w  #err.ef,d0                  ;at EOF ?
        bne.s   L2E04                       ;no, check for next port
        tst.b   ser_prot+1-ser_txq(a2)      ;protocol ?
        blt.s   L2DD4                       ;is raw
        tst.b   ser_txhs+1-ser_txq(a2)      ;handshake ?
        bne.s   L2E02                       ;is required so return for now
L2DD4   tst.b   d4                          ;routine was called from ?
        bne.s   L2E02                       ;transmit interrupt so return immediately
        move.b  ser_prot+1-ser_txq(a2),d6   ;get protocol setting
        lea     -ser_txq(a2),a0             ;base of channel definition
        jsr     L305E(pc)                   ;mm_rechp, reclaim memory
        clr.l   (a5)                        ;clear receive queue pointer (sv_serNc)
        tst.b   d6                          ;protocol ?
        blt.s   L2E02                       ;was raw so return done
        moveq   #$1A,d1                     ;else CTRL Z
        btst    #0,ser_par+1-ser_txq(a2)    ;parity requirement ?
        beq.s   L2DF6                       ;was none
        moveq   #$9A,d1                     ;else parity version of CTRL Z
L2DF6   move.b  d1,pc_tdata                 ;send data byte to serial port
        move.w  sv_timov(a6),sv_timo(a6)    ;reset timeout for serial mode
L2E02   rts

;Switch between serial ports when transmission complete on current port.
                                           
L2E04   sub.w   d3,sv_timo(a6)              ;subtract polls from serial timeout
        bge.s   L2E02                       ;return if not out of time
        clr.w   sv_timo(a6)                 ;else reset counter
        bchg    #pc..sern,(a4)              ;toggle serial port number
        move.b  (a4),pc_tctrl-pc_mctrl(a1)  ;to be serviced next call
        rts

;Fetch any bytes from IPC and put in appropriate serial receive queue.

L2E18   jsr     L380A(pc)                   ;io.qtest, test queue status
        cmpi.w  #25,d2                      ;enough free space ?
        blt.s   L2E56                       ;no, return
        move.l  d5,d0                       ;IPC command to read serial
        jsr     L2F7C(pc)                   ;send d0.b to IPC
        jsr     L2F9A(pc)                   ;get 8 bit reply from IPC into d1.b
        move.b  d1,d4                       ;bytes to fetch from IPC
        andi.w  #$003F,d4                   ;limit (and make dbra word)
        beq.s   L2E56                       ;there are none
        subq.w  #1,d4                       ;(adjust for dbra)
L2E36   jsr     L2F9A(pc)                   ;get 8 bit reply from IPC into d1.b
        tst.b   ser_prot-ser_rxq(a2)        ;channel protocol
        blt.s   L2E4E                       ;is raw
        moveq   #$7F,d0                     ;mask out
        and.b   d1,d0                       ;7 bit data
        cmpi.b  #$1A,d0                     ;is it CTRL Z
        bne.s   L2E4E                       ;no
        jsr     L3888(pc)                   ;io.qeof, put EOF in queue
L2E4E   jsr     L3838(pc)                   ;io.qin, place byte in queue
        dbf     d4,L2E36                    ;until all buffered bytes read
L2E56   rts

;Deal with any keyboard input from IPC.

L2E58   movea.l sv_keyq(a6),a2              ;current keyboard queue
        move.l  a2,d0                       ;if none then
        beq.s   L2EC0                       ;ignore keyboard until required
        moveq   #rdkb_cmd,d0                ;IPC command read keyboard
        jsr     L2F7C(pc)                   ;send d0.b to IPC
        jsr     L2F96(pc)                   ;get 4 bit reply from IPC into d1.b
        move.b  d1,d5                       ;(b3=repeat key, b2-0 number of keys)
        move.b  d1,d4                       ;key counter
        andi.w  #$0007,d4                   ;mask out number of keys buffered
        beq.s   L2E96                       ;none
        subq.w  #1,d4                       ;(adjust for dbra)
L2E76   clr.w   sv_arbuf(a6)                ;clear last key pressed
        jsr     L2F96(pc)                   ;get 4 bit reply from IPC into d1.b
        move.b  d1,d2                       ;(b4=overflow, b2=SHIFT, b1=CTRL, b0=ALT)
        jsr     L2F9A(pc)                   ;get 8 bit reply from IPC into d1.b (matrix key)
        movea.l MBFF2,a3                    ;keyboard translation routine
        jsr     (a3)                        ;returns multiple outcome based on d1 key code
        bra.s   L2EC4                       ;CTRL+SPACE space key
        bra.s   L2EC2                       ;aborted key codes (special accents)
        bsr.s   L2EEC                       ;
        dbf     d4,L2E76                    ;until all buffered keys are dealt with
L2E96   btst    #3,d5                       ;repeat key ?
        beq.s   L2EBA                       ;no, reset key delay and return
        sub.w   d3,sv_arcnt(a6)             ;else decrement polls from key repeat counter
        bgt.s   L2EC0                       ;not expired so return immediately
        jsr     L380A(pc)                   ;io.qtest, test keyboard queue status
        tst.l   d0                          ;is there anything in queue ?
        beq.s   L2EB2                       ;yes, set key frequency and return
        move.w  sv_arbuf(a6),d1             ;else get last key pressed
        beq.s   L2EB2                       ;there wasn't one
        bsr.s   L2EEC                       ;else repeat its action
L2EB2   move.w  sv_arfrq(a6),sv_arcnt(a6)   ;key repeat frequency to key repeat counter
        rts
L2EBA   move.w  sv_ardel(a6),sv_arcnt(a6)   ;key repeat delay to key repeat counter
L2EC0   rts
L2EC2   rts                                 ;(remnant of aborted special key routine)

;Key read from IPC was CTRL+SPACE

L2EC4   sf      sv_scrst(a6)                ;make screen active (if frozen CTRL F5)
        movea.l sv_jbbas(a6),a3             ;base of job table
        movea.l (a3),a3                     ;header of job 0 BASIC
        sf      bv_brk+jb_end(a3)           ;tell BASIC there has been a break
        move.w  jb_stat(a3),d0              ;job status
        beq.s   L2EEA                       ;was active so return
        addq.w  #1,d0                       ;was it waiting for another job ?
        blt.s   L2EEA                       ;yes, so return
        clr.w   jb_stat(a3)                 ;else suspended (I/O) so make active
        move.l  jb_hold(a3),d0              ;location to clear on release ?
        beq.s   L2EEA                       ;no
        movea.l d0,a3                       ;else
        sf      (a3)                        ;clear it
L2EEA   rts

;Take action based on key read from IPC.

L2EEC   cmpi.w  #$F9,d1                     ;CTRL F5 ?
        beq.s   L2F3A                       ;yes, toggle activity status of screen
        sf      sv_scrst(a6)                ;else becomes active (if not already)
        cmp.w   sv_cqch(a6),d1              ;change keyboard queue key ?
        beq.s   L2F40                       ;yes
        cmpi.w  #$E0,d1                     ;Capslock key ?
        bne.s   L2F12                       ;no
        not.b   sv_caps(a6)                 ;toggle capslock status
        tst.l   sv_csub(a6)                 ;is there a capslock subroutine ?
        beq.s   L2EC0                       ;no, return
        lea     sv_csub(a6),a5              ;else get address
        jmp     (a5)                        ;call it and return

;Put passive (non-system) key into current jobs keyboard queue.

L2F12   move.w  sv_ardel(a6),sv_arcnt(a6)   ;reset key repeat counter
        move.w  d1,sv_arbuf(a6)             ;update last key pressed
        cmpi.b  #$FF,d1                     ;ALT combination key ?
        bne.s   L2F36                       ;no
        swap    d1                          ;preserve ALT & key
        jsr     L380A(pc)                   ;io.qtest, test queue status
        cmpi.w  #2,d2                       ;sufficient space ?
        blt.s   L2EC0                       ;no, forget it and return
        swap    d1                          ;else restore, enough space to
        jsr     L3838(pc)                   ;io.qin, place ALT in keyboard queue
        lsr.w   #8,d1                       ;followed by combination key
L2F36   jmp     L3838(pc)                   ;io.qin, put key in keyboard queue

L2F3A   not.b   sv_scrst(a6)                ;toggle screen activity status
        rts

;Action on keyboard queue change key.

L2F40   lea     -sd_kbd(a2),a0                  ;base of channel definition
        tst.b   sd_curf(a0)                     ;cursor status ?
        bge.s   L2F54                           ;visible or suppressed
        dc.w    $4EB9,0,L1B86    *** jsr L1B86  ;sd.cure, enable the curosr
        lea     sd_kbd(a0),a2                   ;keyboard queue header (never was smashed)
L2F54   movea.l (a2),a2                         ;get link to next queue
        cmpa.l  sv_keyq(a6),a2                  ;current keyboard queue ?
        beq.s   L2F6A                           ;yes (only one job using keyboard)
        tst.b   sd_curf-sd_kbd(a2)              ;cursor status ?
        beq.s   L2F54                           ;suppressed, try next queue
        move.l  a2,sv_keyq(a6)                  ;this is now the current keyboard queue
        clr.w   sv_fstat(a6)                    ;clear cursor flash counter
L2F6A   jmp     L2F6E(pc)                       ;(jumping without moving)

;Assign IPC hardware registers

L2F6E   lea     pc_ipcwr,a1                     ;IPC write port
        lea     pc_ipcrd-pc_ipcwr(a1),a0        ;IPC read port
        moveq   #pc..ipch,d6                    ;handshake bit of read port
        rts

;Send data d0.b to IPC

L2F7C   lsl.b   #4,d0                   ;left align data nibble
        ori.b   #$08,d0                 ;bit acts as end-stop
L2F82   lsl.b   #1,d0                   ;data bit to eXtend bit
        beq.s   L2F94                   ;done
        moveq   #$03,d1                 ;magic number
        roxl.b  #1,d1                   ;eXtend bit to b0
        asl.b   #1,d1                   ;(why not roxl.b #2,d1 ?)
        move.b  d1,(a1)                 ;data 1=$0E or 0=$0C
L2F8E   btst    d6,(a0)                 ;handshake ?
        bne.s   L2F8E                   ;wait for acknowledgment
        bra.s   L2F82                   ;next bit
L2F94   rts

;Read reply from IPC into d1.b

L2F96   moveq   #$10,d1                 ;4 bit reply (end-stop bit)
        bra.s   L2F9C
L2F9A   moveq   #$01,d1                 ;8 bit reply (end-stop bit)
L2F9C   move.b  #$0E,(a1)               ;magic number
L2FA0   btst    d6,(a0)                 ;handshake ?
        bne.s   L2FA0                   ;wait for acknowledgment
        move.b  (a0),d0                 ;read port, data is bit 7
        roxl.b  #1,d0                   ;data bit reply to eXtend bit
        roxl.b  #1,d1                   ;accumulate reply
        bcc.s   L2F9C                   ;next bit
        rts

;Vector $C0 MM_ALCHP - Allocate common heap

L2FAE   lea     sv_chpfr(a6),a0
        moveq   #15,d2                  ;rounding factor
        jsr     L3106(pc)               ;mm_alloc
        blt.s   L2FD6
        lea     sv_cheap(a6),a2
        cmpa.l  a1,a2
        beq.s   L2FC6
        clr.l   8(a1)
L2FC6   move.l  d1,d0
        addq.w  #4,a0
        subq.l  #4,d0
L2FCC   clr.l   (a0)+
        subq.l  #4,d0
        bgt.s   L2FCC
        suba.l  d1,a0
        bra.s   L2FF8

L2FD6   move.l  d1,-(a7)
        adda.l  (a0),a1
        cmpa.l  sv_free(a6),a1
        bne.s   L2FE2
        sub.l   (a0),d1
L2FE2   jsr     L324E(pc)
        bne.s   L2FF6
        suba.l  d1,a0
        lea     sv_chpfr(a6),a1         ;pointer to pointer of free space
        jsr     L315E(pc)               ;link in d1.l free space at a0
        move.l  (a7)+,d1
        bra.s   L2FAE                   ;mm_alchp

L2FF6   addq.w  #4,a7
L2FF8   rts

;Get d1.l space in transient program area (also used for RESPR area when TRNSP empty).

L2FFA   lea     sv_trnfr(a6),a0         ;pointer to pointer to free space
        moveq   #15,d2                  ;rounding factor
        moveq   #1,d0                   ;flag find last suitable block in list
        jsr     L3134(pc)               ;find space in list
        tst.l   d2                      ;big enough block found ?
        bgt.s   L3032                   ;yes, unlink it from free space list
        move.l  d1,-(a7)                ;else preserve size requested
        lea     sv_trnsp(a6),a1         ;(pseudo) address of first link
        adda.l  hp_next(a1),a1          ; + offset to point to first free block
        cmpa.l  sv_trnsp(a6),a1         ;any free space at start of TRNSP ?
        bne.s   L301C                   ;no
        sub.l   (a1),d1                 ;else reduce requirement
L301C   jsr     L31B8(pc)               ;make space above BASIC
        bne.s   L305A                   ;unsuccessful
        sub.l   d1,sv_trnsp(a6)         ;expand area downwards by allotted space
        lea     sv_trnfr(a6),a1         ;pointer to pointer to free space
        jsr     L315E(pc)               ;link in d1.l free space at a0
        move.l  (a7)+,d1                ;restore original size requested
        bra.s   L2FFA                   ;this time it should be found

;Block of suitable size found, unlink it from free space list

L3032   movea.l a2,a0                   ;pointer to block before suitable block
        adda.l  hp_next(a2),a0          ;form pointer to suitable block
        move.l  (a0),d3                 ;get size of block
        cmp.l   d1,d3                   ;is it greater than requested size ?
        bgt.s   L304E                   ;yes, link in surplus to free space
        move.l  hp_next(a0),d3          ;get offset to next block in list
        beq.s   L3048                   ;block is at end of list
        add.l   a0,d3                   ;pointer to next block
        sub.l   a2,d3                   ;less pointer to previous block
L3048   move.l  d3,hp_next(a2)          ;update previous blocks link
        bra.s   L3056                   ;which releases our block from list

;Block is bigger than required so link in surplus to free space list

L304E   sub.l   d1,d3                   ;surplus above required size
        move.l  d3,(a0)                 ;is now a free space block in itself
        adda.l  d3,a0                   ;offset pointer to allocated block space
        move.l  d1,(a0)                 ;set block size
L3056   moveq   #err.ok,d0              ;return a0 pointing to allocation
        rts

L305A   addq.w  #4,a7                   ;discard original space request
        rts

;Vector $C2 MM_RECHP - Release common heap

L305E   move.l  (a0),d1
        lea     sv_chpfr(a6),a1         ;pointer to pointer of free space
        jsr     L315E(pc)               ;link in d1.l free space at a0
        move.l  (a1),d2
        adda.l  d2,a1
        cmpa.l  sv_free(a6),a1
        bne.s   L3088
        move.l  d2,d1
        jsr     L3282(pc)
        sub.l   d1,d2
        beq.s   L3084
        adda.l  4(a2),a2
        move.l  d2,(a2)
        bra.s   L3088

L3084   clr.l   4(a2)
L3088   moveq   #err.ok,d0
        rts

;Release at a0 in transient program area

L308C   move.l  (a0),d1                 ;jb_len, length of job area
L308E   lea     sv_trnfr(a6),a1         ;pointer to pointer of free space
        jsr     L315E(pc)               ;link in d1.l free space at a0
        lea     sv_trnsp(a6),a2         ;(psuedo) address of first link
        adda.l  hp_next(a2),a2          ; + offset to point to first free block
        cmpa.l  sv_trnsp(a6),a2         ;is it at the start of TRNSP area ?
        bne.s   L30DE                   ;no, operation complete
        move.l  hp_next(a2),-(a7)       ;preserve offset to next
        move.l  (a2),d2                 ;length of first free block
        move.l  d2,d1                   ;for the system to
        jsr     L31C8(pc)               ;reclaim memory above BASIC
        add.l   d1,sv_trnsp(a6)         ;update new start of TRNSP area
        sub.l   d1,d2                   ;was reclaimed space same as first block ?
        beq.s   L30D2                   ;yes, finish by updating free space pointer
        movea.l (a7)+,a1                ;offset to next free block
        adda.l  d1,a2                   ;skip over area reclaimed
        add.l   d1,sv_trnfr(a6)         ;and free space reclaimed in first block
        move.l  d2,(a2)                 ;the remainder is now length of block
        clr.l   hp_owner(a2)            ;free space owner if job 0
        move.l  a1,hp_next(a2)          ;offset to next in list
        beq.s   L30DE                   ;list is empty
        sub.l   d1,hp_next(a2)          ;otherwise is reduced accordingly
        bra.s   L30DE                   ;operation complete

L30D2   move.l  (a7)+,d2                ;offset to what was next free space
        beq.s   L30DA                   ;list was empty
        add.l   sv_trnfr(a6),d2         ;else offset is changed accordingly
L30DA   move.l  d2,sv_trnfr(a6)         ;set new pointer to free space
L30DE   rts

;Trap #1 D0=$06 MT.FREE - Find amount of free space

L30E0   moveq   #1,d0                   ;last in list
        moveq   #0,d1                   ;of size 0
        lea     sv_trnfr(a6),a0         ;offset to free space
        jsr     L3134(pc)               ;find space in list
        movea.l sv_basic(a6),a0         ;base of BASIC area
        suba.w  #512,a0                 ;less a bit
        suba.l  sv_free(a6),a0          ;unassigned free space
        move.l  a0,d1
        cmp.l   d1,d2                   ;is it greater than TRNSP free space ?
        ble.s   L3100                   ;yes, so return that amount
        move.l  d2,d1                   ;else return TRNSP free space
L3100   bra     L03A4                   ;return from trap err.ok

;Vector $D8 MM_ALLOC - Allocate heap space

L3104   moveq   #7,d2                   ;rounding factor
L3106   moveq   #0,d0                   ;first in list
        jsr     L3134(pc)               ;find space in list
        cmp.l   d1,d2
        beq.s   L3122
        bgt.s   L3116
        moveq   #err.om,d0
        rts

L3116   add.l   d1,4(a1)
        movea.l a0,a1
        adda.l  d1,a1
        sub.l   d1,d3
        move.l  d3,(a1)
L3122   move.l  4(a0),d2
        beq.s   L312C
        add.l   a0,d2
        sub.l   a1,d2
L312C   move.l  d2,4(a1)
        move.l  d1,(a0)
        rts

;Each block in a free space list has a header of two long words. The first is the
;length of the block, the second is an offset from the start of the block to the
;next block in the list. If the second long word is zero then it is the last block
;in the list. The system variables sv_cheap/sv_chpfr and sv_trnsp/sv_trnfr
;are so arranged to constitute the first 'block' in their respective lists.

;Search free space list a0 for suitable block of size d1.l (rounded up with d2.l).
;If d0.b is 0 then find first suitable block in list, else find last. Return a2
;pointing to block before suitable block and d2.l as largest block encountered.

L3134   subq.w  #hp_next,a0             ;pointer to start of block (hp_len)
        add.l   d2,d1                   ;round up requested amount
        not.b   d2                      ;by adding and masking using
        and.b   d2,d1                   ;the supplied minimum multiple-1
        moveq   #0,d2                   ;size of largest suitable block found
        movea.l d2,a2
L3140   movea.l a0,a1                   ;pointer to block before prospective block
        move.l  hp_next(a0),d3          ;offset to next free block
        beq.s   L315C                   ;end of list
        adda.l  d3,a0                   ;form pointer to next block
        move.l  (a0),d3                 ;length of block
        cmp.l   d1,d3                   ;is this block big enough ?
        blt.s   L3140                   ;no, look at next
        movea.l a1,a2                   ;pointer to block before suitable block
        cmp.l   d2,d3                   ;largest suitable block found so far ?
        ble.s   L3158                   ;no
        move.l  d3,d2                   ;this is now the largest
L3158   tst.b   d0                      ;allocation to be at end of list ?
        bne.s   L3140                   ;yes, so keep looking
L315C   rts

L315E   clr.l   hp_owner(a0)            ;owner is job 0

;Vector $DA MM_LNKFR - Link free space into heap

L3162   subq.w  #4,a1
        suba.l  a2,a2
L3166   movea.l a2,a3
        movea.l a1,a2
        move.l  4(a1),d2
        beq.s   L317A
        adda.l  d2,a1
        cmpa.l  a0,a1
        ble.s   L3166
        suba.l  a0,a1
        bra.s   L317C

L317A   movea.l d2,a1
L317C   move.l  a0,d2
        sub.l   a2,d2
        move.l  d2,4(a2)
        move.l  d1,(a0)+
        move.l  a1,(a0)
L3188   movea.l a2,a1
        movea.l a3,a2
L318C   move.l  4(a1),d2
        beq.s   L31B6
        movea.l a2,a3
        movea.l a1,a2
        adda.l  d2,a1
        move.l  a3,d2
        beq.s   L318C
        move.l  (a2),d2
        add.l   a2,d2
        cmp.l   a1,d2
        bne.s   L318C
        move.l  4(a1),d2
        beq.s   L31AC
        add.l   (a2),d2
L31AC   move.l  d2,4(a2)
        move.l  (a1),d2
        add.l   d2,(a2)
        bra.s   L3188

L31B6   rts

;Make space d1.l above BASIC.

L31B8   jsr     L323A(pc)               ;release appropriate memory blocks
        bne.s   L31DE                   ;return if error
        bsr.s   L31E0                   ;update BASICs registers
L31C0   move.l  (a1)+,(a0)+             ;shift BASIC area downwards
        subq.l  #4,d0
        bne.s   L31C0                   ;until done
        bra.s   L31DE                   ;return complete (d0=0 err.ok)

;Return space d1.l to system and move BASIC up.

L31C8   jsr     L3276(pc)               ;return appropriate memory blocks
        neg.l   d1                      ;movement is upwards
        beq.s   L31DE                   ;but may be zero
        bsr.s   L31E0                   ;update BASICs registers
        neg.l   d1                      ;(return d1 as space moved)
        adda.l  d0,a0                   ;start at end of new area
        adda.l  d0,a1                   ;and end of BASIC area
L31D8   move.l  -(a1),-(a0)             ;shift BASIC area upwards
        subq.l  #4,d0
        bne.s   L31D8                   ;until done
L31DE   rts                             ;(d0=0 err.ok)

L31E0   movea.l sv_jbbas(a6),a3         ;base of job table
        move.l  a0,(a3)                 ;job 0 header is now here (BASIC)
        adda.l  d1,a0                   ;it was here
        sub.l   d1,jb_a6(a0)            ;redress BASIC base pointer
        sub.l   d1,jb_a7(a0)            ;redress BASIC stack pointer
        movea.l a0,a1                   ;BASIC area to be moved from here
        suba.l  d1,a0                   ;to new position here
        move.l  sv_trnsp(a6),d0         ;top of BASIC area
        sub.l   a1,d0                   ;size of BASIC area
        cmpa.l  sv_jbpnt(a6),a3         ;is BASIC the current job ?
        bne.s   L320A                   ;no, return
        sub.l   d1,trlv_a6(a5)          ;update trap level register A6
        move    usp,a3
        suba.l  d1,a3                   ;and calling job stack pointer
        move    a3,usp                  ;to account for movement
L320A   rts

L320C   addi.l  #511,d1                 ;round up
L3212   andi.w  #$FE00,d1               ;to nearest multiple of 512
        rts

L3218   movea.l sv_basic(a6),a0         ;start of BASIC area
        moveq   #-8,d3                  ;(free each table entry downwards)
        bra.s   L3226
L3220   movea.l sv_free(a6),a0          ;start of free area
        moveq   #8,d3                   ;(free each table entry upwards)
L3226   move.l  a0,d0                   ;from base of area
        sub.l   a6,d0                   ;down to sv_base
        lsr.l   #6,d0                   ; /64 forms index into
        movea.l sv_btbas(a6),a1         ;memory block table
        adda.w  d0,a1                   ;entries from here to become unavailable
        move.l  d1,d0                   ;memory requested
        lsr.l   #6,d0                   ; /64
        lsr.l   #3,d0                   ; /8 = /512 yields
        rts                             ;number of blocks to be released

L323A   bsr.s   L320C                   ;round d1.l up to multiple of 512
        bsr.s   L3218                   ;set memory table pointer/counter
        suba.l  d1,a0                   ;BASIC area to be lowered to here
        cmpa.l  sv_free(a6),a0          ;is it too low ?
        ble.s   L329E                   ;yes, return out of memory
        move.l  a0,sv_basic(a6)         ;BASIC now resides here
        adda.l  d3,a1                   ;starting with next entry down
        bra.s   L3270                   ;release any memory blocks in use

L324E   bsr.s   L320C                   ;round d1.l up to multiple of 512
        bsr.s   L3220                   ;set memory table pointer/counter
        adda.l  d1,a0                   ;free area to be raised to here
        cmpa.l  sv_basic(a6),a0         ;is it too high ?
        bge.s   L329E                   ;yes, return out of memory
        move.l  a0,sv_free(a6)          ;free area now starts here
        bra.s   L3270                   ;release any memory blocks in use

L3260   moveq   #bt.actn,d2             ;mask for pending action
        and.b   (a1),d2                 ;status of block ?
        beq.s   L326C                   ;block is available
        jsr     L352A(pc)               ;call slaving routine of driver
        bra.s   L3260                   ;until action is complete
L326C   clr.b   (a1)                    ;block is now unavailable for slaving
        adda.w  d3,a1                   ;to next entry (up or down aaccordingly)
L3270   dbf     d0,L3260                ;until all memory blocks freed
        bra.s   L329A                   ;operation complete

L3276   bsr.s   L3212                   ;round d1.l down to multiple of 512
        bsr.s   L3218                   ;set memory table pointer/counter
        adda.l  d1,a0                   ;BASIC area to be moved up to here
        move.l  a0,sv_basic(a6)         ;BASIC now resides here
        bra.s   L3296

L3282   bsr.s   L3212                   ;round d1.l down to multiple of 512
        bsr.s   L3220
        suba.l  d1,a0
        move.l  a0,sv_free(a6)
        suba.w  d3,a1
        bra.s   L3296

L3290   move.b  #bt.empty,(a1)          ;block is now available for slaving
        suba.w  d3,a1                   ;next table entry
L3296   dbf     d0,L3290                ;until all memory blocks done
L329A   moveq   #err.ok,d0              ;operation complete
        bra.s   L32A0

L329E   moveq   #err.om,d0              ;operation unsuccessful
L32A0   rts

;Call routine for requested Trap #2 operation

L32A2   movea.l sv_jbpnt(a6),a1
        movea.l (a1),a1                 ;current job header
        bclr    #7,jb_rela6(a1)
        beq.s   L32B4
        adda.l  trlv_a6(a5),a0          ;relative a6
L32B4   subq.b  #1,d0
        beq.s   L32D0                   ;io.open
        subq.b  #1,d0
        beq     L3352                   ;io.close
        subq.w  #1,d0
        beq     L36B6                   ;io.formt
        subq.w  #1,d0
        beq     L3552                   ;io.delet
        moveq   #err.bp,d0              ;undefined operation
        bra     L03A6                   ;return from trap

;Trap #2 D0=$01 IO.OPEN - Open a channel

L32D0   movem.l a1-a4,-(a7)             ;(if job ID bad then stack abused) [!]
        movea.l a0,a1
        jsr     L03BC(pc)               ;check/get job ID & header (abort if bad)
        exg     a0,a1                   ;a0=filename, a1=job hdr
        movea.l sv_chbas(a6),a3
L32E0   tst.b   (a3)                    ;free channel table entry
        blt.s   L32F0
        addq.w  #4,a3                   ;next entry
        cmpa.l  sv_chtop(a6),a3
        blt.s   L32E0                   ;until top
        moveq   #err.no,d0
        bra.s   L334C

L32F0   movea.l sv_drlst(a6),a2         ;start with list of simple drivers
L32F4   movem.l d1-d7/a1-a6,-(a7)
        lea     -sv_lio(a2),a3          ;base of driver linakge block
        movea.l sv_aopen-sv_lio(a2),a4  ;open routine
        jsr     (a4)
        movem.l (a7)+,d1-d7/a1-a6
        tst.l   d0
        beq.s   L331E                   ;succesful open
        cmpi.w  #err.nf,d0
        bne.s   L334C                   ;abort on errors other than not found
        movea.l (a2),a2                 ;get link to next driver linkage
        move.l  a2,d0
        bgt.s   L32F4                   ;try next simple driver
        jsr     L355A(pc)               ;else try directory drivers
        tst.l   d0
        bne.s   L334C                   ;unsuccesful open
L331E   move.l  a0,(a3)                 ;definition address to table entry
        move.w  sv_chtag(a6),d2         ;new tag for channel
        addq.w  #1,sv_chtag(a6)
        addq.w  #4,a0
        move.l  a2,(a0)+                ;ch_drivr
        move.l  d1,(a0)+                ;ch_owner
        move.l  a3,(a0)+                ;ch_rflag (channel table entry)
        move.w  d2,(a0)+                ;ch_tag
        clr.w   (a0)+                   ;ch_stat & ch_actn
        clr.l   (a0)+                   ;ch_jobwt
        swap    d2
        suba.l  sv_chbas(a6),a3
        move.w  a3,d2
        lsr.w   #2,d2                   ;channel number
        movea.l d2,a0                   ;channel ID returned
        cmp.w   sv_chmax(a6),d2         ;highest channel ?
        bls.s   L334C                   ;no
        move.w  d2,sv_chmax(a6)         ;update highest
L334C   movem.l (a7)+,a1-a4
        bra.s   L3378                   ;return from trap

;Trap #2 D0=$02 IO.CLOSE - Close a channel

L3352   move.l  a0,d7                   ;preserve channel ID
        jsr     L3476(pc)               ;get channel definition (abort if bad ID)
        movem.l d1-d7/a1-a6,-(a7)
        movea.l ch_drivr(a0),a4         ;driver linkage
        lea     -sv_lio(a4),a3          ;base
        movea.l sv_aclos-sv_lio(a4),a4
        jsr     (a4)                    ;call close routine
        movem.l (a7)+,d1-d7/a1-a6
        lsl.w   #2,d7                   ;channel number
        movea.l sv_chbas(a6),a0
        adda.w  d7,a0
        st      (a0)                    ;channel table entry now free
L3378   bra     L03A6                   ;return from trap

;Call driver with requested Trap #3 operation

L337C   move.l  a0,d7                   ;preserve channel ID
        jsr     L3476(pc)               ;get channel definition (abort if bad ID)
        tas     ch_stat(a0)             ;make channel busy
        bne     L3414                   ;it already was, wait or return
        movem.l d2-d7/a2-a6,-(a7)
        clr.l   -(a7)                   ;offset for A1
        andi.l  #$7F,d0                 ;mask operation code (already done)
        cmpi.b  #fs.save,d0
        bgt.s   L33B0                   ;operations $49 to $7F
        cmpi.b  #fs.heads,d0
        bge.s   L33AE                   ;operations $46 to $49
        cmpi.b  #io.sstrg,d0
        bgt.s   L33B0                   ;operations $08 to $45
        btst    #1,d0
        beq.s   L33B0                   ;operations $00, $01, $04 and $05
L33AE   moveq   #0,d1                   ;counter for multiple byte read/write operations
L33B0   movea.l sv_jbpnt(a6),a3         ;current job
        movea.l (a3),a3                 ;job header
        bclr    #7,jb_rela6(a3)         ;A1 to be relative A6 ?
        beq.s   L33C4                   ;no
        move.l  trlv_a6(a5),(a7)
        adda.l  (a7),a1                 ;offset A1 by A6
L33C4   movea.l ch_drivr(a0),a4
        move.b  d0,ch_actn(a0)          ;pending operation
        moveq   #0,d3                   ;first attempt at I/O
        lea     -sv_lio(a4),a3
        movea.l sv_aio-sv_lio(a4),a4
        jsr     (a4)                    ;call driver I/O routine
        suba.l  (a7),a1                 ;redress A1
        cmpi.w  #err.nc,d0              ;incomplete operation ?
        bne.s   L3422                   ;no
        move.w  10(a7),d3               ;restore requested timeout
        beq.s   L3422                   ;none
        movea.l 44(a7),a6               ;restore a6 to sv_base
        jsr     L0408(pc)               ;return current job ID in d0.l
        move.l  d0,ch_jobwt(a0)         ;set job is waiting on I/O
        lea     ch_stat(a0),a0
        tst.l   (a7)+                   ;relative A6 ?
        bne.s   L33FC                   ;yes
        st      (a0)                    ;waiting and A1 passed absolute
L33FC   move.l  a0,jb_hold(a3)          ;but if job removed release channel
        move.w  d3,jb_stat(a3)          ;job timeout till reactivation
        sf      jb_prior(a3)            ;don't reschedule job next
        moveq   #err.nc,d0              ;error if job times out
        movem.l (a7)+,d2-d7/a2-a6
L340E   movea.l d7,a0                   ;restore channel ID
        bra     L0936                   ;return via scheduler

L3414   tst.w   d3                      ;timeout ?
        beq.s   L341E                   ;none, return immediately
        subq.l  #2,14(a7)               ;backtrack jobs PC to trap #3 instruction
        bra.s   L340E                   ;and wait
L341E   moveq   #err.nc,d0              ;operation incomplete
        bra.s   L342C

L3422   addq.w  #4,a7                   ;discard offset for a1
        movem.l (a7)+,d2-d7/a2-a6
        sf      ch_stat(a0)             ;release channel
L342C   movea.l d7,a0                   ;restore channel ID
        bra     L03A6                   ;return from trap

;Operation for Trap #4. A0 next trap #2 or A1 next trap #3 is to be relative A6

L3432   move.l  a3,-(a7)
        movea.l sv_jbpnt(a6),a3         ;current job
        movea.l (a3),a3                 ;job header
        tas     jb_rela6(a3)            ;flag A0/A1 relative A6
        movea.l (a7)+,a3
        bra     L03A6                   ;return from trap

;Return channel definition in a0 given channel ID in a0

L3444   move.l  a0,-(a7)                ;channel ID
        move.l  d0,-(a7)                ;preserve
        move.l  a0,d0
        cmp.w   sv_chmax(a6),d0         ;too high ?
        bhi.s   L346E                   ;yes
        lsl.w   #2,d0                   ;table entry offset
        movea.l sv_chbas(a6),a0
        adda.w  d0,a0
        tst.b   (a0)                    ;valid entry ?
        blt.s   L346E                   ;no
        movea.l (a0),a0                 ;channel definition address
        swap    d0
        cmp.w   ch_tag(a0),d0           ;correct tag ?
        bne.s   L346E                   ;no
        move.l  (a7)+,d0                ;restore
        addq.w  #4,a7                   ;discard ID
        cmp.b   d0,d0                   ;return OK
L346C   rts

L346E   addq.w  #4,a7                   ;discard d0
        moveq   #err.no,d0              ;return channel not open
        movea.l (a7)+,a0                ;but restore ID
        rts

L3476   bsr.s   L3444                   ;get channel definition
        beq.s   L346C                   ;OK
        addq.w  #4,a7                   ;discard return
        bra     L03A6                   ;return from trap

;Third linkage in list of internal scheduler tasks

L3480   dc.l    0                       ;last in list
        dc.l    L3488                   ;waiting I/O

;Do any waiting I/O

L3488   movea.l sv_chpnt(a6),a1         ;last channel dealt with
        movea.w sv_chmax(a6),a2
        adda.w  a2,a2
        adda.w  a2,a2
        adda.l  sv_chbas(a6),a2         ;highest channel to check
        movea.l a1,a3
L349A   addq.w  #4,a1                   ;next channel table entry
        cmpa.l  a2,a1                   ;highest entry ?
        ble.s   L34A4                   ;no
        movea.l sv_chbas(a6),a1         ;start again at the bottom
L34A4   tst.b   (a1)                    ;in use ?
        blt.s   L34B0                   ;no
        movea.l (a1),a0
        move.b  ch_stat(a0),d4          ;pending I/O ?
        bne.s   L34B6                   ;yes
L34B0   cmpa.l  a1,a3                   ;every channel checked ?
        bne.s   L349A                   ;no, look at next
        bra.s   L3522                   ;complete
L34B6   move.l  a1,sv_chpnt(a6)         ;update latest channel dealt with
        move.l  ch_jobwt(a0),d1         ;waiting job
        lsl.w   #2,d1                   ;job table entry
        movea.l sv_jbbas(a6),a4
        adda.w  d1,a4
        tst.b   (a4)                    ;job still alive ?
        blt.s   L3524                   ;no, clear pending status
        movea.l (a4),a4                 ;job header
        swap    d1
        cmp.w   jb_tag(a4),d1           ;correct tag ?
        blt.s   L3524                   ;no, clear pending status
        moveq   #0,d0
        move.b  ch_actn(a0),d0          ;I/O operation pending
        moveq   #-1,d3                  ;previous call was incomplete
        movem.l jb_d1(a4),d1-d2         ;restore IOSS registers
        movem.l jb_a1(a4),a1-a2         ;  "      "       "
        move.l  a4,-(a7)                ;preserve job header pointer
        clr.l   -(a7)                   ;offset for A1
        addq.b  #1,d4                   ;ch_stat
        beq.s   L34F6                   ;was $FF, A1 passed absolute
        move.l  jb_a6(a4),(a7)          ;else relative A6
        adda.l  (a7),a1
L34F6   movea.l ch_drivr(a0),a4
        lea     -sv_lio(a4),a3          ;base of driver
        movea.l sv_aio-sv_lio(a4),a4
        jsr     (a4)                    ;try I/O again
        suba.l  (a7)+,a1                ;redress A1
        movea.l (a7)+,a4                ;job header pointer
        move.l  d1,jb_d1(a4)            ;update IOSS register
        move.l  a1,jb_a1(a4)            ;   "     "     "
        cmpi.b  #$FF,d0                 ;err.nc ?
        beq.s   L3522                   ;yes, try again next time
        move.l  d0,jb_d0(a4)            ;operation complete code
        clr.b   ch_stat(a0)             ;no longer waiting
        clr.w   jb_stat(a4)             ;reactivate job
L3522   rts

L3524   clr.b   ch_stat(a0)             ;no longer waiting
        rts

;Call slaving routine of driver owning slave block

L352A   movem.l d0-d3/a0-a4,-(a7)
        moveq   #0,d1
        move.b  (a1),d1                 ;get bt_stat
        lsr.b   #4,d1                   ;clear status bits
        lsl.b   #2,d1                   ;and multiply by 4
        lea     sv_fsdef(a6),a2
        movea.l 0(a2,d1.w),a2           ;pointer to physical definition block
        movea.l fs_drivr(a2),a4         ;get associated driver
        lea     -sv_lio(a4),a3          ;base of driver linkage block
        movea.l ch_slave-ch_next(a4),a4
        jsr     (a4)                    ;call slaving routine
        movem.l (a7)+,d0-d3/a0-a4
        rts

;Trap #2 D0=$04 IO.DELET - Delete a file

L3552   st      d3                      ;operation is delete
        bsr.s   L3564                   ;when calling open
        bra     L03A6                   ;return from trap

;Scan directory device driver list for open

L355A   cmpi.b  #io.dir,d3
        bls.s   L3564
        moveq   #err.bp,d0              ;bad open type
        rts

L3564   movem.l d1-d6/a3-a6,-(a7)
        movea.l a0,a5                   ;pointer to filename
        move.l  #fs_end,d1              ;fixed length channel definition
        jsr     L2FAE(pc)               ;mm_alchp
        bne     L368A                   ;no memory for channel definition
        adda.w  #fs_next,a0             ;skip first $18 (set up by io.open)
        lea     sv_fslst(a6),a1         ;list of file channel definitions
        jsr     L39DC(pc)               ;ut_link, link in this one
        lea     (a5),a1                 ;pointer to filename
        jsr     L36D4(pc)               ;find matching device name
        bne     L366E                   ;no match found, clear up and abort
        moveq   #15,d2                  ;maximum physical definitions -1
        moveq   #-1,d0                  ;table is full
        lea     sv_fsdef+$40(a6),a4     ;search from end of table backwards
L3596   move.l  -(a4),d3
        bne.s   L359E                   ;used entry, check it
        move.w  d2,d0                   ;spare entry index
        bra.s   L35AC                   ;but find earliest
L359E   movea.l d3,a1
        cmp.b   fs_drivn(a1),d1         ;same drive number ?
        bne.s   L35AC                   ;no, try next
        cmpa.l  fs_drivr(a1),a2         ;same driver ?
        beq.s   L35DC                   ;yes
L35AC   dbf     d2,L3596
        move.w  d0,d2                   ;free entry ?
        blt     L366C                   ;no, clear up and abort
        movem.l d1-d2/a0/a2,-(a7)
        move.l  ch_dflen(a2),d1         ;length of physical definition block
        jsr     L2FAE(pc)               ;mm_alchp
        movem.l (a7)+,d1-d2/a1-a2
        exg     a0,a1
        bne     L366E                           ;no memory, clear up and abort
        move.l  a2,fs_drivr(a1)
        move.b  d1,fs_drivn(a1)
        move.w  d2,d0
        lsl.w   #2,d0
        adda.w  d0,a4                           ;pointer to table free entry
        move.l  a1,(a4)                         ;update with channel definition
L35DC   move.b  d2,fs_drive-fs_next(a0)
        move.b  11(a7),fs_acces-fs_next(a0)     ;d3.b operation required
        lea     fs_fname-fs_next(a0),a4
        move.w  (a5)+,d0                        ;filename length
        move.w  ch_drnam(a2),d3                 ;length of driver name
        addq.w  #2,d3                           ;+ drive number and underscore
        adda.w  d3,a5                           ;skip over device part of filename
        sub.w   d3,d0
        blt.s   L3660                           ;filename too short, bad name
        cmpi.w  #36,d0
        bgt.s   L3660                           ;filename too long, bad name
        move.w  d0,(a4)+                        ;fs_fname length
        bra.s   L3604
L3602   move.b  (a5)+,(a4)+                     ;copy filename part
L3604   dbf     d0,L3602
        movea.l a1,a5                           ;pointer to physical definition block
        movea.l a0,a1                           ;pointer to fs_next in channel definition
L360C   movea.l (a1),a1
        move.l  a1,d0
        beq.s   L3652                           ;last link in file system
        cmp.b   fs_drive-fs_next(a1),d2
        bne.s   L360C                           ;not same drive ID
        moveq   #1,d0                           ;ignore case in ut_cstr
        movea.w #fs_fname-fs_next,a6            ;offset for a0 & a1
        jsr     L3A9C(pc)                       ;ut_cstr
        movea.l 36(a7),a6                       ;restore a6
        bne.s   L360C                           ;not same filename
        cmpi.b  #io.new,fs_acces-fs_next(a0)    ;new exclusive device open ?
        beq.s   L3664                           ;yes, error file exists
        cmpi.b  #io.share,fs_acces-fs_next(a0)  ;old shared device open ?
        bne.s   L3668                           ;no, error in use
        cmpi.b  #io.share,fs_acces-fs_next(a1)  ;already open as shared open ?
        bne.s   L3668                                       ;no, error in use
        move.w  fs_filnr-fs_next(a1),fs_filnr-fs_next(a0)   ;copy drive file number
        move.l  fs_eblok-fs_next(a1),fs_eblok-fs_next(a0)   ;copy EOF position
        move.w  #64,fs_nbyte-fs_next(a0)                    ;file position after header
L3652   movea.l a5,a1                           ;pointer to physical definition block
        tst.w   fs_filnr-fs_next(a0)            ;is this file already open ?
        beq.s   L3690                           ;no, this is the only occurence
        suba.w  #fs_next,a0                     ;start of channel definition block
        bra.s   L3684                           ;open operation complete

L3660   moveq   #err.bn,d0              ;bad device name
        bra.s   L366E
L3664   moveq   #err.ex,d0              ;file already exists
        bra.s   L366E
L3668   moveq   #err.iu,d0              ;file is in use
        bra.s   L366E
L366C   moveq   #err.no,d0              ;channel is not open
L366E   lea     sv_fslst(a6),a1
        jsr     L39E2(pc)               ;ut_unlnk, unlink the physical definition block
        suba.w  #fs_next,a0             ;base of channel definition
        move.l  d0,d4                   ;preserve error
        jsr     L305E(pc)               ;mm_rechp, reclaim heap for channel definition
        move.l  d4,d0
        bra.s   L368A
L3684   addq.b  #1,fs_files(a1)         ;increment number of files open to this FS
        moveq   #err.ok,d0
L368A   movem.l (a7)+,d1-d6/a3-a6
        rts

L3690   suba.w  #fs_next,a0             ;start of channel definition
        move.l  a1,-(a7)                ;pointer to physical definition block
        move.l  a2,-(a7)                ;pointer to driver linkage block
        lea     -sv_lio(a2),a3          ;base of linkage block
        movea.l sv_aopen-sv_lio(a2),a4
        jsr     (a4)                    ;open to file
        movea.l (a7)+,a2
        movea.l (a7)+,a1
        tst.b   fs_acces(a0)            ;open type ?
        blt.s   L36B0                   ;was delete, clear up
        tst.l   d0                      ;any error opening ?
        beq.s   L3684                   ;no, open complete
L36B0   adda.w  #fs_next,a0
        bra.s   L366E                   ;clear up

;Trap #2 D0=$03 IO.FORMAT - Format a medium

L36B6   movem.l d3-d7/a1-a5,-(a7)
        movea.l a0,a1
        jsr     L36D4(pc)               ;check device name
        blt.s   L36CC                   ;bad
        lea     -sv_lio(a2),a3
        movea.l ch_formt(a2),a4
        jsr     (a4)                    ;call format routine
L36CC   movem.l (a7)+,d3-d7/a1-a5
        bra     L03A6                   ;return from trap

;Find matching device in filename with a directory driver

L36D4   movem.l a1/a4,-(a7)
        movea.l sv_ddlst(a6),a2         ;list of directory driver linkages
L36DC   lea     ch_drnam(a2),a4
        move.w  (a4)+,d0
        movea.l (a7),a1
        addq.w  #2,a1                   ;start of text of filename
L36E6   move.b  (a1)+,d1
        bclr    #5,d1                   ;make uppercase
        cmp.b   (a4)+,d1
        bne.s   L36F6                   ;name mismatch
        subq.w  #1,d0
        bgt.s   L36E6                   ;next device name character
        bra.s   L36FE                   ;match found
L36F6   movea.l (a2),a2                 ;next link in list
        move.l  a2,d0
        bne.s   L36DC                   ;look for device name match
        bra.s   L3718                   ;not found
L36FE   moveq   #0,d1
        move.b  (a1)+,d1                ;drive number requested
        subi.b  #'0',d1
        ble.s   L3718                   ;bad number < 1
        cmpi.b  #9,d1
        bge.s   L3718                   ;bad number > 8
        cmpi.b  #'_',(a1)
        bne.s   L3718                   ;no underscore separator
        moveq   #err.ok,d0
        bra.s   L371A                   ;match found OK
L3718   moveq   #err.nf,d0
L371A   movem.l (a7)+,a1/a4
        rts

L3720   move.l  a2,d2
        addq.l  #1,d2
        bclr    #0,d2
        movea.l d2,a2
        rts

;Vector $122 IO.NAME - Decode device name

L372C   movea.l (a7),a2
        addq.w  #6,a2
        movem.l d4/d7/a0/a3,-(a7)
        moveq   #0,d7
        move.w  (a0)+,d7
        add.l   a0,d7
        move.w  (a2)+,d2
L373C   bsr.s   L37B6
        cmp.b   (a2)+,d1
        bne.s   L37A6
        subq.b  #1,d2
        bne.s   L373C
        bsr.s   L3720
        move.w  (a2)+,d4
        bra.s   L3790

L374C   bsr.s   L3720
        move.b  (a2)+,d1
        beq.s   L3778
        blt.s   L3760
        bsr.s   L37B6
        cmp.b   (a2)+,d1
        beq.s   L3762
        subq.w  #1,a0
        move.w  (a2)+,(a3)+
        bra.s   L3790

L3760   addq.w  #1,a2
L3762   movea.l a7,a1
        move.w  (a2)+,-(a7)
        suba.l  a6,a0
        suba.l  a6,a1
        sub.l   a6,d7
        jsr     L3DC2(pc)               ;cn_dtoi
        adda.l  a6,a0
        add.l   a6,d7
        move.w  (a7)+,(a3)+
        bra.s   L3790

L3778   move.b  (a2)+,d2
        ext.w   d2
        adda.w  d2,a2
        move.l  a2,-(a7)
        bsr.s   L37B6
L3782   cmp.b   -(a2),d1
        beq.s   L378C
        subq.w  #1,d2
        bne.s   L3782
        subq.w  #1,a0
L378C   move.w  d2,(a3)+
        movea.l (a7)+,a2
L3790   dbf     d4,L374C
        cmp.l   a0,d7
        bne.s   L379C
        moveq   #$04,d1
        bra.s   L37AE

L379C   movem.l (a7)+,d4/d7/a0/a3
        addq.l  #2,(a7)
        moveq   #err.bn,d0
        rts

L37A6   movem.l (a7)+,d4/d7/a0/a3
        moveq   #err.nf,d0
        rts

L37AE   movem.l (a7)+,d4/d7/a0/a3
        addq.l  #4,(a7)
        rts

L37B6   moveq   #0,d1
        cmp.l   a0,d7
        beq.s   L37C8
        move.b  (a0),d1
        cmpi.b  #$60,d1
        blt.s   L37C8
        subi.b  #$20,d1
L37C8   addq.w  #1,a0
        rts

;Vector $E8 IO.SERQ - Direct queue handling

L37CC   lea     ch_qin(a0),a2
        cmpi.b  #io.fstrg,d0
        bls.s   L37D8                   ;fetch operation
        addq.w  #4,a2                   ;else send operation, ch_qout
L37D8   tst.l   (a2)                    ;queue pointer ?
        beq.s   L37F0                   ;none, so incorrect operation
        movea.l (a2),a2
        jsr     L388C(pc)               ;io.serio  to perform...
        dc.l    L380A                   ;io.qtest  test pending input
        dc.l    L385E                   ;io.qout   fetch a byte
        dc.l    L3838                   ;io.qin    send a byte
        rts                             ;return error to caller

L37F0   moveq   #err.bp,d0
        rts

;Vector $DC IO.QSET - Set up a queue header

L37F4   lea     q_queue(a2,d1.w),a3     ;end of queue
        clr.l   (a2)+                   ;q_queue, link to next queue
        move.l  a3,(a2)+                ;q_end
        subq.w  #1,a3
        move.l  a3,(a2)+                ;q_nextin
        move.l  a3,(a2)+                ;q_nxtout is same
        suba.l  #16,a2                  ;redress back to queue header
        rts                             ;a3 is start of actual queue

;Vector $DE IO.QTEST - Test queue status

L380A   moveq   #-q_queue-1,d2
        add.l   q_end(a2),d2
        sub.l   a2,d2                   ;length of queue-1
        move.l  q_nextin(a2),d0
        movea.l q_nxtout(a2),a3
        move.b  (a3),d1                 ;next byte in queue
        sub.l   a3,d0
        bgt.s   L3832                   ;q_nextin > q_qnxtout
        blt.s   L382E                   ;q_nextin < q_qnxtout
        tst.b   (a2)                    ;q_eoff ?
        blt.s   L382A                   ;msb set
        moveq   #err.nc,d0              ;queue is empty
        rts

L382A   moveq   #err.ef,d0              ;queue is at EOF
        rts

L382E   add.l   d2,d0
        addq.l  #1,d0
L3832   sub.l   d0,d2                   ;free space in queue
        moveq   #err.ok,d0              ;queue has something in it
        rts

;Vector $E0 IO.QIN - Put byte in queue

L3838   tst.b   (a2)                    ;q_eoff queue at EOF ?
        bne.s   L385A                   ;yes, but don't tell the caller (!)
        movea.l q_nextin(a2),a3
        move.b  d1,(a3)+                ;put byte in queue (tentatively)
        cmpa.l  q_end(a2),a3            ;end of queue ?
        blt.s   L384C                   ;no
        lea     q_queue(a2),a3          ;else reset pointer to start of queue
L384C   cmpa.l  q_nxtout(a2),a3         ;unless it's full ?
        bne.s   L3856                   ;no
        moveq   #err.nc,d0              ;queue is full, try again later
        rts                             ;(d1.b lost in spare queue byte)

L3856   move.l  a3,q_nextin(a2)         ;update pointer for next in
L385A   moveq   #err.ok,d0              ;byte was put in queue
        rts

;Vector $E2 IO.QOUT - Get byte from queue

L385E   movea.l q_nxtout(a2),a3
        cmpa.l  q_nextin(a2),a3         ;something in queue ?
        bne.s   L3874                   ;yes
        tst.b   (a2)                    ;q_eoff queue at EOF ?
        blt.s   L3870                   ;yes, tell caller
        moveq   #err.nc,d0              ;else queue is empty
        rts

L3870   moveq   #err.ef,d0              ;queue is at EOF
        rts

L3874   move.b  (a3)+,d1                ;get next byte from queue
        cmpa.l  q_end(a2),a3            ;end of queue ?
        blt.s   L3880                   ;no
        lea     q_queue(a2),a3          ;else reset pointer to start of queue
L3880   move.l  a3,q_nxtout(a2)         ;update pointer for next out
        moveq   #err.ok,d0              ;byte was fetched from queue
        rts

;Vector $E4 IO.QEOF - Put EOF marker in queue

L3888   tas     (a2)                    ;set q_eoff the EOF flag
        rts

;Vector $EA IO.SERIO - General I/O handling

L388C   addi.l  #12,(a7)                ;skip over pending/fetch/send pointers
        movea.l (a7),a4                 ;return address
        move.l  d2,d4                   ;IOSS value
        move.l  d1,d5                   ;IOSS value
        cmpi.w  #fs.mdinf,d0
        bhi.s   L38B2                   ;file system operation
        cmpi.w  #io.sstrg,d0
        bhi.s   L3914                   ;return with bad parameter
        andi.l  #$0000FFFF,d4           ;limit IOSS value to word
        move.b  L38C0(pc,d0.w),d0
        jmp     L38C0(pc,d0.w)          ;operations $00 to $07

L38B2   cmpi.b  #fs.save,d0
        bhi.s   L3914                   ;return with bad parameter
        move.b  L38C0-$3E(pc,d0.w),d0
        jmp     L38C0(pc,d0.w)          ;operations $46 to $49

L38C0   dc.b    L390A-L38C0             ;$00 io.pend
        dc.b    L390C-L38C0             ;$01 io.fbyte
        dc.b    L38CC-L38C0             ;$02 io.fline
        dc.b    L38F8-L38C0             ;$03 io.fstrg
        dc.b    L3914-L38C0             ;$04 io.edlin
        dc.b    L390E-L38C0             ;$05 io.sbyte
        dc.b    L3914-L38C0             ;$06 (undefined)
        dc.b    L38E4-L38C0             ;$07 io.sstrg
        dc.b    L3930-L38C0             ;$46 fs.heads
        dc.b    L3944-L38C0             ;$47 fs.headr
        dc.b    L38F8-L38C0             ;$48 fs.load
        dc.b    L38E4-L38C0             ;$49 fs.save

;IO.SERIO operation io.fline

L38CC   movea.l -8(a4),a4               ;fetch a byte routine
L38D0   cmp.l   d5,d4                   ;buffer overflow ?
        bls.s   L3918                   ;yes, return such
        bsr.s   L3922                   ;fetch a byte
        bne.s   L391E                   ;return any error
        move.b  d1,(a1)+                ;put byte in buffer
        addq.l  #1,d5                   ;increment byte counter
        cmpi.b  #10,d1                  ;LF ?
        bne.s   L38D0                   ;no, keep fetching
        bra.s   L391E                   ;line fetched

;IO.SERIO operation io.sstrg / fs.save

L38E4   movea.l -4(a4),a4               ;send a byte routine
L38E8   cmp.l   d5,d4                   ;all bytes sent ?
        bls.s   L391C                   ;yes, complete
        move.b  (a1),d1                 ;get byte from buffer
        bsr.s   L3922                   ;send it
        bne.s   L391E                   ;return any error
        addq.w  #1,a1                   ;update buffer pointer
        addq.l  #1,d5                   ;increment byte counter
        bra.s   L38E8                   ;next byte

;IO.SERIO operation io.fstrg / fs.load

L38F8   movea.l -8(a4),a4               ;fetch a byte routine
L38FC   cmp.l   d5,d4                   ;buffer full ?
        bls.s   L391C                   ;yes, return complete
        bsr.s   L3922                   ;fetch a byte
        bne.s   L391E                   ;return any error
        move.b  d1,(a1)+                ;put byte in buffer
        addq.l  #1,d5                   ;increment byte counter
        bra.s   L38FC                   ;next byte

;IO.SERIO operation io.pend

L390A   subq.w  #4,a4                   ;test pending routine

;IO.SERIO operation io.fbyte

L390C   subq.w  #4,a4                   ;fetch a byte routine

;IO.SERIO operation io.sbyte

L390E   movea.l -4(a4),a4               ;send a byte routine
        bra.s   L3922                   ;perform action

;IO.SERIO operation unsupported or error return

L3914   moveq   #err.bp,d0
        bra.s   L392C
L3918   moveq   #err.bo,d0              ;buffer exceeded
        bra.s   L391E
L391C   moveq   #err.ok,d0
L391E   move.l  d5,d1                   ;bytes achieved
        bra.s   L392C

;Call pending/fetch/send a byte routine

L3922   movem.l d4-d5/a1/a4,-(a7)
        jsr     (a4)                    ;call appropriate routine
        movem.l (a7)+,d4-d5/a1/a4
L392C   tst.l   d0                      ;returned error code
        rts

;IO.SERIO operation fs.heads

L3930   movea.l -4(a4),a4               ;send a byte routine
        moveq   #15,d4                  ;length of file header
        tst.w   d5                      ;(0 if from trap #3)
        bgt.s   L38E8                   ;send all bytes
        st      d1                      ;else set header byte $FF
        bsr.s   L3922                   ;and send it
        bne.s   L391E                   ;return any error
        moveq   #1,d5                   ;one byte already sent
        bra.s   L38E8                   ;now send the rest

;IO.SERIO operation fs.headr

L3944   moveq   #15,d4                  ;length of file header
        tst.w   d5                      ;(0 if from trap #3)
        bgt.s   L38F8                   ;fetch all bytes
        movea.l -12(a4),a4              ;test pending input routine
        bsr.s   L3922                   ;call it
        bne.s   L391E                   ;return any error
        addq.b  #1,d1                   ;header byte $FF ?
        bne.s   L3914                   ;no, return bad parameter
        movea.l (a7),a4                 ;reset pointer
        bsr.s   L390C                   ;fetch a byte
        moveq   #1,d5                   ;one byte already fetched
        bra.s   L38FC                   ;now fetch the rest

;Vector $CA UT_ERR0 - Send error message to channel #0

L395E   move.l  a0,-(a7)
        suba.l  a0,a0                   ;channel #0
        bsr.s   L3968                   ;ut_err
        movea.l (a7)+,a0
        rts

;Vector $CC UT_ERR - Send error message to channel

L3968   tst.l   d0
        bge.s   L398E
        movem.l d0-d3/a1,-(a7)
        movea.l d0,a1
        add.l   d0,d0
        bvs.s   L3986
        neg.w   d0
        movea.l sv_mgtab+sv_base,a1
        move.w  0(a1,d0.w),d0
        lea     0(a1,d0.w),a1
L3986   jsr     L39B2(pc)               ;ut_mtext
        movem.l (a7)+,d0-d3/a1
L398E   rts

;Vector $CE UT_MINT - Print decimal number on channel

L3990   move.l  a6,-(a7)
        suba.l  a6,a6
        movea.l a7,a1
        subq.w  #8,a7
        move.l  a0,-(a7)
        lea     4(a7),a0
        move.w  d1,-(a1)
        jsr     L3E54(pc)               ;cn_itod
        movea.l (a7)+,a0
        movea.l a7,a1
        move.w  d1,d2
        bsr.s   L39B4
        addq.w  #8,a7
        movea.l (a7)+,a6
        rts

;Vector $D0 UT_MTEXT - Send message to channel

L39B2   move.w  (a1)+,d2
L39B4   moveq   #io.sstrg,d0
        move.l  a0,d3
        beq.s   L39BE
        moveq   #forever,d3
        bra.s   L39C4
L39BE   sf      sv_scrst+sv_base        ;make screen active
L39C4   trap    #3
        cmpi.w  #err.nc,d0
        bne.s   L39D8
        movea.l #$00010001,a0
        moveq   #io.sstrg,d0
        trap    #3
        suba.l  a0,a0
L39D8   tst.l   d0
        rts

;Vector $D2 UT_LINK - Link item into list

L39DC   move.l  (a1),(a0)
        move.l  a0,(a1)
        rts

;Vector $D4 UT_UNLNK - Unlink item from list

L39E2   cmpa.l  (a1),a0
        beq.s   L39EE                   ;link found
        tst.l   (a1)
        beq.s   L39F0                   ;end of list
        movea.l (a1),a1
        bra.s   L39E2                   ;until found

L39EE   move.l  (a0),(a1)               ;unlink
L39F0   rts

;Vector $C4 UT_WINDW - Create window channel

L39F2   bsr.s   L3A2A
        bra.s   L3A0C

;Vector $C6 UT_CON - Create console channel

L39F6   lea     L3A44(pc),a0
        bra.s   L3A00

;Vector $C8 UT_SCR - Create screen channel

L39FC   lea     L3A4A(pc),a0
L3A00   bsr.s   L3A2A
L3A02   addq.w  #4,a1
        moveq   #sd.wdef,d0
        moveq   #0,d2
        bsr.s   L3A36
        subq.w  #4,a1
L3A0C   moveq   #sd.bordr,d0
        move.b  (a1)+,d1
        move.b  (a1)+,d2
        bsr.s   L3A36
        moveq   #sd.setpa,d0
        move.b  (a1),d1
        bsr.s   L3A36
        moveq   #sd.setst,d0
        move.b  (a1)+,d1
        bsr.s   L3A36
        moveq   #sd.setin,d0
        move.b  (a1),d1
        bsr.s   L3A36
        moveq   #sd.clear,d0
        bra.s   L3A36

L3A2A   move.l  a1,-(a7)
        moveq   #io.open,d0
        moveq   #my.job,d1
        moveq   #io.old,d3
        trap    #2
        bra.s   L3A3A

L3A36   move.l  a1,-(a7)
        trap    #3
L3A3A   movea.l (a7)+,a1
        tst.l   d0
        beq.s   L3A42
        addq.w  #4,a7
L3A42   rts

L3A44   dc.w    3
        dc.b    'CON',0
L3A4A   dc.w    3
        dc.b    'SCR',0

;Get end of strings0 and string1 for ut_cstr

L3A50   moveq   #0,d4
        move.b  d0,d4
        ror.l   #1,d4
        subq.b  #2,d4
        move.w  0(a6,a0.l),d0
        addq.w  #2,a0
        lea     0(a0,d0.w),a2
        move.w  0(a6,a1.l),d1
        addq.w  #2,a1
        lea     0(a1,d1.w),a3
        rts

L3A6E   movem.l d4/a0-a4,-(a7)
        bsr.s   L3A50
        lea     0(a1,d0.w),a4
        exg     a4,a3
        moveq   #0,d1
L3A7C   cmpa.l  a4,a3
        bgt.s   L3A8C
        addq.l  #1,d1
        bsr.s   L3A96
        beq.s   L3A90
        addq.w  #1,a1
        addq.w  #1,a3
        bra.s   L3A7C

L3A8C   moveq   #0,d1
        moveq   #err.ok,d0
L3A90   movem.l (a7)+,d4/a0-a4
        rts

L3A96   movem.l d1-d5/a0-a3,-(a7)       ;naughty Qlib cstr entry point
        bra.s   L3AA2

;Vector $E6 UT_CSTR - String comparison

L3A9C   movem.l d1-d5/a0-a3,-(a7)
        bsr.s   L3A50
L3AA2   cmpa.l  a2,a0                   ;null length string0 ?
        bne.s   L3AAC
        cmpa.l  a3,a1                   ;null length string1 ?
        bne.s   L3AC8                   ;no, so string0 < string1
        bra.s   L3ACC                   ;must be same

L3AAC   cmpa.l  a3,a1                   ;null length string1 ?
        beq.s   L3AD0                   ;yes, so string0 > string1
        bsr.s   L3AE8
        cmp.b   d4,d0
        beq.s   L3B2C
        cmp.b   d3,d2
        bne.s   L3AC6
        tst.b   d0
        ble.s   L3AA2
        tst.l   d4
        blt.s   L3AA2
        cmpi.b  #2,d0
L3AC6   bgt.s   L3AD0
L3AC8   moveq   #-1,d0
        bra.s   L3AD2

L3ACC   moveq   #err.ok,d0
        bra.s   L3AD2

L3AD0   moveq   #1,d0
L3AD2   movem.l (a7)+,d1-d5/a0-a3
        rts

L3AD8   dc.b    0,0,0,0,0,-1,0,0,0,0,0,1,0,0,3,0

L3AE8   exg     a1,a0
        exg     a3,a2
        bsr.s   L3B04
        exg     a1,a0
        exg     a3,a2
        move.b  d0,d1
        move.b  d2,d3
        bsr.s   L3B04
        lsl.b   #2,d0
        add.b   d1,d0
        ext.w   d0
        move.b  L3AD8(pc,d0.w),d0
        rts

L3B04   move.b  0(a6,a0.l),d2
        addq.w  #1,a0
        bsr     L3BFA
        cmpi.b  #$CE,d2
        bne.s   L3B2A
        cmpa.l  a2,a0
        beq.s   L3B28
        cmpi.b  #'0',0(a6,a0.l)
        blt.s   L3B28
        cmpi.b  #'9',0(a6,a0.l)
        ble.s   L3B2A
L3B28   clr.b   d0
L3B2A   rts

L3B2C   bsr.s   L3B96
        move.w  d3,d2
        move.w  d1,d3
        exg     a1,a0
        exg     a3,a2
        bsr.s   L3B96
        cmp.w   d1,d3
L3B3A   bne.s   L3AC6
        suba.w  d1,a0
        suba.w  d1,a1
        bra.s   L3B50

L3B42   move.b  0(a6,a1.l),d5
        addq.w  #1,a1
        cmp.b   0(a6,a0.l),d5
        addq.w  #1,a0
        bne.s   L3B3A
L3B50   dbf     d1,L3B42
        bsr.s   L3BCC
        move.w  d3,-(a7)
        move.w  d1,-(a7)
        exg     a1,a0
        exg     a3,a2
        bsr.s   L3BCC
        move.w  (a7)+,d0
        move.w  (a7)+,d2
        sub.w   d2,d3
        bgt.s   L3B6A
        add.w   d3,d2
L3B6A   tst.w   d2
        beq.s   L3B8C
        suba.w  d1,a0
        suba.w  d0,a1
        sub.w   d2,d1
        sub.w   d2,d0
L3B76   move.b  0(a6,a0.l),d5
        addq.w  #1,a0
        cmp.b   0(a6,a1.l),d5
        addq.w  #1,a1
        bne.s   L3B3A
        subq.w  #1,d2
        bne.s   L3B76
        adda.w  d1,a0
        adda.w  d0,a1
L3B8C   tst.w   d3
        bne     L3AC6
        bra     L3AA2

L3B96   moveq   #0,d1
L3B98   cmpi.b  #$D0,d2
        bne.s   L3BB0
        cmpa.l  a2,a0
        beq.s   L3BCA
        move.b  0(a6,a0.l),d2
        addq.w  #1,a0
        bsr.s   L3BFA
        subq.b  #1,d0
        bne.s   L3BC8
        bra.s   L3B98

L3BB0   cmpi.b  #$CE,d2
        beq.s   L3BC8
        addq.w  #1,d1
        cmpa.l  a2,a0
        beq.s   L3BCA
        move.b  0(a6,a0.l),d2
        addq.w  #1,a0
        bsr.s   L3BFA
        subq.b  #1,d0
        beq.s   L3BB0
L3BC8   subq.w  #1,a0
L3BCA   rts

L3BCC   moveq   #0,d1
        moveq   #0,d3
L3BD0   cmpa.l  a2,a0
        beq.s   L3BF8
        move.b  0(a6,a0.l),d2
        bsr.s   L3BFA
        subq.b  #1,d0
        bne.s   L3BF8
        cmpi.b  #$CE,d2
        bne.s   L3BEA
        tst.l   d1
        bne.s   L3BF8
        moveq   #-$01,d1
L3BEA   addq.w  #1,a0
        addq.w  #1,d1
        cmpi.b  #$D0,d2
        beq.s   L3BD0
        move.w  d1,d3
        bra.s   L3BD0

L3BF8   rts

L3BFA   move.b  d2,d0
        blt.s   L3C14
        cmpi.b  #'.',d0
        beq.s   L3C18
        ext.w   d0
        addi.w  #$002E,d0
        move.b  L3C28-$2E(pc,d0.w),d0
        beq.s   L3C14
        subq.b  #2,d0
        ble.s   L3C18
L3C14   clr.b   d0
        rts

L3C18   move.b  d2,d0
        addi.b  #$A0,d2
        bcc.s   L3C24
        subi.b  #$20,d2
L3C24   lsr.b   #5,d0
        rts

;Character classification (codes 0 to 127)

L3C28   dc.b    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;codes 0-31
        dc.b    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

        dc.b    $20                             ;' '
        dc.b    3                               ;'!'
        dc.b    0,0
        dc.b    $24                             ;'$'
        dc.b    $25                             ;'%'
        dc.b    0
        dc.b    3                               ;'''
        dc.b    0,0,0,0
        dc.b    3                               ;','
        dc.b    0,0,0

        dc.b    2,2,2,2,2,2,2,2,2,2             ;'0' to '9'

        dc.b    0
        dc.b    3                               ;';'
        dc.b    0,0,0,0,0

        dc.b    1,1,1,1,1,1,1,1,1,1,1,1,1       ;'A' to 'Z'
        dc.b    1,1,1,1,1,1,1,1,1,1,1,1,1

        dc.b    0,0,0,0
        dc.b    1                               ;'_'
        dc.b    0

        dc.b    1,1,1,1,1,1,1,1,1,1,1,1,1       ;'a' to 'z'
        dc.b    1,1,1,1,1,1,1,1,1,1,1,1,1

        dc.b    0,0,0,0,0

L3CA8   cmp.l   a0,d7
        beq.s   L3CCA
        addq.w  #1,a0
L3CAE   cmpi.b  #$20,0(a6,a0.l)
        beq.s   L3CA8
        moveq   #0,d5
        moveq   #$2B,d6
        sub.b   0(a6,a0.l),d6
        beq.s   L3CC8
        addq.b  #2,d6
        bne.s   L3CCA
        bset    #31,d5
L3CC8   addq.w  #1,a0
L3CCA   rts

L3CCC   moveq   #0,d6
        cmp.l   a0,d7
        beq.s   L3CEA
        move.b  0(a6,a0.l),d6
        subi.w  #$0030,d6
        blt.s   L3CE6
        cmpi.w  #$0009,d6
        bgt.s   L3CE6
        addq.l  #2,(a7)
        bra.s   L3CEA

L3CE6   addi.w  #$0030,d6
L3CEA   addq.w  #1,a0
        rts

L3CEE   bsr.s   L3CAE
        bsr.s   L3CCC
        bra.s   L3D08
        move.l  d6,d3
L3CF6   bsr.s   L3CCC
        bra.s   L3D0C
        mulu.w  #10,d3
        add.l   d6,d3
        cmpi.l  #$00007FFF,d3
        ble.s   L3CF6
L3D08   moveq   #err.xp,d0
        rts

L3D0C   tst.l   d5
        bpl.s   L3D12
        neg.w   d3
L3D12   moveq   #err.ok,d0
        rts

;Vector $100 CN_DTOF - Convert ASCII to floating point

L3D16   movem.l d3-d6/a0-a1,-(a7)
        moveq   #0,d4
        bsr.s   L3CAE
        subq.w  #6,a1
        clr.l   2(a6,a1.l)
        clr.w   0(a6,a1.l)
L3D28   bsr.s   L3CCC
        bra.s   L3D48
        move.b  #$DF,d5
        tst.w   d4
        beq.s   L3D36
        addq.w  #1,d4
L3D36   bsr.s   L3DB6
        jsr     L48DE(pc)               ;ri.mult
        bne.s   L3DB0
        move.l  d6,d1
        bsr.s   L3DB8
        jsr     L4838(pc)               ;ri.add
        bra.s   L3D28

L3D48   cmpi.b  #'.',d6
        bne.s   L3D56
        tst.w   d4
        bne.s   L3DAE
        moveq   #1,d4
        bra.s   L3D28

L3D56   tst.b   d5
        beq.s   L3DAE
        tst.l   d5
        bpl.s   L3D62
        jsr     L4A0C(pc)               ;ri.neg
L3D62   moveq   #0,d3
        and.b   d5,d6
        cmpi.b  #'E',d6
        bne.s   L3D70
        bsr.s   L3CEE
        bne.s   L3DB0
L3D70   tst.w   d4
        beq.s   L3D76
        subq.w  #1,d4
L3D76   sub.w   d3,d4
        bvs.s   L3DAE
        beq.s   L3DA2
        sge     d5
        bge.s   L3D82
        neg.w   d4
L3D82   bsr.s   L3DB6
        subq.w  #2,a1
        move.w  d4,0(a6,a1.l)
        jsr     L47DC(pc)
        bne.s   L3DB0
        tst.b   d5
        beq.s   L3D9C
        jsr     L497E(pc)               ;ri.div
        bne.s   L3DB0
        bra.s   L3DA2

L3D9C   jsr     L48DE(pc)               ;ri.mult
        bne.s   L3DB0
L3DA2   movem.l (a7)+,d3-d6
        subq.w  #1,a0
        addq.w  #8,a7
        moveq   #err.ok,d0
        rts

L3DAE   moveq   #err.xp,d0
L3DB0   movem.l (a7)+,d3-d6/a0-a1
        rts

L3DB6   moveq   #10,d1
L3DB8   move.l  #$081F,d0
        jmp     L4830(pc)

;Vector $102 CN_DTOI - Convert ASCII to integer

L3DC2   movem.l d3-d6/a0-a1,-(a7)
        bsr     L3CEE
        bne.s   L3DB0
        subq.w  #2,a1
        move.w  d3,0(a6,a1.l)
        bra.s   L3DA2

;Vector $10A CN_HTOIB - Convert ASCII to hex byte

L3DD4   moveq   #2,d2
        bra.s   L3DDE

;Vector $10C CN_HTOIW - Convert ASCII to hex word

L3DD8   moveq   #4,d2
        bra.s   L3DDE

;Vector $10E CN_HTOIL - Convert ASCII to hex long

L3DDC   moveq   #8,d2
L3DDE   movem.l d3-d6/a0-a1,-(a7)
        move.l  d2,d4
        lsr.b   #1,d4
        subq.w  #2,a1
        move.b  d4,1(a6,a1.l)
        move.b  #15,0(a6,a1.l)
        moveq   #4,d4
L3DF4   move.l  d2,d5
        moveq   #0,d3
L3DF8   bsr     L3CCC
        bra.s   L3E00
        bra.s   L3E0E
L3E00   andi.b  #$DF,d6
        subi.b  #'A',d6
        blt.s   L3E1E
        addi.b  #10,d6
L3E0E   cmp.b   0(a6,a1.l),d6
        bhi.s   L3E1E
        lsl.l   d4,d3
        add.l   d6,d3
        dbf     d2,L3DF8
        bra.s   L3E30

L3E1E   move.b  1(a6,a1.l),d4
        addq.l  #2,a1
        move.l  d3,-4(a6,a1.l)
        suba.l  d4,a1
        cmp.w   d2,d5
        bgt     L3DA2
L3E30   bra     L3DAE

;Vector $104 CN_BTOIB - Convert ASCII to binary byte

L3E34   moveq   #8,d2
        bra.s   L3E3E

;Vector $106 CN_BTOIW - Convert ASCII to binary word

L3E38   moveq   #16,d2
        bra.s   L3E3E

;Vector $108 CN_BTIOL - Convert ASCII to binary long

L3E3C   moveq   #32,d2
L3E3E   movem.l d3-d6/a0-a1,-(a7)
        move.l  d2,d4
        lsr.b   #3,d4
        subq.w  #2,a1
        move.b  d4,1(a6,a1.l)
        moveq   #1,d4
        move.b  d4,0(a6,a1.l)
        bra.s   L3DF4

;Vector $F2 CN_ITOD - Convert integer to ASCII

L3E54   move.l  a2,-(a7)
        move.l  a0,-(a7)
        moveq   #0,d0
        move.w  0(a6,a1.l),d0
        addq.w  #2,a1
        bge.s   L3E6C
        move.b  #'-',0(a6,a0.l)
        addq.w  #1,a0
        neg.w   d0
L3E6C   move.l  a0,-(a7)
        addq.w  #5,a0
L3E70   divu.w  #10,d0
        swap    d0
        addi.b  #'0',d0
        subq.w  #1,a0
        move.b  d0,0(a6,a0.l)
        clr.w   d0
        swap    d0
        bne.s   L3E70
        move.l  (a7)+,d1
        sub.l   a0,d1
        move.w  d1,d0
        addq.w  #4,d0
L3E8E   lea     0(a0,d1.w),a2
        move.b  0(a6,a0.l),0(a6,a2.l)
        addq.w  #1,a0
        dbf     d0,L3E8E
        add.l   a0,d1
        movea.l d1,a0
        sub.l   (a7)+,d1
        movea.l (a7)+,a2
        rts

;Vector $FE CN_ITOHL - Convert hex long word to ASCII

L3EA8   bsr     L3EAC                   ;cn_itohw

;Vector $FC CN_ITOHW - Convert hex word to ASCII

L3EAC   bsr     L3EB0                   ;cn_itohb

;Vector $FA CN_IT0HB -- Convert hex byte to ASCII

L3EB0   move.b  0(a6,a1.l),d0
        lsr.b   #4,d0
        bsr.s   L3EC0
        moveq   #$0F,d0
        and.b   0(a6,a1.l),d0
        addq.w  #1,a1
L3EC0   addi.b  #'0',d0
        cmpi.b  #'9',d0
        bls.s   L3ECC
        addq.b  #7,d0
L3ECC   move.b  d0,0(a6,a0.l)
        addq.w  #1,a0
        rts

;Vector $F8 CN_ITOBL - Convert binary long to ASCII

L3ED4   bsr     L3ED8                   ;cn_itobw

;Vector $F6 CN_ITOBW - Convert binary word to ASCII

L3ED8   bsr     L3EDC                   ;cn_itobb

;Vector $F2 CN_ITOBB - Convert binary byte to ASCII

L3EDC   moveq   #7,d0
L3EDE   btst    d0,0(a6,a1.l)
        seq     0(a6,a0.l)
        addi.b  #'1',0(a6,a0.l)
        addq.w  #1,a0
        dbf     d0,L3EDE
        addq.w  #1,a1
        rts

;Vector $F0 CN_FTOD - Convert floating point to ASCII

L3EF6   movem.l d2-d5,-(a7)
        move.l  a0,-(a7)                ;pointer to buffer for result
        tst.l   2(a6,a1.l)              ;if mantissa has no bits set
        beq     L4000                   ;then number has no magnitude
        moveq   #6,d4
        tst.b   2(a6,a1.l)
        bge.s   L3F18                   ;number is positive
        move.b  #'-',0(a6,a0.l)         ;else stuff negative sign
        addq.w  #1,a0                   ;as part of result
        jsr     L4A0C(pc)               ;ri.neg, now make it postive
L3F18   cmpi.l  #$081B5F60,0(a6,a1.l)
        blt.s   L3F2C
        addq.w  #1,d4
        bsr.s   L3F40
        jsr     L497E(pc)               ;ri.div
        bra.s   L3F18

L3F2C   cmpi.l  #$08184C4C,0(a6,a1.l)
        bge.s   L3F6C
        subq.w  #1,d4
        bsr.s   L3F40
        jsr     L48DE(pc)               ;ri.mult
        bra.s   L3F2C

L3F40   subq.w  #6,a1
        move.l  #$08045000,0(a6,a1.l)   ;fp[10]
        clr.w   4(a6,a1.l)
        rts

L3F50   moveq   #0,d0
        swap    d1
        move.w  d1,d0
        divu.w  #10,d0
        swap    d0
        move.w  d0,d1
        swap    d1
        divu.w  #10,d1
        move.w  d1,d0
        swap    d1
        exg     d0,d1
        rts

L3F6C   moveq   #31,d0
        sub.w   0(a6,a1.l),d0
        move.l  2(a6,a1.l),d1
        lsr.l   d0,d1
        addq.w  #6,a1
L3F7A   addq.l  #5,d1
        bsr.s   L3F50
        addq.w  #1,d4
        cmpi.l  #$00989680,d1
        bge.s   L3F7A
        addq.w  #8,a0
        moveq   #6,d2
L3F8C   bsr.s   L3F50
        addi.b  #'0',d0
        subq.w  #1,a0
        move.b  d0,0(a6,a0.l)
        dbf     d2,L3F8C
        cmpi.w  #5,d4
        bgt.s   L3FAC
        cmpi.w  #-1,d4
        blt.s   L3FAC
        moveq   #0,d5
        bra.s   L3FB0

L3FAC   move.l  d4,d5
        moveq   #0,d4
L3FB0   addq.w  #1,d4
        move.l  a0,d2
        bra.s   L3FBE

L3FB6   move.b  0(a6,a0.l),-1(a6,a0.l)
        addq.w  #1,a0
L3FBE   dbf     d4,L3FB6
        move.b  #'.',-1(a6,a0.l)
        movea.l d2,a0
        addq.w  #7,a0
L3FCC   subq.w  #1,a0
        cmpi.b  #'0',0(a6,a0.l)
        beq.s   L3FCC
        cmpi.b  #'.',0(a6,a0.l)
        beq.s   L3FE0
        addq.w  #1,a0
L3FE0   tst.w   d5
        beq.s   L3FF6
        move.b  #'E',0(a6,a0.l)
        addq.w  #1,a0
        subq.w  #2,a1
        move.w  d5,0(a6,a1.l)
        jsr     L3E54(pc)               ;cn_itod
L3FF6   move.l  a0,d1
        sub.l   (a7)+,d1                ;length of result in buffer
        movem.l (a7)+,d2-d5
        rts

L4000   move.b  #'0',0(a6,a0.l)         ;mantissa was zero
        addq.w  #1,a0                   ;result has length one
        addq.w  #6,a1                   ;skip float value
        bra.s   L3FF6                   ;return result

L400C   movem.l d0/d5,-(a7)
        cmpi.w  #10,d0
        bge.s   L4034
L4016   subq.w  #1,d5
        ble.s   L4024
        move.b  #'0',0(a6,a1.l)
        addq.l  #1,a1
        bra.s   L4016

L4024   addi.w  #0+'0',d0
        move.b  d0,0(a6,a1.l)
        addq.l  #1,a1
        movem.l (a7)+,d0/d5
        rts

L4034   andi.l  #$0000FFFF,d0
        divu.w  #10,d0
        subq.w  #1,d5
        bsr.s   L400C
        swap    d0
        bra.s   L4024

L4046   bsr.s   L400C
        move.b  #' ',0(a6,a1.l)
        addq.l  #1,a1
        rts

L4052   bsr.s   L400C
        move.b  #':',0(a6,a1.l)
        addq.l  #1,a1
        rts

;Vector $EC CN_DATE - Get date string

L405E   movem.l d1-d5/a2,-(a7)
        bsr     L40F6
        suba.l  #22,a1
        move.w  #20,0(a6,a1.l)
        addq.l  #2,a1
        move.w  d2,d0
        moveq   #4,d5
        bsr.s   L4046
        mulu.w  #3,d4
        movea.l sv_mgtab+sv_base,a2
        move.w  $3A(a2),d0
        lea     0(a2,d0.w),a2
        adda.w  d4,a2
        bsr.s   L40E8
        move.b  #' ',0(a6,a1.l)
        addq.l  #1,a1
        moveq   #2,d5
        move.w  d1,d0
        bsr.s   L4046
        swap    d2
        move.w  d2,d0
        bsr.s   L4052
        move.w  d3,d0
        bsr.s   L4052
        swap    d3
        move.w  d3,d0
        bsr     L400C
        suba.l  #22,a1
L40B6   movem.l (a7)+,d1-d5/a2
        moveq   #err.ok,d0
        rts

;Vector $EE CN_DAY - Get day string

L40BE   movem.l d1-d5/a2,-(a7)
        bsr.s   L40F6
        subq.l  #6,a1
        move.w  #3,0(a6,a1.l)
        addq.l  #2,a1
        mulu.w  #3,d0
        movea.l sv_mgtab+sv_base,a2
        move.w  $38(a2),d1
        lea     0(a2,d1.w),a2
        adda.w  d0,a2
        bsr.s   L40E8
        subq.l  #5,a1
        bra.s   L40B6

L40E8   moveq   #2,d0
L40EA   move.b  (a2)+,0(a6,a1.l)
        addq.l  #1,a1
        dbf     d0,L40EA
        rts

L40F6   move.w  #60,d2
        bsr     L417E
        move.w  d0,d3
        swap    d3
        bsr.s   L417E
        move.w  d0,d3
        divu.w  #24,d1
        move.l  d1,d2
        andi.l  #$0000FFFF,d1
        move.l  d1,d0
        divu.w  #7,d0
        swap    d0
        divu.w  #1461,d1
        move.w  d1,d2
        asl.w   #2,d2
        addi.w  #1961,d2
        clr.w   d1
        swap    d1
        divu.w  #365,d1
        moveq   #0,d4
        cmpi.w  #4,d1
        bne.s   L413C
        subq.w  #1,d1
        move.w  #365,d4
L413C   add.w   d1,d2
        swap    d1
        add.w   d4,d1
        moveq   #0,d5
        move.w  d2,d4
        andi.w  #$0003,d4
        bne.s   L414E
        moveq   #1,d5
L414E   move.w  d5,d4
        addi.w  #58,d4
        cmp.w   d4,d1
        ble.s   L415C
        addq.w  #2,d1
        sub.w   d5,d1
L415C   move.w  d1,d5
        addi.w  #92,d5
        mulu.w  #100,d5
        divu.w  #3055,d5
        move.w  d5,d4
        addi.w  #92,d1
        mulu.w  #3055,d5
        divu.w  #100,d5
        sub.w   d5,d1
        subq.w  #3,d4
        rts

L417E   moveq   #0,d0
        swap    d1
        move.w  d1,d0
        divu.w  d2,d0
        swap    d0
        move.w  d0,d1
        swap    d1
        divu.w  d2,d1
        move.w  d1,d0
        swap    d1
        exg     d0,d1
        rts

L4196   move.l  a6,-(a7)
        suba.l  a6,a6
        bsr.s   L41AC                   ;ri_exec
        bra.s   L41A4

L419E   move.l  a6,-(a7)
        suba.l  a6,a6
        bsr.s   L41B4                   ;ri_execb
L41A4   beq.s   L41A8
        moveq   #0,d2
L41A8   movea.l (a7)+,a6
        rts

;Vector $11C RI_EXEC - Do maths operation

L41AC   movem.l d1-d3/a0/a2-a3/a5,-(a7)
        suba.l  a5,a5
        bra.s   L41C0

;Vector $11E RI_EXECB - Do a list of maths operations

L41B4   movem.l d1-d3/a0/a2-a3/a5,-(a7)
        movea.l a3,a5
L41BA   moveq   #0,d0
        move.b  (a5)+,d0
        beq.s   L41F8
L41C0   cmpi.b  #ri.powfp,d0
        bhi.s   L41CC
        bsr.s   L41FE
        bne.s   L41F8
        bra.s   L41BA

L41CC   ori.w   #$FF00,d0
        bclr    #0,d0
        adda.w  d0,a4
        bne.s   L41DE
        subq.w  #6,a1
        bsr.s   L41EA
        bra.s   L41E6

L41DE   exg     a4,a1
        bsr.s   L41EA
        exg     a1,a4
        addq.w  #6,a1
L41E6   suba.w  d0,a4
        bra.s   L41BA

L41EA   move.w  0(a6,a4.l),0(a6,a1.l)
        move.l  2(a6,a4.l),2(a6,a1.l)
        rts

L41F8   movem.l (a7)+,d1-d3/a0/a2-a3/a5
        rts

L41FE   move.w  L4206-2(pc,d0.w),d0
        jmp     L4206(pc,d0.w)

L4206   dc.w    L4796-L4206     ;$02 ri.nint
        dc.w    L479A-L4206     ;$04 ri.int
        dc.w    L47A6-L4206     ;$06 ri.nlint
        dc.w    L47B8-L4206     ;$08 ri.float
        dc.w    L4838-L4206     ;$0A ri.add
        dc.w    L482A-L4206     ;$0C ri.sub
        dc.w    L48DE-L4206     ;$0E ri.mult
        dc.w    L497E-L4206     ;$10 ri.div
        dc.w    L4A06-L4206     ;$12 ri.abs
        dc.w    L4A0C-L4206     ;$14 ri.neg
        dc.w    L4A4A-L4206     ;$16 ri.dup
        dc.w    L423E-L4206     ;$18 ri.cos
        dc.w    L4236-L4206     ;$1A ri.sin
        dc.w    L4262-L4206     ;$1C ri.tan
        dc.w    L426A-L4206     ;$1E ri.cot
        dc.w    L42F2-L4206     ;$20 ri.asin
        dc.w    L42E4-L4206     ;$22 ri.acos
        dc.w    L4326-L4206     ;$24 ri.atan
        dc.w    L431E-L4206     ;$26 ri.acot
        dc.w    L452C-L4206     ;$28 ri.sqrt
        dc.w    L4446-L4206     ;$2A ri.ln
        dc.w    L442C-L4206     ;$2C ri.log10
        dc.w    L44DE-L4206     ;$2E ri.exp
        dc.w    L43C2-L4206     ;$30 ri.powfp

;RI operation $1A RI.SIN - Sine

L4236   movem.l d4-d7/a4,-(a7)
        moveq   #0,d7
        bra.s   L4248

;RI operation $18 RI.COS - Cosine

L423E   movem.l d4-d7/a4,-(a7)
        jsr     L4A06(pc)               ;ri.abs
        moveq   #-1,d7
L4248   jsr     L4684(pc)
        bne.s   L42B0
        bsr.s   L42B6
        lea     L4588+38(pc),a4
        bsr.s   L42C6
        btst    #0,d7
        beq.s   L42B0
        jsr     L4A0C(pc)               ;ri.neg
        bra.s   L42B0

;RI operation $1C RI.TAN - Tangent

L4262   movem.l d4-d7/a4,-(a7)
        moveq   #0,d6
        bra.s   L4274

;RI operation $1E RI.COT - Cotangent

L426A   movem.l d4-d7/a4,-(a7)
        moveq   #-1,d6
        jsr     L4A0C(pc)               ;ri.neg
L4274   moveq   #0,d7
        jsr     L4674(pc)
        bne.s   L42B0
        eor.b   d6,d7
        bsr.s   L42B6
        lea     L45AE+40(pc),a4
        jsr     L4726(pc)
        addq.w  #6,a1
        bsr.s   L42CA
        subq.w  #6,a1
        move.l  -10(a6,a1.l),2(a6,a1.l)
        move.w  -12(a6,a1.l),0(a6,a1.l)
        btst    #0,d7
        beq.s   L42AA
        jsr     L4A5C(pc)
        dc.w    $4EB9,0,L4A0C    *** jsr L4A0C  ;
L42AA   jsr     L497E(pc)                       ;ri.div
L42AE   moveq   #err.ok,d0
L42B0   movem.l (a7)+,d4-d7/a4
        rts

L42B6   jsr     L4A4A(pc)               ;ri.dup
L42BA   jsr     L4A4A(pc)               ;ri.dup
        jsr     L4A4A(pc)               ;ri.dup
        jmp     L48DE(pc)               ;ri.mult

L42C6   jsr     L472C(pc)
L42CA   jsr     L48DE(pc)               ;ri.mult
        jmp     L4838(pc)               ;ri.add

L42D2   subq.w  #6,a1
        clr.w   4(a6,a1.l)
        move.l  #$08014000,0(a6,a1.l)   ;fp[1]
        jmp     L4A5C(pc)

;RI operation $22 RI.ACOS - Arccosine

L42E4   jsr     L4A0C(pc)               ;ri.neg
        bsr.s   L42F2                   ;ri.asin
        bne.s   L431C
        bsr.s   L430C
        jmp     L4838(pc)               ;ri.add

;RI operation $20 RI.ASIN - Arcsine

L42F2   bsr.s   L42BA
        bsr.s   L42D2
        jsr     L482A(pc)               ;ri.sub
        bsr     L452C                   ;ri.sqrt
        bne.s   L431C
        jsr     L497E(pc)               ;ri.div
        beq.s   L4326                   ;ri.atan
        bsr.s   L430C
        jmp     L48DE(pc)               ;ri.mult

L430C   subq.w  #6,a1
        move.l  #$6487ED51,2(a6,a1.l)
        move.w  #$0801,0(a6,a1.l)       ;PI/2 fp[1.570796]
L431C   rts

;RI operation $26 RI.ACOT - Arccotangent

L431E   movem.l d4-d7/a4,-(a7)
        moveq   #2,d7
        bra.s   L432C

;RI operation $24 RI.ATAN - Arctangent

L4326   movem.l d4-d7/a4,-(a7)
        moveq   #0,d7
L432C   tst.b   2(a6,a1.l)
        bge.s   L4338
        addq.b  #4,d7
        jsr     L4A0C(pc)               ;ri.neg
L4338   cmpi.w  #$0800,0(a6,a1.l)
        ble.s   L434A
        bsr.s   L42D2
        jsr     L497E(pc)               ;ri.div
        bchg    #1,d7
L434A   jsr     L4A4A(pc)               ;ri.dup
        subq.w  #6,a1
        move.l  #$4498517A,2(a6,a1.w)
        move.w  #$07FF,0(a6,a1.l)       ;fp[0.2679492]
        jsr     L482A(pc)               ;ri.sub
        addq.w  #6,a1
        tst.b   -4(a6,a1.l)
        ble.s   L4378
        lea     L45D6+28(pc),a4
        jsr     L4726(pc)
        jsr     L497E(pc)               ;ri.div
        addq.b  #1,d7
L4378   bsr     L42B6
        lea     L45F2+40(pc),a4
        jsr     L4726(pc)
        jsr     L497E(pc)               ;ri.div
        bsr     L42CA
        lsr.b   #1,d7
        bcc.s   L43A4
        subq.w  #6,a1
        move.l  #$430548E1,2(a6,a1.l)
        move.w  #$0800,0(a6,a1.l)       ;fp[0.5235988]
        jsr     L4838(pc)               ;ri.add
L43A4   lsr.b   #1,d7
        bcc.s   L43B4
        jsr     L4A0C(pc)               ;ri.neg
        bsr     L430C
        jsr     L4838(pc)               ;ri.add
L43B4   lsr.b   #1,d7
        bcc     L42AE
        jsr     L4A0C(pc)               ;ri.neg
        bra     L42B0

;RI operation $30 RI.POWFP - Take NOS ^ TOS

L43C2   move.w  0(a6,a1.l),d1
        beq.s   L43E6
        move.w  #$080F,d0
        sub.w   d1,d0
        blt.s   L43F0
        cmpi.w  #$000F,d0
        bgt.s   L43F0
        move.l  2(a6,a1.l),d1
        tst.w   d1
        bne.s   L43F0
        asr.l   d0,d1
        tst.w   d1
        bne.s   L43F0
        swap    d1
L43E6   addq.w  #4,a1
        move.w  d1,0(a6,a1.l)
        jmp     L47DC(pc)

L43F0   movem.l d4-d7/a4,-(a7)
        move.w  0(a6,a1.l),d4
        move.l  2(a6,a1.l),d5
        addq.w  #6,a1
        tst.b   2(a6,a1.l)
        beq.s   L4420
        bsr.s   L4446                   ;ri.ln
        bne     L42B0
        subq.w  #6,a1
        move.l  d5,2(a6,a1.l)
        move.w  d4,0(a6,a1.l)
        jsr     L48DE(pc)               ;ri.mult
        bne     L42B0
        bra     L44E4

L4420   tst.b   -4(a6,a1.l)
        bge     L42AE
        bra     L44D8

;RI operation $2C RI.LOG10 - Base 10 logs

L442C   bsr.s   L4446                   ;ri.ln
        bne.s   L4444
        subq.w  #6,a1
        move.l  #$6F2DEC55,2(a6,a1.l)
        move.w  #$07FF,0(a6,a1.l)       ;fp[0.4342945]
        jsr     L48DE(pc)               ;ri.mult
L4444   rts

;RI operation $2A RI.LN - Natural logs

L4446   movem.l d4-d7/a4,-(a7)
        move.w  0(a6,a1.l),d4
        move.l  2(a6,a1.l),d5
        addq.w  #6,a1
        ble     L44D8
        move.w  #$0800,d0
        move.l  d5,d1
        lsr.l   #1,d5
        cmpi.l  #$5A82799A,d1
        bgt.s   L4476
        subq.w  #1,d4
        addi.l  #$20000000,d5
        bclr    #30,d1
        bra.s   L447E

L4476   bset    #30,d5
        bset    #31,d1
L447E   tst.l   d1
        jsr     L4830(pc)
        subq.w  #6,a1
        move.l  d5,2(a6,a1.l)
        move.w  #$0800,0(a6,a1.l)
        jsr     L497E(pc)               ;ri.div
        bsr     L42B6
        jsr     L4A4A(pc)               ;ri.dup
        lea     L461A+28(pc),a4
        move.w  d4,d7
        jsr     L4726(pc)
        jsr     L497E(pc)               ;ri.div
        jsr     L48DE(pc)               ;ri.mult
        bsr     L42CA
        subi.w  #$0800,d7
        subq.w  #2,a1
        move.w  d7,0(a6,a1.l)
        jsr     L47B8(pc)               ;ri.float
        subq.w  #6,a1
        move.l  #$58B90BFC,2(a6,a1.l)
        move.w  #$0800,0(a6,a1.l)       ;fp[0.6931472]
        bsr     L42CA
        bra     L42AE

L44D8   moveq   #err_ov,d0
L44DA   bra     L42B0

;RI operation $2E RI.EXP - Exponential

L44DE   movem.l d4-d7/a4,-(a7)
        moveq   #0,d7
L44E4   jsr     L4692(pc)
        bne.s   L44DA
        bsr     L42BA
        lea     L4636+34(pc),a4
        jsr     L4726(pc)
        addq.w  #6,a1
        jsr     L48DE(pc)               ;ri.mult
        jsr     L4A4A(pc)               ;ri.dup
        subq.w  #6,a1
        jsr     L4A5C(pc)
        jsr     L482A(pc)               ;ri.sub
        jsr     L497E(pc)               ;ri.div
        subq.w  #6,a1
        move.l  #$40000000,2(a6,a1.l)
        move.w  #$0800,0(a6,a1.l)       ;fp[0.5]
        jsr     L4838(pc)               ;ri.add
        addq.w  #1,d7
        add.w   d7,0(a6,a1.l)
        bra     L42AE

;RI operation $28 RI.SQRT - Square root

L452C   movem.l d4-d7/a4,-(a7)
        move.w  0(a6,a1.l),d6
        tst.l   2(a6,a1.l)
        beq     L42AE
        blt.s   L44D8
        lea     L4658+14(pc),a4
        move.w  d6,d7
        subi.w  #$0800,d7
        sub.w   d7,0(a6,a1.l)
        asr.w   #1,d7
        bcc.s   L4554
        lea     L4666+14(pc),a4
L4554   swap    d6
        jsr     L472C(pc)
        swap    d6
        add.w   d7,0(a6,a1.l)
        moveq   #1,d7
L4562   jsr     L4A4A(pc)               ;ri.dup
        subq.w  #6,a1
        move.l  d5,2(a6,a1.l)
        move.w  d6,0(a6,a1.l)
        jsr     L4A5C(pc)
        jsr     L497E(pc)               ;ri.div
        jsr     L4838(pc)               ;ri.add
        subq.w  #1,0(a6,a1.l)
        dbf     d7,L4562
        bra     L42AE

;The following float point data tables are in reverse order.

;Data for ri.sin and ri.cos

L4588   dc.w    $0000
        dc.l    $00000000               ;fp[0]
        dc.w    $07FE
        dc.l    $AAAAAAB0               ;fp[-0.1666667]
        dc.w    $07FA
        dc.l    $444442DD               ;fp[8.333331E-3]
        dc.w    $07F4
        dc.l    $97FA15C1               ;fp[-1.984083E-4]
        dc.w    $07EE
        dc.l    $5C5AE940               ;fp[2.752397E-6]
        dc.w    $07E7
        dc.l    $997C79C0               ;fp[-2.386835E-8]
        dc.w    5                       ;6 numbers above

;Data for ri.tan and ri.cot

L45AE   dc.w    $0801
        dc.l    $40000000               ;fp[1]
        dc.w    $07FF
        dc.l    $8E287BC1               ;fp[-0.4446948]
        dc.w    $07FB
        dc.l    $416D50CD               ;fp[1.597339E-2]
        dc.w    2                       ;3 numbers above
        dc.l    $0000
        dc.w    $00000000               ;fp[0]
        dc.w    $07FD
        dc.l    $8DF7443E               ;fp[-0.1113614]
        dc.w    $07F7
        dc.l    $46761A70               ;fp[1.075155E-3]
        dc.w    2                       ;3 numbers above

;Data for ri.atan

L45D6   dc.w    $0801
        dc.l    $6ED9EBA1               ;fp[1.732051]
        dc.w    $0801
        dc.l    $40000000               ;fp[1]
        dc.w    1                       ;2 numbers above
        dc.w    $0800
        dc.l    $80000000               ;fp[-1]
        dc.w    $0801
        dc.l    $6ED9EBA1               ;fp[1.732051]
        dc.w    1                       ;2 numbers above

L45F2   dc.w    $0803
        dc.l    $451FBEDF               ;fp[4.32025]
        dc.w    $0803
        dc.l    $4C091DF8               ;fp[4.752226]
        dc.w    $0801
        dc.l    $40000000               ;fp[1]
        dc.w    2                       ;3 numbers above
        dc.w    $0000
        dc.l    $00000000               ;fp[0]
        dc.w    $0801
        dc.l    $A3D5AC3B               ;fp[-1.440083]
        dc.w    $0800
        dc.l    $A3D62904               ;fp[-0.7200268]
        dc.w    2                       ;3 numbers above

;Data for ri.ln

L461A   dc.w    $0803
        dc.l    $A6BCEEE1               ;fp[-5.578874]
        dc.w    $0801
        dc.l    $40000000               ;fp[1]
        dc.w    1                       ;2 numbers above
        dc.w    $07FF
        dc.l    $88FBE7C1               ;fp[-0.4649062]
        dc.w    $07FA
        dc.l    $6F6B44F3               ;fp[1.360095E-2]
        dc.w    1                       ;2 numbers above

;Data for ri.exp

L4636   dc.w    $0800
        dc.l    $40000000               ;fp[0.5]
        dc.w    $07FC
        dc.l    $6DB4CE83               ;fp[5.356752E-2]
        dc.w    $07F5
        dc.l    $4DEF09CA               ;fp[2.972936E-4]
        dc.w    2                       ;3 numbers above
        dc.w    $07FF
        dc.l    $40000000               ;fp[0.25]
        dc.w    $07F9
        dc.l    $617DE4BA               ;fp[5.950426E-3]
        dc.w    1                       ;2 numbers above

;Data for ri.sqrt

L4658   dc.w    $07FF
        dc.l    $6AD4D402               ;fp[0.41731]
        dc.w    $0800
        dc.l    $4B8A5CE6               ;fp[0.59016]
        dc.w    1                       ;2 numbers above

L4666   dc.w    $0800
        dc.l    $4B8A5CE6               ;fp[0.59016]
        dc.w    $0800
        dc.l    $6AD4D402               ;fp[0.83462]
        dc.w    1                       ;2 numbers above

L4674   addq.w  #1,0(a6,a1.l)
        bsr.s   L4684
        bne.s   L46EE
        subq.w  #1,0(a6,a1.l)
        moveq   #err.ok,d0
        rts

L4684   lea     L4702+18(pc),a4
        cmpi.w  #$0810,0(a6,a1.l)
        bgt.s   L46EC
        bra.s   L469E

L4692   lea     L4714+18(pc),a4
        cmpi.w  #$0809,0(a6,a1.l)
        bgt.s   L46EC
L469E   jsr     L4A4A(pc)               ;ri.dup
        subq.w  #6,a1
        move.l  -(a4),2(a6,a1.l)
        move.w  -(a4),0(a6,a1.l)
        jsr     L48DE(pc)               ;ri.mult
        tst.b   d7
        bne.s   L46C0
        jsr     L4796(pc)               ;ri.nint
        move.w  d1,d7
        jsr     L47B8(pc)               ;ri.float
        bra.s   L46D6

L46C0   jsr     L479A(pc)               ;ri.int
        add.w   d1,d7
        add.w   d1,0(a6,a1.l)
        addq.w  #1,0(a6,a1.l)
        jsr     L47B8(pc)               ;ri.float
        subq.w  #1,0(a6,a1.l)
L46D6   move.w  0(a6,a1.l),d4
        move.l  2(a6,a1.l),d5
        bsr.s   L46F0
        subq.w  #6,a1
        move.l  d5,2(a6,a1.l)
        move.w  d4,0(a6,a1.l)
        bra.s   L46F0

L46EC   moveq   #err.ov,d0
L46EE   rts

L46F0   subq.w  #6,a1
        move.l  -(a4),2(a6,a1.l)
        move.w  -(a4),0(a6,a1.l)
        jsr     L48DE(pc)               ;ri.mult
        jmp     L482A(pc)               ;ri.sub

;Data common to ri.sin, ri.cos, ri.tan and ri.cot

L4702   dc.w    $07F0
        dc.l    $B54442D1               ;fp[-8.90891E-6]
        dc.w    $0802
        dc.l    $64880000               ;fp[3.141602]
        dc.w    $07FF
        dc.l    $517CC1B7               ;fp[0.3183099]

;Data for ri.exp

L4714   dc.w    $07F4
        dc.l    $90BFBE8F               ;fp[-2.121944E-4]
        dc.w    $0800
        dc.l    $58C00000               ;fp[0.6933594]
        dc.w    $0801
        dc.l    $5C551D95               ;fp[1.442695]

L4726   bsr.s   L472C
        subq.w  #6,a1
        bra.s   L4734

L472C   move.w  0(a6,a1.l),d4
        move.l  2(a6,a1.l),d5
L4734   move.w  -(a4),d6
        move.l  -(a4),2(a6,a1.l)
        move.w  -(a4),0(a6,a1.l)
L473E   subq.w  #6,a1
        move.l  d5,2(a6,a1.l)
        move.w  d4,0(a6,a1.l)
        jsr     L48DE(pc)               ;ri.mult
        subq.w  #6,a1
        move.l  -(a4),2(a6,a1.l)
        move.w  -(a4),0(a6,a1.l)
        jsr     L4838(pc)               ;ri.add
        subq.w  #1,d6
        bgt.s   L473E
        rts

L4760   subq.w  #6,a1
        clr.w   4(a6,a1.l)
        move.l  #$08004000,0(a6,a1.l)   ;fp[0.5]
        jsr     L4838(pc)               ;ri.add
L4772   move.w  0(a6,a1.l),d0
        move.l  2(a6,a1.l),d1
        addq.w  #2,a1
        clr.l   0(a6,a1.l)
        subi.w  #$0800,d0
        bge.s   L4788
        moveq   #0,d0
L4788   subi.w  #$001F,d0
        neg.w   d0
        asr.l   d0,d1
        move.l  d1,0(a6,a1.l)
        rts

;RI operation $02 RI.NINT - INT floating point form into word integer

L4796   bsr.s   L4760
        bra.s   L479C

;RI operation $04 RI.INT - Truncate floating point form into word integer

L479A   bsr.s   L4772
L479C   addq.l  #2,a1
        cmpi.w  #$0010,d0
        blt.s   L47B4
        bra.s   L47B0

;RI operation $06 RI.NLINT - INT floating point form into long integer

L47A6   bsr.s   L4760
        bra.s   L47AC

L47AA   bsr.s   L4772
L47AC   tst.w   d0
        blt.s   L47B4
L47B0   moveq   #err.ok,d0
        rts

L47B4   moveq   #err.ov,d0
        rts

;RI operation $08 RI.FLOAT - Convert integer word into floating point form

L47B8   move.w  #$081F,d0
        move.w  0(a6,a1.l),d1
        addq.w  #2,a1
        ext.l   d1
        jmp     L4830(pc)

L47C8   subq.w  #6,a1
        clr.w   4(a6,a1.l)
        move.l  #$08014000,0(a6,a1.l)   ;fp[0.5]
        jsr     L4A5C(pc)
        rts

L47DC   movem.l d4-d6,-(a7)
        move.w  0(a6,a1.l),d6
        addq.w  #2,a1
        bge.s   L47F2
        neg.w   d6
        bsr.s   L47C8
        jsr     L497E(pc)               ;ri.div
        bne.s   L4824
L47F2   bsr.s   L47C8
L47F4   lsr.w   #1,d6
        bcc.s   L4810
        move.w  0(a6,a1.l),d5
        move.l  2(a6,a1.l),d4
        jsr     L48DE(pc)               ;ri.mult
        subq.w  #6,a1
        bne.s   L4822
        move.l  d4,2(a6,a1.l)
        move.w  d5,0(a6,a1.l)
L4810   tst.w   d6
        beq.s   L4820
        jsr     L4A4A(pc)               ;ri.dup
        jsr     L48DE(pc)               ;ri.mult
        bne.s   L4822
        bra.s   L47F4

L4820   moveq   #err.ok,d0
L4822   addq.w  #6,a1
L4824   movem.l (a7)+,d4-d6
        rts

;RI operation $0C RI.SUB - Subtract TOS from NOS

L482A   jsr     L4A0C(pc)               ;ri.neg
        bra.s   L4838                   ;ri.add

L4830   subq.w  #6,a1
        moveq   #0,d2
        tst.l   d1
        bra.s   L4870

;RI operation $0A RI.ADD - Add TOS to NOS

L4838   addq.w  #6,a1
        move.w  0(a6,a1.l),d0
        sub.w   -6(a6,a1.l),d0
        bge.s   L485C
        neg.w   d0
        cmpi.w  #$0020,d0
        bge.s   L48AE
        move.l  2(a6,a1.l),d1
        bsr.s   L48C2
        move.w  -6(a6,a1.l),d0
        add.l   -4(a6,a1.l),d1
        bra.s   L4870

L485C   cmpi.w  #$0020,d0
        bge.s   L48BE
        move.l  -4(a6,a1.l),d1
        bsr.s   L48C2
        move.w  0(a6,a1.l),d0
        add.l   2(a6,a1.l),d1
L4870   bvs.s   L4898
        beq.s   L48AA
        move.l  d1,d3
        add.l   d3,d3
        bvs.s   L48B6
        sub.l   d2,d3
        bvc.s   L4880
        add.l   d2,d3
L4880   subq.w  #1,d0
        move.l  d3,d1
        moveq   #16,d2
L4886   move.l  d1,d3
        asl.l   d2,d3
        bvs.s   L4892
        move.l  d3,d1
        sub.w   d2,d0
        blt.s   L48A6
L4892   asr.l   #1,d2
        bne.s   L4886
        bra.s   L48B6

L4898   roxr.l  #1,d1
        addq.w  #1,d0
        btst    #12,d0
        beq.s   L48B6
        moveq   #err.ov,d0
        rts

L48A6   neg.w   d0
        asr.l   d0,d1
L48AA   clr.w   d0
        bra.s   L48B6

L48AE   move.w  -6(a6,a1.l),d0
        move.l  -4(a6,a1.l),d1
L48B6   move.l  d1,2(a6,a1.l)
        move.w  d0,0(a6,a1.l)
L48BE   moveq   #err.ok,d0
        rts

L48C2   moveq   #0,d2
        tst.w   d0
        beq.s   L48DC
        asr.l   d0,d1
        bcc.s   L48DC
        addq.l  #1,d1
        moveq   #1,d2
        subq.w  #1,d0
        bgt.s   L48DC
        bclr    #0,d1
        beq.s   L48DC
        moveq   #-$01,d2
L48DC   rts

;RI operation $0E RI.MULT - Multiply TOS by NOS

L48DE   movem.l d4-d6,-(a7)
        sf      d5
        sf      d6
        move.l  2(a6,a1.l),d3
        bge.s   L48F4
        jsr     L4A0C(pc)               ;ri.neg
        move.l  d1,d3
        st      d6
L48F4   addq.w  #6,a1
        move.l  2(a6,a1.l),d1
        bgt.s   L4904
        beq.s   L496A
        jsr     L4A0C(pc)               ;ri.neg
        st      d5
L4904   lsl.l   #1,d1
        move.l  d1,d0
        swap    d0
        lsl.l   #1,d3
        move.l  d3,d2
        swap    d2
        move.w  d3,d4
        mulu.w  d1,d4
        clr.w   d4
        swap    d4
        mulu.w  d0,d3
        mulu.w  d2,d1
        add.l   d4,d3
        add.l   d3,d1
        move.w  d1,d4
        clr.w   d1
        swap    d1
        roxr.w  #1,d1
        roxl.l  #1,d1
        mulu.w  d0,d2
        move.w  0(a6,a1.l),d0
        add.w   -6(a6,a1.l),d0
        subi.w  #$0800,d0
        blt.s   L4956
        add.l   d2,d1
        bmi.s   L4948
        beq.s   L4956
        subq.w  #1,d0
        blt.s   L4956
        asl.w   #1,d4
        bra.s   L494A

L4948   lsr.l   #1,d1
L494A   moveq   #0,d4
        addx.l  d4,d1
        bpl.s   L495A
        lsr.l   #1,d1
        addq.w  #1,d0
        bra.s   L495A

L4956   clr.w   d0
        clr.l   d1
L495A   move.l  d1,2(a6,a1.l)
        move.w  d0,0(a6,a1.l)
        cmp.b   d5,d6
        beq.s   L496A
        jsr     L4A0C(pc)               ;ri.neg
L496A   movem.l (a7)+,d4-d6
        btst    #4,0(a6,a1.l)
        bne.s   L497A
        moveq   #err.ok,d0
        rts

L497A   moveq   #err.ov,d0
        rts

;RI operation $10 RI.DIV - Divide TOS into NOS

L497E   move.l  d4,-(a7)
        move.l  d5,-(a7)
        sf      d5
        move.l  2(a6,a1.l),d2
        bgt.s   L4994
        beq.s   L4A00
        jsr     L4A0C(pc)               ;ri.neg
        move.l  d1,d2
        st      d5
L4994   addq.w  #6,a1
        move.l  2(a6,a1.l),d1
        bgt.s   L49A4
        beq.s   L49F6
        jsr     L4A0C(pc)               ;ri.neg
        not.b   d5
L49A4   move.w  0(a6,a1.l),d0
        addi.w  #$0800,d0
        sub.w   -6(a6,a1.l),d0
        bge.s   L49B8
        clr.w   d0
        clr.l   d3
        bra.s   L49E6

L49B8   btst    #12,d0
        bne.s   L4A02
        moveq   #$1F,d4
        moveq   #0,d3
L49C2   sub.l   d2,d1
        bcs.s   L49CA
        bset    d4,d3
        bra.s   L49CC

L49CA   add.l   d2,d1
L49CC   add.l   d1,d1
        dbeq    d4,L49C2
        tst.l   d3
        blt.s   L49DE
        sub.l   d1,d2
        bhi.s   L49E6
        addq.l  #1,d3
        bvc.s   L49E6
L49DE   addq.w  #1,d0
        lsr.l   #1,d3
        moveq   #0,d1
        addx.l  d1,d3
L49E6   move.l  d3,2(a6,a1.l)
        move.w  d0,0(a6,a1.l)
        tst.b   d5
        beq.s   L49F6
        jsr     L4A0C(pc)               ;ri.neg
L49F6   moveq   #err.ok,d0
L49F8   move.l  (a7)+,d5
        move.l  (a7)+,d4
        tst.l   d0
        rts

L4A00   addq.w  #6,a1
L4A02   moveq   #err.ov,d0
        bra.s   L49F8

;RI operation $12 RI.ABS - Take positive value

L4A06   tst.b   2(a6,a1.l)
        bge.s   L4A46

;RI operation $14 RI.NEG - Negate

L4A0C   move.l  2(a6,a1.l),d1           ;get mantissa
        neg.l   d1
        bvs.s   L4A2C
        cmpi.l  #$C0000000,d1
        bne.s   L4A42
        lsl.l   #1,d1
        subq.w  #1,0(a6,a1.l)
        bge.s   L4A42
        asr.l   #1,d1
        clr.w   0(a6,a1.l)
        bra.s   L4A42

L4A2C   lsr.l   #1,d1
        addq.w  #1,0(a6,a1.l)           ;increment exponent
        btst    #4,0(a6,a1.l)
        beq.s   L4A42
        subq.w  #1,0(a6,a1.l)
        moveq   #-$01,d1
        lsr.l   #1,d1
L4A42   move.l  d1,2(a6,a1.l)
L4A46   moveq   #err.ok,d0
        rts

;RI operation $16 RI.DUP - Duplicate

L4A4A   subq.w  #6,a1
L4A4C   move.w  6(a6,a1.l),0(a6,a1.l)
        move.l  8(a6,a1.l),2(a6,a1.l)
        moveq   #err.ok,d0
        rts

L4A5C   move.w  0(a6,a1.l),d2
        move.l  2(a6,a1.l),d1
        jsr     L4A4C(pc)
        move.w  d2,6(a6,a1.l)
        move.l  d1,8(a6,a1.l)
        moveq   #err.ok,d0
        rts

;Bootstrap continuation

L4A74   bra.s   L4AAA

L4A76   cmpi.l  #$4AFB0001,(a3)         ;ROM header
        bne.s   L4A9E
        lea     8(a3),a1                ;banner text (if any)
        jsr     L39B2(pc)               ;ut_mtext
        move.w  4(a3),d0                ;BASIC procs/fns ?
        beq.s   L4A94                   ;none
        lea     0(a3,d0.w),a1
        jsr     L6DA6(pc)               ;bp_init
L4A94   move.w  6(a3),d0                ;initialisation routine ?
        beq.s   L4A9E                   ;none
        jsr     0(a3,d0.w)
L4A9E   rts

L4AA0   jsr     L39F6(pc)               ;ut_con
        move.l  d4,d1
        jmp     L6646(pc)

L4AAA   jsr     L566E(pc)               ;set up BASIC
        jsr     L6DA2(pc)               ;introduce BASIC keywords
        lea     L4BBA(pc),a1            ;ROM banner window definition block
        moveq   #0,d4                   ;BASIC channel #0
        bsr.s   L4AA0                   ;..open it
        movea.l #$0000C000,a3           ;ROM slot first
        bsr.s   L4A76
        movea.l #$000C0000,a3           ;768K boundary
L4AC8   bsr.s   L4A76
        adda.w  #$4000,a3               ;and every 16K boundary
        cmpa.l  #$00100000,a3           ;to 1M upper limit
        blt.s   L4AC8
        lea     L4BC6(pc),a1            ;copyright window definition block
        moveq   #1,d4                   ;BASIC channel #1
        bsr.s   L4AA0                   ;open it
        moveq   #err.cp,d0              ;copyright message
        jsr     L3968(pc)               ;ut_err
        lea     L4BD2(pc),a1            ;prompt window definition block
        moveq   #2,d4                   ;BASIC channel #2
        bsr.s   L4AA0                   ;open it
        moveq   #err.bt,d0              ;boot message
        jsr     L3968(pc)               ;ut_err
L4AF2   moveq   #io.fbyte,d0
        moveq   #forever,d3             ;wait for user to press F1 or F2
        trap    #3
        moveq   #0,d6                   ;mode 4
        moveq   #0,d7                   ;monitor mode
        moveq   #$20,d5
        subi.b  #$E8,d1
        beq.s   L4B0E                   ;F1 pressed
        subq.b  #4,d1
        bne.s   L4AF2                   ;F2 not pressed, try again
        moveq   #8,d6                   ;mode 8
        moveq   #1,d7                   ;TV mode
        moveq   #$44,d5
L4B0E   move.b  d6,d1
        move.b  d7,d2
        moveq   #mt.dmode,d0
        trap    #1
        lea     L4B72-$08(pc,d5.w),a1   ;redefine #2 window
        bsr.s   L4B6E
        movea.l #$00010001,a0           ;channel #1
        lea     L4B72-$14(pc,d5.w),a1   ;redefine #1 window
        bsr.s   L4B6E
        suba.l  a0,a0                   ;channel #0
        lea     L4B72-$20(pc,d5.w),a1   ;redefine #0 window
        bsr.s   L4B6E
        lea     L4BDE(pc),a0
        bsr.s   L4B46                   ;try to open to device 'BOOT'
        beq.s   L4B40
        lea     L4BE4(pc),a0
        bsr.s   L4B46                   ;try to open to 'MDV1_BOOT'
        bne.s   L4B52                   ;give up, use #0 for input
L4B40   clr.w   bv_nxlin(a6)            ;no line to start after
        bra.s   L4B54

L4B46   moveq   #io.open,d0
        moveq   #my.job,d1
        moveq   #io.old,d3
        trap    #2
        tst.l   d0
        rts

L4B52   suba.l  a0,a0
L4B54   move.l  (a6),bv_bfp(a6)         ;bv_bfbas, clear BASIC buffer
        moveq   #0,d7
        moveq   #126,d1
        jsr     L4E6A(pc)               ;reserve space in buffer
        move.l  a0,bv_comch(a6)
        lea     L4BF0(pc),a5
        move.l  a5,-(a7)
L4B6A   jmp     L4C04(pc)

L4B6E   jmp     L3A02(pc)

;Default Monitor windows

L4B72   dc.b    0,0,0,4
        dc.w    512,50,0,206            ;#0
        dc.b    255,1,2,7
        dc.w    256,202,256,0           ;#1
        dc.b    255,1,7,2
        dc.w    256,202,0,0             ;#2

;Default TV windows

        dc.b    0,0,0,7
        dc.w    448,40,32,216           ;#0
        dc.b    0,0,2,7
        dc.w    448,200,32,16           ;#1
        dc.b    0,0,1,7
        dc.w    448,200,32,16           ;#2

;Boot window definitions

L4BBA   dc.b    0,0,0,4
        dc.w    448,170,32,32           ;ROM banner window
L4BC6   dc.b    7,2,2,7
        dc.w    368,14,72,238           ;copyright window
L4BD2   dc.b    4,4,7,2
        dc.w    168,28,174,206          ;F1/F2 window

L4BDE   dc.w    4
        dc.b    'BOOT'
L4BE4   dc.w    9
        dc.b    'MDV1_BOOT',0

L4BF0   jsr     LA9BA(pc)
        lea     L4BF0(pc),a5
        move.l  a5,-(a7)
L4BFA   clr.l   bv_comch(a6)
        moveq   #0,d1
        jsr     L661E(pc)
L4C04   clr.l   bv_sssav(a6)
        move.l  (a6),bv_bfp(a6)         ;bv_bfbas
        tst.b   bv_auto(a6)
        beq.s   L4C2C
        move.w  bv_edlin(a6),d4
        move.w  d4,d6
        sf      bv_print(a6)
        jsr     L7518(pc)               ;(convert precompiled basic to ascii)
        move.w  bv_edinc(a6),d0
        sne     bv_auto(a6)
        add.w   d0,bv_edlin(a6)
L4C2C   move.l  a0,d0
        jsr     L79C4(pc)
        beq.s   L4C64
        bgt.s   L4C3C
        cmpi.b  #$F6,d0
        beq.s   L4C50
L4C3C   sf      bv_auto(a6)
        jsr     L9B9C(pc)
L4C44   bsr     L4CFA
        beq.s   L4C4E
        moveq   #io.close,d0
        trap    #2
L4C4E   bra.s   L4BFA

L4C50   moveq   #io.close,d0
        trap    #2
        clr.l   bv_comch(a6)
        tst.w   bv_nxlin(a6)
        blt     L4D6A
        bra     L4D9E

L4C64   tas     bv_brk(a6)
        move.l  a1,d1
        sub.l   (a6),d1                 ;bv_bfbas
        sf      bv_arrow(a6)
        move.b  -1(a6,a1.l),d0
        subi.b  #$D0,d0
        bcs.s   L4C86
        st      bv_arrow(a6)
        beq.s   L4C86
        move.b  #1,bv_arrow(a6)
L4C86   subq.w  #1,d1
        ble     L4C04
        cmpi.b  #' ',-2(a6,a1.l)
        bne.s   L4C98
        subq.w  #1,a1
        bra.s   L4C86

L4C98   move.b  #10,-1(a6,a1.l)
        move.l  a1,bv_bfp(a6)
L4CA2   jsr     L890C(pc)               ;(initialise basic stacks)
        lea     L8B5A(pc),a2            ;(commands syntax table)
        jsr     L87D4(pc)               ;(basic syntax analyser)
        beq.s   L4CD0
        blt.s   L4CB8
        jsr     L97DC(pc)
        bra.s   L4CA2

L4CB8   tst.l   bv_comch(a6)
        bne.s   L4CCC
        moveq   #$EB,d0
        jsr     L9B9C(pc)
        subq.l  #1,bv_bfp(a6)
        bra     L4C2C

L4CCC   jsr     L8A4E(pc)               ;(error when compiling)
L4CD0   jsr     L8AB4(pc)               ;(format precompiled basic line)
        jsr     L8E88(pc)               ;(store precomplied line)
        bra.s   L4D02

        sf      bv_sing(a6)
        st      bv_edit(a6)
        move.l  d0,d5
        bsr.s   L4CFA
        bne     L4C04
        moveq   #2,d1
        jsr     L661E(pc)
        blt.s   L4CF6
        jsr     L8FE6(pc)
L4CF6   bra     L4BFA

L4CFA   movea.l bv_comch(a6),a0
        move.l  a0,d0
        rts

L4D02   movea.l bv_tkbas(a6),a4
        move.b  #1,bv_stmnt(a6)
        sf      bv_inlin(a6)
        st      bv_cont(a6)
L4D14   st      bv_sing(a6)
        clr.l   bv_linum(a6)
        jsr     LA4BA(pc)
        jsr     LA8B8(pc)
        bne     L4C44
        tst.b   bv_comln(a6)
        beq.s   L4D5E
        subq.w  #4,bv_stopn(a6)
        beq.s   L4CF6
        blt.s   L4D14
        movea.l bv_tkbas(a6),a0
        move.l  bv_tkp(a6),d0
        suba.l  a0,a4
        sub.l   a0,d0
        move.l  d0,d1
        suba.l  d0,a7
L4D46   move.w  0(a6,a0.l),(a7)+
        addq.w  #2,a0
        subq.w  #2,d1
        bgt.s   L4D46
        suba.l  d0,a7
        move.w  d0,-(a7)
        move.w  a4,-(a7)
        move.b  bv_stmnt(a6),-(a7)
        move.l  bv_inlin(a6),-(a7)
L4D5E   tst.w   bv_nxlin(a6)
        bge.s   L4D98
        bsr.s   L4CFA
L4D66   bne     L4C04
L4D6A   tst.b   bv_comln(a6)
        beq.s   L4CF6
        sf      bv_comln(a6)
        move.l  (a7)+,bv_inlin(a6)
        move.b  (a7)+,bv_stmnt(a6)
        movea.l bv_tkbas(a6),a0
        movea.l a0,a4
        adda.w  (a7)+,a4
        move.w  (a7)+,d0
L4D86   move.w  (a7)+,0(a6,a0.l)
        addq.w  #2,a0
        subq.w  #2,d0
        bgt.s   L4D86
        move.l  a0,bv_tkp(a6)
        bra     L4D14

L4D98   bsr     L4CFA
        bne.s   L4D66
L4D9E   jsr     LA4BA(pc)
        movea.l bv_pfbas(a6),a4
        sf      bv_sing(a6)
        clr.l   bv_linum(a6)
        move.b  #1,bv_stmnt(a6)
        move.w  bv_nxlin(a6),d4
        beq.s   L4DD6
        jsr     L9FBE(pc)
        bne     L4BFA
        move.b  bv_nxstm(a6),d4
        beq.s   L4DD6
        jsr     LA96A(pc)
        jsr     LA00A(pc)
L4DD0   jsr     LA90C(pc)
        bra.s   L4DDA

L4DD6   jsr     LA8A8(pc)
L4DDA   bne     L4BFA
        tst.w   bv_stopn(a6)
        bne     L4D5E
        bra.s   L4DD0

L4DE8   jsr     L4E5E(pc)               ;reserve 8 bytes on name table stack
        movea.l bv_ntp(a6),a2
        addq.l  #8,bv_ntp(a6)
        rts

L4DF6   move.l  d1,-(a7)
        addq.l  #7,d1
        andi.w  #$FFF8,d1
L4DFE   movea.w #bv_vvfree,a0
        moveq   #mt.alloc,d0
        trap    #1
        tst.l   d0
        blt.s   L4E0C
        bra.s   L4E26

L4E0C   move.l  d1,-(a7)
        jsr     L4E76(pc)               ;reserve space on variable values stack
        movea.l bv_vvp(a6),a0
        add.l   d1,bv_vvp(a6)
        movea.w #bv_vvfree,a1
        moveq   #mt.lnkfr,d0
        trap    #1
        move.l  (a7)+,d1
        bra.s   L4DFE

L4E26   move.l  (a7)+,d1
        rts

L4E2A   dc.w    256,256,256,256

L4E32   lea     L4E2A(pc),a1
        add.w   d0,d0
        moveq   #0,d1
        move.w  0(a1,d0.w),d1
        moveq   #bv_ssp,d2
        move.l  a7,bv_ssp(a6)
        move.l  a6,d0
        sub.l   d0,bv_ssp(a6)
        bra.s   L4E90

L4E4C   moveq   #$20,d1

;Vector $11A BV_CHRIX - Reserve space on maths stack

L4E4E   moveq   #bv_rip,d2
        bra.s   L4E90

;Reserve space on Backtrack stack (parsing)

L4E52   moveq   #12,d1
L4E54   moveq   #bv_btp,d2
        bra.s   L4E90

;Reserve space on Temporary graph stack

L4E58   moveq   #4,d1
L4E5A   moveq   #bv_tgp,d2
        bra.s   L4E90

;Reserve space on Name table stack

L4E5E   moveq   #8,d1
L4E60   moveq   #bv_ntp,d2
        bra.s   L4E84

;Reserve space on Return table stack

L4E64   moveq   #22,d1
        moveq   #bv_rtp,d2
        bra.s   L4E84

;Reserve space in Buffer

L4E6A   moveq   #bv_bfp,d2
        bra.s   L4E84

;Reserve space on Token list stack

L4E6E   moveq   #bv_tkp,d2
        bra.s   L4E84

;Reserve space on Name list stack

L4E72   moveq   #bv_nlp,d2
        bra.s   L4E84

;Reserve space on Variable values stack

L4E76   moveq   #bv_vvp,d2
        bra.s   L4E84

;Reserve space in Channel table

L4E7A   moveq   #bv_chp,d2
        bra.s   L4E84

;Reserve space in Line number table

L4E7E   moveq   #bv_lnp,d2
        bra.s   L4E84

;Reserve space in Program file stack

L4E82   moveq   #bv_pfp,d2
L4E84   moveq   #0,d0
        move.l  4(a6,d2.l),d3
        sub.l   0(a6,d2.l),d3
        bra.s   L4E9A

L4E90   moveq   #-1,d0
        move.l  0(a6,d2.l),d3
        sub.l   -4(a6,d2.l),d3
L4E9A   cmp.l   d1,d3
        bge.s   L4EE2
        movem.l a0-a3,-(a7)
        addi.l  #15,d1
        andi.w  #$FFF0,d1
L4EAC   move.l  bv_btp(a6),d3
        sub.l   bv_lnp(a6),d3
        cmp.l   d1,d3
        bgt.s   L4F14
        movem.l d0-d2,-(a7)
        moveq   #mt.albas,d0
        trap    #1
        tst.l   d0
        beq.s   L4EE4
        move.w  #$0012,bv_stopn(a6)
        trap    #0
        movea.l bv_ssbas(a6),a5
        adda.l  a6,a5
        suba.l  bv_sssav(a6),a5
        subq.w  #4,a5
        move    a5,usp
        move.w  #$0004,sr                       ;user mode, interrupts on, Z flag set
        sf      bv_cont(a6)
L4EE2   rts

L4EE4   movea.l bv_btp(a6),a0
        movea.l bv_endpt(a6),a1
        lea     0(a1,d1.l),a2
L4EF0   subq.w  #4,a2
        subq.w  #4,a1
        move.l  0(a6,a1.l),0(a6,a2.l)
        cmpa.l  a0,a1
        bgt.s   L4EF0
        moveq   #72,d0
        moveq   #100,d2
L4F02   add.l   d1,0(a6,d0.l)
        addq.w  #4,d0
        cmp.l   d2,d0
        ble.s   L4F02
        adda.l  d1,a7
        movem.l (a7)+,d0-d2
        bra.s   L4EAC

L4F14   tst.b   d0
        bmi.s   L4F54
        cmpi.l  #68,d2
        beq.s   L4F84
        movea.l bv_lnp(a6),a1
        movea.l 4(a6,d2.l),a0
        lea     0(a1,d1.l),a2
L4F2C   subq.w  #4,a1
        subq.w  #4,a2
        move.l  0(a6,a1.l),0(a6,a2.l)
        cmpa.l  a0,a1
        bgt.s   L4F2C
        moveq   #4,d0
        add.w   d2,d0
        moveq   #72,d2
        tst.l   bv_vvfree(a6)
        beq.s   L4F7A
        cmpi.l  #40,d0
        bgt.s   L4F7A
        add.l   d1,bv_vvfree(a6)
        bra.s   L4F7A

L4F54   cmpi.l  #72,d2
        beq.s   L4F84
        movea.l -4(a6,d2.l),a1
        movea.l bv_btp(a6),a0
        neg.l   d1
        lea     0(a0,d1.l),a2
L4F6A   move.l  0(a6,a0.l),0(a6,a2.l)
        addq.w  #4,a0
        addq.w  #4,a2
        cmpa.l  a1,a0
        ble.s   L4F6A
        moveq   #72,d0
L4F7A   add.l   d1,0(a6,d0.l)
        addq.w  #4,d0
        cmp.l   d2,d0
        blt.s   L4F7A
L4F84   movem.l (a7)+,a0-a3
        rts

L4F8A   moveq   #20,d2
        bra.s   L4FA0
L4F8E   moveq   #36,d2
        bra.s   L4FA0
L4F92   moveq   #28,d2
        bra.s   L4FA0
L4F96   moveq   #44,d2
        bra.s   L4FA0
L4F9A   moveq   #52,d2
        bra.s   L4FA0
L4F9E   moveq   #60,d2
L4FA0   movea.l 4(a6,d2.l),a1
        move.l  a1,d1
        move.l  0(a6,d2.l),d0
        addq.w  #1,d0
        bclr    #0,d0
        movea.l d0,a0
        sub.l   a0,d1
        beq.s   L4FE6
L4FB6   move.l  0(a6,a1.l),0(a6,a0.l)
        addq.w  #4,a1
        addq.w  #4,a0
        cmpa.l  bv_lnp(a6),a1
        blt.s   L4FB6
        moveq   #4,d0
        add.b   d2,d0
        tst.l   bv_vvfree(a6)
        beq.s   L4FDA
        cmpi.b  #40,d2
        bge.s   L4FDA
        sub.l   d1,bv_vvfree(a6)
L4FDA   moveq   #72,d2
L4FDC   sub.l   d1,0(a6,d0.l)
        addq.b  #4,d0
        cmp.b   d2,d0
        blt.s   L4FDC
L4FE6   rts

L4FE8   addq.l  #7,d1
        andi.l  #$FFFFFFF8,d1
        beq.s   L4FFA
        movea.w #bv_vvfree,a1
        moveq   #mt.lnkfr,d0
        trap    #1
L4FFA   rts

L4FFC   rts

        dc.w    0

;MDV driver FORMAT

L5000   tst.b   sv_mdrun(a6)                    ;currently turning microdrive ?
        beq.s   L500A                           ;none
        moveq   #err.iu,d0                      ;else return in use
        rts

L500A   move.l  d1,d7                           ;drive number
        movea.l a1,a4                           ;pointer to medium name string
        move.l  #16+14+610+512,d1               ;(hp_end + header + format block + map)
        dc.w    $4EB9,0,L2FAE    *** jsr L2FAE  ;mm_alchp, get working memory
        beq.s   L501E                           ;memory allocation successful
        rts                                     ;else return error

L501E   lea     hp_end(a0),a0                   ;skip heap header
        movea.l a0,a5                           ;preserve pointer to format buffer
        moveq   #$FF,d0                         ;set sector header flag ($FF)
        move.w  d0,(a0)+                        ;and initial sector number (255)
        moveq   #9,d1                           ;medium name
L502A   move.b  #' ',(a0)+                      ;pad out blank
        dbf     d1,L502A
        move.w  sv_rand(a6),(a0)                ;random format number
        suba.w  #10,a0                          ;back to start of medium name
        move.w  (a4)+,d1                        ;length of format name string
        addq.w  #5,a4                           ;skip device name
        subq.w  #5,d1                           ;less device name length
        cmpi.w  #10,d1                          ;maximum length of medium name
        bls.s   L5048                           ;ok if same or less
        moveq   #10,d1                          ;else ignore anything longer
L5048   move.b  (a4)+,(a0)+                     ;copy medium name into header
        subq.w  #1,d1
        bgt.s   L5048
        lea     14(a5),a0                       ;start of formatting block
        move.l  #$FD000C10,(a0)+                ;block header and checksum
        addq.w  #6,a0                           ;short preamble is 6 zeros
        move.w  d0,(a0)+                        ;followed by two $FF's
        move.w  #298,d1                         ;extended data block for formatting
L5060   move.w  #$AA55,(a0)+                    ;contains unambiguous data words
        dbf     d1,L5060                        ;but delimit normal data block within it
        move.w  #$0F0E,538(a5)                  ;with checksum at 512 data block boundary
        move.w  d7,d1                           ;drive number
        lea     pc_mctrl,a3                     ;microdrive control port
        moveq   #pc.mdvmd,d0                    ;microdrive mode
        dc.w    $4EB9,0,L0420    *** jsr L0420  ;set hardware output mode
        ori.w   #$0700,sr                       ;interrupts off
        dc.w    $4EB9,0,L2C56    *** jsr L2C56  ;turn on MDV motor d1.b
        move.l  #125000,d0                      ;wait 0.5s for spin up
L508E   subq.l  #1,d0
        bne.s   L508E
        move.b  #pc.erase,(a3)                  ;erase on, write off
L5096   movea.l a5,a1                           ;point to sector header in buffer
        moveq   #13,d1                          ;bytes-1 to write
        move.w  #1145,d0                        ;wait 2.750mS
L509E   dbf     d0,L509E
        jsr     L51A6(pc)                       ;write sector header to microdrive
        move.w  #609,d1                         ;next in buffer is format data block
        move.w  #1147,d0                        ;wait 2.755mS
L50AE   dbf     d0,L50AE
        jsr     L51A6(pc)                       ;write format block to microdrive
        subq.b  #1,-623(a1)                     ;update sector number in header
        bcc.s   L5096                           ;repeat until sectors 255-0 written
        move.b  #pc.read,(a3)                   ;idle microdrive
        clr.l   -(a7)                           ;good/total sectors
        moveq   #0,d5                           ;flag first verfication pass
L50C4   move.w  #255,d5                         ;maximum number of sectors to check
L50C8   movea.l a5,a1                           ;point to start of buffer
        jsr     L523A(pc)                       ;md_sectr, read header into buffer 0-13
        bra.s   L50E6                           ;bad medium, abort format failed
        bra.s   L50E2                           ;bad header, skip sector and try next
        jsr     L52A4(pc)                       ;verify data with buffer 14-623
        bra.s   L50E2                           ;failed, skip sector and try next
        add.w   d7,d7                           ;sector number read is offset to
        subq.b  #1,0(a1,d7.w)                   ;update microdrive map (buffer 624-1135)
        tst.w   d7                              ;reached sector 0 ?
        beq.s   L50EA                           ;yes, verification pass complete
L50E2   dbf     d5,L50C8                        ;get next sector until count exhausted
L50E6   bra     L5180                           ;abort format failed if sector 0 fails !

L50EA   tst.l   d5                              ;second verification pass ?
        blt.s   L50F2                           ;yes, continue
        moveq   #-1,d5                          ;else do a second pass and
        bra.s   L50C4                           ;verify all sectors again

;Formatting phase complete, now check for useable sectors and write map and directory.

L50F2   moveq   #0,d5                           ;total sector counter
L50F4   subq.b  #1,(a1)                         ;map entry for sector
        cmpi.b  #md.sbad,(a1)                   ;sector fail verification ?
        bgt.s   L5108                           ;yes, both passes
        beq.s   L5100                           ;yes, one pass only
        addq.w  #1,(a7)                         ;no, so increment good sectors
L5100   move.b  d5,3(a7)                        ;store total sectors
        move.b  (a1),d4                         ;highest sector map entry
        movea.l a1,a4                           ;and pointer to it
L5108   addq.w  #2,a1                           ;next sector map entry
        addq.b  #1,d5                           ;increment sector counter
        bcc.s   L50F4                           ;until all possible sectors done
        st      (a4)                            ;last is useable (too near sector 0)
        addq.b  #2,d4                           ;but did last sector fail ?
        beq.s   L5116                           ;yes
        subq.w  #1,(a7)                         ;else decrement good sectors
L5116   cmpi.w  #200,(a7)                       ;unacceptable number of good sectors ?
        blt.s   L5180                           ;yes, abort format failed
        lea     14(a5),a1                       ;point to data block in header
        moveq   #0,d0
L5122   clr.l   (a1)+                           ;clear next 512 bytes (buffer 14-525)
        addq.b  #1,d0                           ;(ie. all file/block map entries)
        bpl.s   L5122
        lea     624(a5),a1                      ;start of map in buffer
        move.b  #md.smap,(a1)                   ;sector 0 is always map
        move.l  (a7),d1                         ;good/total sectors
        add.w   d1,d1                           ;create offset to last sector in map
        subi.w  #16,d1                          ;require good sector for directory
L5138   subq.w  #2,d1                           ;at least nine away from map sector
        cmpi.b  #md.svac,0(a1,d1.w)             ;sector vacant ?
        bne.s   L5138                           ;no, try next
        clr.b   0(a1,d1.w)                      ;use for file number 0, the directory
        move.w  d1,510(a1)                      ;last sector allocated (md_lsect)
        lsr.w   #1,d1                           ;redress to real sector number
        move.b  d1,(a7)                         ;which is used by directory
        moveq   #0,d2                           ;sector number 0 (map)
        jsr     L5222(pc)                       ;search for sector header
        bra.s   L5180                           ;abort format failed
        lea     624(a5),a1                      ;point to map in buffer
        move.w  (a1),-(a7)                      ;file/block number ($F8/$00)
        jsr     L51B0(pc)                       ;md_write, write map to microdrive
        addq.w  #2,a7                           ;discard file/block number
        move.b  (a7),d2                         ;sector number of directory
        jsr     L5222(pc)                       ;search for sector header
        bra.s   L5180                           ;abort format failed
        lea     14(a5),a1                       ;point to (null) data block in header
        move.l  #md_deend,(a1)                  ;set md_delen, current file length
        clr.w   -(a7)                           ;file 0/block 0 is new directory
        jsr     L51B0(pc)                       ;md_write, write directory to microdrive
        addq.w  #2,a7                           ;discard file/block number
        moveq   #err.ok,d7                      ;format succesful
        bra.s   L5182                           ;return operation complete

L5180   moveq   #err.ff,d7                      ;format failed
L5182   dc.w    $4EB9,0,L2C50    *** jsr L2C50  ;turn off all MDV motors
        lea     -hp_end(a5),a0                  ;start of heap used
        dc.w    $4EB9,0,L305E    *** jsr L305E  ;mm_rechp, reclaim working memory
        dc.w    $4EB9,0,L0452    *** jsr L0452  ;prepare for serial transmit mode
        andi.w  #$F0FF,sr                       ;interrupts on
        clr.b   (a7)                            ;clear directory sector number
        move.w  (a7)+,d1                        ;good sectors
        move.w  (a7)+,d2                        ;total sectors
        move.l  d7,d0                           ;restore error
        rts

L51A6   lea     L51AC(pc),a4                    ;return via this routine
        bra.s   L51E4                           ;write data to microdrive

L51AC   moveq   #pc.erase,d4                    ;erase on, write off
        bra.s   L51DA                           ;set hardware mode

;Vector $126 MD_WRITE - Write a microdrive sector

L51B0   move.b  #pc.erase,(a3)                  ;erase on, write off
        move.w  #1481,d0                        ;wait 3.557mS
L51B8   dbf     d0,L51B8
        movea.l a1,a0                           ;preserve start of buffer
        lea     4(a7),a1                        ;pointer to file/block number
        moveq   #1,d1                           ;two bytes to write
        lea     L51CA(pc),a4                    ;return via this routine
        bra.s   L51E4                           ;write data to microdrive

L51CA   movea.l a0,a1                           ;pointer to data block
        move.w  #511,d1                         ;bytes-1 to write
        moveq   #5,d5                           ;short preamble starts with 6 zeros
        lea     L51D8(pc),a4                    ;return via this routine
        bra.s   L51F2                           ;write data to microdrive

L51D8   moveq   #pc.read,d4                     ;return hardware to idling
L51DA   moveq   #48,d0                          ;after waiting 0.1176mS
L51DC   dbf     d0,L51DC
        move.b  d4,(a3)                         ;set control mode
        rts                                     ;operation complete (never in error)

L51E4   moveq   #pc.write,d0                    ;erase on, write on
        move.b  d0,(a3)                         ;set control mode
        move.b  d0,(a3)                         ;(to be sure)
        moveq   #pc..txfl,d6                    ;port bit for transmit buffer full
        lea     pc_tdata-pc_mctrl(a3),a2        ;transmit data port
        moveq   #9,d5                           ;long preamble starts with 10 zeros
L51F2   moveq   #0,d4                           ;zero byte
L51F4   bsr.s   L521A                           ;send byte
        subq.b  #1,d5
        bge.s   L51F4
        moveq   #$FF,d4                         ;preamble ends with two $FF's
        bsr.s   L521A                           ;send byte
        bsr.s   L521A                           ;send byte
        move.w  #$0F0F,d3                       ;initialise checksum
        moveq   #0,d4
L5206   move.b  (a1)+,d4                        ;get byte from buffer
        add.w   d4,d3                           ;accumulate checksum
        bsr.s   L521A                           ;send byte
        dbf     d1,L5206                        ;until all bytes in buffer sent
        move.w  d3,d4                           ;checksum LSB
        bsr.s   L521A                           ;send byte
        lsr.w   #8,d4                           ;checksum MSB
        bsr.s   L521A                           ;send byte
        jmp     (a4)                            ;return via routine supplied

L521A   btst    d6,(a3)                         ;transmit buffer empty yet ?
        bne.s   L521A                           ;no, keep checking
        move.b  d4,(a2)                         ;write data to transmit port
        rts

;Search for header of sector d2.b

L5222   moveq   #0,d5                           ;initialise attempts counter
L5224   movea.l a5,a1                           ;buffer for header read
        bsr.s   L523A                           ;md_sectr (read header)
        rts                                     ;bad medium
        bra.s   L5224                           ;bad header, try again (ad infinitum !)
        cmp.b   d7,d2                           ;is this the sector number required ?
        beq.s   L5236                           ;yes, return
        addq.b  #1,d5                           ;increment attempts counter
        bcc.s   L5224                           ;try upto 255 times
        rts                                     ;else failed

L5236   addq.l  #2,(a7)                         ;outcome OK
        rts

;Vector $12A MD_SECTR - Read a microdrive sector header

L523A   jsr     L5448(pc)                       ;find gap
        rts                                     ;not found, outcome is bad medium
        addq.l  #2,(a7)                         ;else prime for bad header
        moveq   #13,d1                          ;bytes-1 to read
        bsr     L52CE                           ;read data
        bra.s   L525A                           ;bad, outcome is bad header
        cmpi.b  #$FF,-14(a1)                    ;header starts $FF ?
        bne.s   L525A                           ;bad header
        moveq   #0,d7
        move.b  -13(a1),d7                      ;sector number of header read
        addq.l  #2,(a7)                         ;outcome OK
L525A   rts

;Vector $124 MD_READ - Read a microdrive sector

L525C   lea     L52CE(pc),a0                    ;read data block routine
        bra.s   L5266

;Vector $128 MD_VERIN - Verify a microdrive sector

L5262   lea     L5354(pc),a0                    ;verify data block routine
L5266   jsr     L5448(pc)                       ;find gap
        rts                                     ;not found
        move.l  a1,-(a7)                        ;found, preserve start of data buffer
        clr.w   -(a7)                           ;make space for temporary
        movea.l a7,a1                           ;buffer for file/block number read
        moveq   #1,d1                           ;2 bytes to fetch
        bsr.s   L52CE                           ;read data
        bra.s   L5296                           ;failed read or checksum check
        move.b  #pc.read,d1                     ;microdrive read mode
        move.b  d1,(a3)                         ;set control control
        moveq   #8,d0                           ;wait 21.6uS
L5280   dbf     d0,L5280
        move.b  d1,(a3)                         ;and set control mode again
        move.w  #511,d1                         ;512 byte sector to read or verify
        movea.l 2(a7),a1                        ;buffer pointer
        jsr     (a0)                            ;read or verify buffer data
        bra.s   L5296                           ;fail read/verify or checksum check
        addq.l  #2,6(a7)                        ;return outcome will be successful
L5296   moveq   #0,d1
        moveq   #0,d2
        move.b  1(a7),d2                        ;block number
        move.b  (a7)+,d1                        ;file number
        addq.w  #4,a7                           ;discard buffer pointer
        rts

;Verify format data block.

L52A4   jsr     L5448(pc)                       ;find gap
        rts                                     ;not found
        move.w  #609,d1                         ;610 byte format block to
        bsr     L5354                           ;verify buffer data
        rts                                     ;failed verify or checksum check
        addq.l  #2,(a7)                         ;successful outcome
        rts

;Initialise microdrive registers

L52B8   move.w  #256,d0                         ;initial attempts
        move.w  #$0F0F,d3                       ;initialise checksum
        moveq   #0,d4
        moveq   #pc..rxrd,d6                    ;port bit for read buffer ready
        lea     pc_trak1-pc_mctrl(a3),a2        ;microdrive read track1 port
        lea     pc_trak2-pc_mctrl(a3),a4        ;    "        "    "  2  "
        rts

;Read microdrive data to buffer

L52CE   bsr.s   L52B8                           ;initialise registers
L52D0   btst    d6,(a3)                         ;read buffer ready ?
        dbne    d0,L52D0                        ;exit when ready or if attempts exhausted
        exg     a4,a4                           ;(slight wait)
        move.b  (a2),d4                         ;read first data byte from track port 1
        exg     a2,a4                           ;alternate track pointers
        move.b  d4,(a1)+                        ;data to buffer
        add.w   d4,d3                           ;accumulate checksum
        tst.w   d0                              ;initial attempts counter exhausted ?
        blt     L5446                           ;yes, return outcome failed
        moveq   #20,d0                          ;(redundant loop counter)
        subq.w  #1,d1                           ;decrement byte counter

;Attempt 19 times to read each subsequent byte.

L52EA   btst    d6,(a3)                         ;read buffer ready ?
        bne.s   L5338                           ;yes
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        btst    d6,(a3)
        bne.s   L5338
        moveq   #err.nc,d0                      ;failed to read within 19 attempts
L5338   nop                                     ;(slight wait)
        nop
        move.b  (a2),d4                         ;read data from track port 1 or 2
        exg     a2,a4                           ;alternate track pointers
        move.b  d4,(a1)+                        ;data to buffer
        add.w   d4,d3                           ;accumulate checksum
        tst.w   d0                              ;failed subsequent read ?
        blt     L5446                           ;yes, return outcome failed
        moveq   #20,d0                          ;(redundant loop counter)
        subq.w  #1,d1                           ;decrement byte counter
        bge.s   L52EA                           ;read next if not expired
        bra     L53DE                           ;else read and verify checksum

;Verify microdrive data with buffer

L5354   bsr     L52B8                           ;initialise registers
L5358   btst    d6,(a3)                         ;read buffer ready ?
        dbne    d0,L5358                        ;exit when ready or if attempts exhausted
        exg     a4,a4                           ;(slight wait)
        move.b  (a2),d4                         ;read first data byte from track port 1
        exg     a2,a4                           ;alternate track pointers
        cmp.b   (a1)+,d4                        ;match buffer data ?
        bne     L5446                           ;no, return outcome failed
        add.w   d4,d3                           ;accumulate checksum
        tst.w   d0                              ;initial attempts counter exhausted ?
        blt     L5446                           ;yes, return outcome failed
        moveq   #20,d0                          ;(redundant loop counter)
        subq.w  #1,d1                           ;decrement byte counter

;Attempt 19 times to read each subsequent byte for verification.

L5376   btst    d6,(a3)                         ;read buffer ready ?
        bne.s   L53C4                           ;yes
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        btst    d6,(a3)
        bne.s   L53C4
        moveq   #err.nc,d0                      ;failed to read within 19 attempts
L53C4   nop                                     ;(slight wait)
        nop
        move.b  (a2),d4                         ;read data from track port 1 or 2
        exg     a2,a4                           ;alternate track pointers
        cmp.b   (a1)+,d4                        ;match buffer data ?
        bne.s   L5446                           ;no, return outcome failed
        add.w   d4,d3                           ;accumulate checksum
        tst.w   d0                              ;failed subsequent read ?
        blt     L5446                           ;yes, return outcome failed
        moveq   #20,d0                          ;(redundant loop counter)
        subq.w  #1,d1                           ;decrement byte counter
        bge.s   L5376                           ;read next if not expired

;Read and validate checksum immediately following data read/verified.

L53DE   btst    d6,(a3)                         ;read buffer ready ?
        bne.s   L542C                           ;yes
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        btst    d6,(a3)
        bne.s   L542C
        moveq   #err.nc,d0                      ;failed to read within 19 attempts
L542C   nop                                     ;(slight wait)
        nop
        move.b  (a2),d4                         ;read data from track port 1 or 2
        exg     a2,a4                           ;alternate track pointers
        ror.w   #8,d4                           ;align checksum LSB or MSB
        tst.w   d0                              ;failed to read checksum byte ?
        blt.s   L5446                           ;yes, return outcome failed
        moveq   #20,d0                          ;(redundant loop counter)
        addq.w  #1,d1                           ;read both checksum bytes ?
        beq.s   L53DE                           ;no, read checksum MSB
        cmp.w   d4,d3                           ;checksum valid ?
        bne.s   L5446                           ;no, return outcome failed
        addq.l  #2,(a7)                         ;else outcome successful
L5446   rts

;Find gap on microdrive and put in read mode.

L5448   moveq   #0,d1                           ;attempts counter (65536)
L544A   subq.w  #1,d1                           ;failed all attempts ?
        beq.s   L5476                           ;yes, return outcome failed
        btst    #pc..gap,(a3)                   ;gap found ?
        beq.s   L544A                           ;no, keep trying (0.5 sec)
        moveq   #0,d1                           ;reset attempts counter
L5456   subq.w  #1,d1                           ;failed all attempts ?
        beq.s   L5476                           ;yes, return outcome failed
        moveq   #23,d0                          ;gap width
L545C   btst    #pc..gap,(a3)                   ;gap closed yet ?
        bne.s   L5456                           ;no, keep trying (0.5 sec)
        dbf     d0,L545C                        ;else wait for 160uS
        move.b  #pc.read,d1                     ;microdrive read mode
        move.b  d1,(a3)                         ;set control mode
        moveq   #8,d0                           ;wait 21.6uS
L546E   dbf     d0,L546E
        move.b  d1,(a3)                         ;and set control mode again
        addq.l  #2,(a7)                         ;return outcome successful
L5476   rts

;Set up registers/hardware for network use.

L5478   btst    #pc..serb,sv_tmode(a6)          ;current output mode ?
        bne.s   L549A                           ;is microdrive
        lea     pc_mctrl,a3                     ;link control port
        lea     pc_tctrl-pc_mctrl(a3),a2        ;transmit control port
        moveq   #pc.netmd,d0                    ;network mode
        dc.w    $4EB9,0,L0420    *** jsr L0420  ;set hardware output mode
        ori.w   #$0700,sr                       ;interrupts off
        moveq   #pc..neti,d7                    ;network data in bit of link port
        rts

L549A   addq.w  #4,a7                           ;discard return address
        moveq   #err.nc,d0                      ;cannot use network
        bra     L55A8                           ;return

;Get a data block from network.

L54A2   movem.l d4-d7,-(a7)
        clr.w   net_type(a0)
        bsr.s   L5478                           ;set up registers/hardware
L54AC   move.w  #5858,d4
L54B0   move.w  #437,d0
L54B4   btst    d7,(a3)
        beq.s   L54BE
        dbf     d4,L54B0
        bra.s   L5532
L54BE   dbf     d0,L54B4
        move.w  #3124,d0
L54C6   btst    d7,(a3)
        bne.s   L54D0
        dbf     d0,L54C6
        bra.s   L5532

L54D0   move.w  #218,d0
L54D4   dbf     d0,L54D4
        lea     net_data(a0),a1
        moveq   #8,d1
        bsr     L55B4
        blt.s   L54AC
        sub.b   -(a1),d3
        cmp.b   (a1),d3
        bne.s   L54AC
        subq.w  #7,a1
        move.w  net_dest(a0),d3
        ror.w   #8,d3
        tst.b   d3
        beq.s   L5500
        cmp.b   net_self(a0),d3
        beq.s   L5500
        cmp.w   (a1),d3
        bra.s   L5504

L5500   cmp.b   net_dest-net_hedr(a1),d3
L5504   bne.s   L5532
        move.w  net_blkl-net_hedr(a1),d6
        sub.w   net_blkl(a0),d6
        bne.s   L5516
        move.l  net_type-net_hedr(a1),net_type(a0)
L5516   bsr     L563A
        move.b  net_nbyt(a0),d1
        bsr     L55B4
        bne.s   L5532
        cmp.b   net_dchk(a0),d3
        bne.s   L5532
        bsr     L563A
        tst.w   d6
        beq.s   L5592
L5532   clr.w   net_type(a0)
        bra.s   L55A0

;Send a data block to network.

L5538   movem.l d4-d7,-(a7)
        bsr     L5478                           ;set up registers/hardware
        move.b  net_nbyt(a0),d1                 ;bytes in data block
        moveq   #0,d3                           ;for data checksum
        lea     net_data(a0),a1                 ;pointer to data block
L554A   add.b   (a1)+,d3                        ;accumulate checksum (modulo 256)
        subq.b  #1,d1                           ;decrement byte counter
        bne.s   L554A                           ;until done
        move.b  d3,net_dchk(a0)                 ;store data checksum
        moveq   #7,d1                           ;length of header-1
        moveq   #0,d3                           ;for header checksum
        lea     net_hedr(a0),a1                 ;starting here
L555C   add.b   (a1)+,d3                        ;accumulate checksum (modulo 256)
        subq.b  #1,d1                           ;decrement byte counter
        bne.s   L555C                           ;until done
        move.b  d3,(a1)                         ;store header checksum (net_hchk)
        subq.w  #7,a1                           ;point to start of header
        bsr     L55F2
        bne.s   L55A2                           ;return if ???
        moveq   #8,d1
        moveq   #1,d5
L5570   bsr     L5648
        moveq   #1,d1
        lea     net_hchk(a0),a1
        bsr.s   L55B4
        beq.s   L5586
        tst.b   net_dest(a0)
        bne.s   L55A0
        moveq   #1,d3
L5586   subq.b  #1,d3
        bne.s   L55A0
        move.b  net_nbyt(a0),d1
        dbf     d5,L5570
L5592   addq.b  #1,net_blkl(a0)
        bcc.s   L559C
        addq.b  #1,net_blkh(a0)
L559C   moveq   #0,d0
        bra.s   L55A2

L55A0   moveq   #err.nc,d0
L55A2   dc.w    $4EB9,0,L0452    *** jsr L0452  ;prepare for serial transmit mode
L55A8   andi.w  #$F8FF,sr                       ;interrupts on
        movem.l (a7)+,d4-d7
        tst.l   d0
        rts

L55B4   move.w  #399,d0
        moveq   #0,d3
L55BA   btst    d7,(a3)
        beq.s   L55C0
        bra.s   L55C6
L55C0   dbf     d0,L55BA
        bra.s   L55EE
L55C6   moveq   #70,d0
L55C8   btst    d7,(a3)
        dbeq    d4,L55C8
        bne.s   L55EE
        moveq   #7,d4
        moveq   #$13,d0
L55D4   ror.b   d0,d7
        move.b  (a3),d0
        roxr.b  #1,d0
        roxr.b  #1,d2
        moveq   #6,d0
        dbf     d4,L55D4
        move.b  d2,(a1)+
        add.b   d2,d3
        subq.b  #1,d1
        bne.s   L55C6
L55EA   moveq   #err.ok,d0
        rts

L55EE   moveq   #err.nc,d0
        rts

L55F2   moveq   #115,d0
        mulu.w  sv_rand(a6),d0
        move.w  d0,sv_rand(a6)
        ext.w   d0
        addi.w  #663,d0
L5602   btst    d7,(a3)
        bne.s   L55EE
        dbf     d0,L5602
        move.b  net_self(a0),d4
        not.b   d4
        move.b  sv_tmode(a6),d2
        moveq   #9,d1
L5616   lsl.b   #1,d4
        rol.b   #1,d2
        roxr.b  #1,d2
        move.b  d2,(a2)
        bmi.s   L562E
        moveq   #5,d0
L5622   btst    d7,(a3)
        beq.s   L5628
        bra.s   L55EE
L5628   dbf     d0,L5622
        bra.s   L5634
L562E   moveq   #16,d0
L5630   dbf     d0,L5630
L5634   dbf     d1,L5616
L5638   bra.s   L55EA

L563A   tst.b   net_dest(a0)
        beq.s   L55EA
        moveq   #1,d1
        move.w  #$FF01,d4
        bra.s   L564C

L5648   moveq   #$FF,d4
L564A   move.b  (a1)+,d4
L564C   lsl.w   #1,d4
        rol.w   #2,d4
        moveq   #12,d3
        move.b  sv_tmode(a6),d0
L5656   asr.w   #1,d4
        rol.b   #1,d0
        roxr.b  #1,d0
        moveq   #0,d7
        move.b  d0,(a2)
        subq.w  #1,d3
        bge.s   L5656
        subq.b  #1,d1
        bne.s   L564A
        move.b  sv_tmode(a6),(a2)
        bra.s   L5638

;Initialise BASIC pointers/data

;Set pointers from bv.bfbas ($00) to bv.lnp ($44) to $100

L566E   suba.l  a3,a3
        moveq   #bv_change,d0
L5672   move.l  #$100,(a6,a3.l)
        addq.w  #4,a3
        cmpa.w  d0,a3
        blt.s   L5672

;Set pointers from bv.btp ($48) to bv.ssbas ($64) to top BASIC area (-4)

        moveq   #bv_endpt,d0
L5682   move.l  a5,0(a6,a3.l)
        addq.w  #4,a3
        cmpa.w  d0,a3
        ble.s   L5682

;Set up further data

        sf      bv_inlin(a6)
        st      bv_cont(a6)
        st      bv_rand+2(a6)
        sf      bv_auto(a6)
        st      bv_print(a6)
        move.w  #-1,bv_nxlin(a6)
        move.l  #-1,bv__CA(a6)
        moveq   #0,d0
        move.w  d0,bv__C8(a6)
        move.l  d0,bv_comch(a6)
        move.l  d0,bv_vvfree(a6)
        jsr     L4E32(pc)
        moveq   #126,d1
        jmp     L4E6A(pc)               ;reserve space in buffer

L56C6   movea.l bv_btp(a6),a1
        move.l  a1,d1
        sub.l   bv_lnp(a6),d1
        andi.l  #$FFFFFE00,d1
        beq.s   L5700
        movea.l a1,a0
        suba.l  d1,a0
L56DC   move.l  0(a6,a1.l),0(a6,a0.l)
        addq.w  #4,a1
        addq.w  #4,a0
        cmpa.l  bv_ssbas(a6),a1
        blt.s   L56DC
        moveq   #bv_change,d0
        moveq   #bv_endpt,d2
L56F0   sub.l   d1,0(a6,d0.l)
        addq.b  #4,d0
        cmp.b   d2,d0
        ble.s   L56F0
        suba.l  d1,a7
        moveq   #mt.rebas,d0
        trap    #1
L5700   rts

L5702   cmpa.l  bv_ntp(a6),a5
        bne.s   L570C
        move.l  a3,bv_ntp(a6)
L570C   cmpa.l  a5,a3
        bge     L57EE
        andi.b  #$0F,1(a6,a3.l)
        beq.s   L5778
        cmpi.w  #-1,2(a6,a3.l)
        bne.s   L5728
        movea.l a3,a2
        bsr.s   L5784
        bra.s   L5778

L5728   moveq   #1,d0
        sub.b   0(a6,a3.l),d0
        bge.s   L5778
        move.w  2(a6,a3.l),d0
        lsl.l   #3,d0
        movea.l bv_ntbas(a6),a2
        adda.l  d0,a2
        cmpi.b  #3,0(a6,a2.l)
        bne.s   L576C
        cmpi.b  #2,0(a6,a3.l)
        beq.s   L5778
        movea.l 4(a6,a3.l),a0
        adda.l  bv_vvbas(a6),a0
        moveq   #0,d1
        move.w  4(a6,a0.l),d1
        lsl.w   #2,d1
        addq.w  #6,d1
        movem.l d2/a1-a3,-(a7)
        jsr     L4FE8(pc)
        movem.l (a7)+,d2/a1-a3
        bra.s   L5778

L576C   move.l  4(a6,a3.l),4(a6,a2.l)
        move.b  0(a6,a3.l),0(a6,a2.l)
L5778   move.l  d7,0(a6,a3.l)
        move.l  d7,4(a6,a3.l)
        addq.w  #8,a3
        bra.s   L570C

L5784   movem.l d2/d4/d6/a1-a3,-(a7)
        movea.l bv_vvbas(a6),a0
        move.l  4(a6,a2.l),d1
        blt.s   L57E8
        adda.l  d1,a0
        move.b  0(a6,a2.l),d0
        subq.b  #2,d0
        ble.s   L57C0
        subq.b  #1,d0
        bne.s   L57AC
        move.l  d1,d4
        move.b  1(a6,a2.l),d6
        jsr     L99FE(pc)
        bra.s   L57E8

L57AC   subq.b  #3,d0
        beq.s   L57BC
        subq.b  #1,d0
        beq.s   L57B8
        moveq   #err.bn,d0
        bra.s   L57EA

L57B8   moveq   #26,d1
        bra.s   L57DC

L57BC   moveq   #12,d1
        bra.s   L57DC

L57C0   move.b  1(a6,a2.l),d0
        subq.b  #2,d0
        blt.s   L57D2
        beq.s   L57CE
        moveq   #2,d1
        bra.s   L57DC

L57CE   moveq   #6,d1
        bra.s   L57DC

L57D2   moveq   #3,d1
        add.w   0(a6,a0.l),d1
        bclr    #0,d1
L57DC   move.l  #-1,4(a6,a2.l)
        jsr     L4FE8(pc)
L57E8   moveq   #err.ok,d0
L57EA   movem.l (a7)+,d2/d4/d6/a1-a3
L57EE   rts

L57F0   move.l  a0,-(a7)
        move.w  0(a6,a1.l),d0
        addq.w  #2,a1
        moveq   #0,d1
        move.w  d0,d1
        beq     L588A
        swap    d0
        addq.w  #1,d1
        bclr    #0,d1
        move.w  d1,d0
        suba.l  bv_ribas(a6),a1
        movem.l d0-d2/a1,-(a7)
        jsr     L4DF6(pc)
        movem.l (a7)+,d0-d2/a1
        adda.l  bv_ribas(a6),a1
L581E   move.w  0(a6,a1.l),0(a6,a0.l)
        addq.w  #2,a1
        addq.w  #2,a0
        subq.w  #2,d1
        bgt.s   L581E
        suba.w  d0,a0
        move.w  0(a6,a1.l),d2
        addq.w  #2,a1
        movea.l a1,a2
        move.w  d2,d1
        addq.w  #1,d1
        bclr    #0,d1
        adda.w  d1,a1
        move.w  d2,d1
        swap    d0
        add.w   d0,d1
        addq.w  #1,d1
        bmi.s   L5890
        bclr    #0,d1
        suba.w  d1,a1
        move.w  d2,d1
        beq.s   L5862
L5854   move.b  0(a6,a2.l),0(a6,a1.l)
        addq.w  #1,a2
        addq.w  #1,a1
        subq.w  #1,d1
        bgt.s   L5854
L5862   add.w   d0,d2
        move.l  d0,d1
L5866   move.b  0(a6,a0.l),0(a6,a1.l)
        addq.w  #1,a0
        addq.w  #1,a1
        subq.w  #1,d0
        bgt.s   L5866
        suba.w  d1,a0
        suba.w  d2,a1
        subq.w  #2,a1
        move.w  d2,0(a6,a1.l)
        clr.w   d1
        swap    d1
        move.l  a1,-(a7)
        jsr     L4FE8(pc)
        movea.l (a7)+,a1
L588A   moveq   #err.ok,d0
L588C   movea.l (a7)+,a0
        rts

L5890   lea     -2(a2),a1
        move.w  d0,d1
        move.l  a1,-(a7)
        jsr     L4FE8(pc)
        movea.l (a7)+,a1
        moveq   #err.ov,d0
        bra.s   L588C

L58A2   dc.b    0,2,2,2,2,5,5,5,5,5,5,5,3,3,3,2
        dc.b    1,2,2,2,3,3,1,2,2,3,2,0

L58BE   andi.b  #$0F,-7(a6,a5.l)
        moveq   #0,d0
        move.b  L58A2(pc,d4.w),d0
        cmpi.b  #22,d4
        bgt     L598C
        andi.b  #$0F,-15(a6,a5.l)
        cmp.b   -7(a6,a5.l),d0
        beq.s   L5904
        cmpi.b  #5,d0
        bne.s   L58FA
        move.b  -7(a6,a5.l),d2
        cmp.b   -15(a6,a5.l),d2
        bne.s   L58F8
        subq.b  #1,d2
        bne.s   L58F8
        subq.w  #8,a5
L58F4   bra     L5992

L58F8   moveq   #2,d0
L58FA   bsr     L5996
        bne     L5994
        move.b  d2,d0
L5904   subq.w  #8,a5
        cmp.b   -7(a6,a5.l),d0
        beq.s   L58F4
        move.b  1(a6,a5.l),d1
        subq.b  #2,d1
        blt.s   L592A
        beq.s   L591E
        move.w  0(a6,a1.l),-(a7)
        addq.w  #2,a1
        bra.s   L5946

L591E   move.l  2(a6,a1.l),-(a7)
        move.w  0(a6,a1.l),-(a7)
        addq.w  #6,a1
        bra.s   L5946

L592A   moveq   #3,d2
        add.w   0(a6,a1.l),d2
        bclr    #0,d2
        suba.l  d2,a7
        move.l  d2,d1
        subq.w  #1,d1
L593A   move.w  0(a6,a1.l),(a7)+
        addq.w  #2,a1
        subq.w  #2,d1
        bge.s   L593A
        suba.l  d2,a7
L5946   move.b  d1,-(a7)
        move.l  a1,bv_rip(a6)
        bsr.s   L5996
        move.b  (a7)+,d2
        blt.s   L5968
        beq.s   L595C
        subq.w  #2,a1
        move.w  (a7)+,0(a6,a1.l)
        bra.s   L5980

L595C   subq.w  #6,a1
        move.w  (a7)+,0(a6,a1.l)
        move.l  (a7)+,2(a6,a1.l)
        bra.s   L5980

L5968   moveq   #3,d2
        add.w   (a7),d2
        bclr    #0,d2
        suba.l  d2,a1
        move.l  d2,d1
L5974   move.w  (a7)+,0(a6,a1.l)
        addq.w  #2,a1
        subq.w  #2,d1
        bgt.s   L5974
        suba.l  d2,a1
L5980   move.l  a1,bv_rip(a6)
        tst.l   d0
        beq.s   L5994
        addq.w  #8,a5
        rts

L598C   cmp.b   -7(a6,a5.l),d0
        bne.s   L5996
L5992   moveq   #err.ok,d0
L5994   rts

L5996   move.l  d7,-(a7)
        move.l  a0,-(a7)
        move.b  d0,-(a7)
        move.b  -7(a6,a5.l),d2
        moveq   #$0F,d1
        and.l   d1,d0
        and.l   d1,d2
        sub.b   d2,d0
        beq.s   L5A0A
        subq.b  #2,d2
        blt.s   L59DE
        beq.s   L59C4
        addq.b  #1,d0
        blt.s   L59BA
        jsr     L47B8(pc)               ;ri.float
        bra.s   L5A0A

L59BA   movea.l (a6),a0                 ;bv_bfbas
        jsr     L3E54(pc)               ;cn_itod
        bsr.s   L5A20
        bra.s   L5A0A

L59C4   tst.b   d0
        blt.s   L59D4
        jsr     L4796(pc)               ;ri.nint
        move.b  #3,-7(a6,a5.l)
        bra.s   L5A0A

L59D4   movea.l (a6),a0                 ;bv_bfbas
        jsr     L3EF6(pc)               ;cn_ftod
        bsr.s   L5A20
        bra.s   L5A0A

L59DE   subq.b  #1,d0
        blt.s   L5A0A
        beq.s   L59F0
        bsr.s   L5A34
        move.l  a0,-(a7)
        addq.w  #2,a0
        jsr     L3DC2(pc)               ;cn_dtoi
        bra.s   L59FA

L59F0   bsr.s   L5A34
        move.l  a0,-(a7)
        addq.w  #2,a0
        jsr     L3D16(pc)               ;cn_dtof
L59FA   movea.l (a7)+,a0
        move.l  d0,-(a7)
        bsr.s   L5A66
        move.l  (a7)+,d0
        beq.s   L5A0A
        subq.w  #2,a1
        clr.w   0(a6,a1.l)
L5A0A   move.b  (a7)+,d2
        movea.l (a7)+,a0
        move.l  (a7)+,d7
        move.l  a1,bv_rip(a6)
        tst.l   d0
        bne.s   L5A1E
        move.b  d2,-7(a6,a5.l)
        moveq   #err.ok,d0
L5A1E   rts

L5A20   move.l  a1,bv_rip(a6)
        move.l  a4,-(a7)
        move.l  a0,d1
        movea.l (a6),a4                 ;bv_bfbas
        sub.l   a4,d1
        jsr     L5F88(pc)
        movea.l (a7)+,a4
        bra.s   L5A62

L5A34   moveq   #3,d1
        add.w   0(a6,a1.l),d1
        bclr    #0,d1
        jsr     L4DF6(pc)
        movea.l bv_rip(a6),a1
        move.l  a0,-(a7)
L5A48   move.w  0(a6,a1.l),0(a6,a0.l)
        addq.w  #2,a0
        addq.w  #2,a1
        subq.l  #2,d1
        bgt.s   L5A48
        movea.l (a7)+,a0
        move.l  a0,d7
        moveq   #2,d1
        add.w   0(a6,a0.l),d1
        add.l   d1,d7
L5A62   moveq   #err.ok,d0
        rts

L5A66   move.l  a1,-(a7)
        moveq   #3,d1
        add.w   0(a6,a0.l),d1
        bclr    #0,d1
        jsr     L4FE8(pc)
        movea.l (a7)+,a1
        rts

        moveq   #1,d0
        bra.s   L5A84

L5A7E   moveq   #2,d0
        bra.s   L5A84

L5A82   moveq   #3,d0
L5A84   move.b  d0,-(a7)
        bsr.s   L5AA0
        bne.s   L5A9A
        move.b  (a7),d0
        bne.s   L5A90
        moveq   #1,d0
L5A90   jsr     L5996(pc)
        subq.l  #8,a5
        move.l  a5,bv_ntp(a6)
L5A9A   addq.w  #2,a7
        tst.l   d0
        rts

L5AA0   bsr.s   L5ABA
        bne.s   L5AA8
        bsr     L5EC8
L5AA8   rts

L5AAA   dc.b    0
        dc.b    L5B08-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    L5AF8-L5AF4
        dc.b    L5B78-L5AF4
        dc.b    L5B8C-L5AF4
        dc.b    L5BAC-L5AF4
        dc.b    L5BB6-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    L5BB2-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    L5B08-L5AF4
        dc.b    0

L5ABA   movea.l bv_ntp(a6),a5
        move.l  a5,-(a7)
        movem.l d4-d6,-(a7)
        st      -(a7)
        moveq   #64,d1
        jsr     L4E60(pc)               ;reserve space on name table stack
        moveq   #2,d0
        jsr     L4E32(pc)
        bra.s   L5ADC

L5AD4   moveq   #1,d6
        bra.s   L5ADE

L5AD8   st      d6
        bra.s   L5ADE

L5ADC   sf      d6
L5ADE   moveq   #$7F,d4
        and.b   0(a6,a0.l),d4
        cmpi.b  #$70,d4
        bge     L5C58
        move.b  L5AAA(pc,d4.w),d4
        jmp     L5AF4(pc,d4.w)

L5AF4   addq.w  #2,a0
        bra.s   L5ADE

L5AF8   move.b  1(a6,a0.l),d4
        cmpi.b  #5,d4
        beq.s   L5B0E
        cmpi.b  #1,d4
        beq.s   L5B72
L5B08   moveq   #0,d5
        bra     L5D16

L5B0E   addq.w  #2,a0
        tst.b   d6
        beq.s   L5B42
        bgt     L5BAC
        cmpi.b  #3,-8(a6,a5.l)
        beq.s   L5B28
        jsr     L63D0(pc)
        bne.s   L5B2C
        bra.s   L5AD8

L5B28   jsr     L6272(pc)
L5B2C   bne     L5CA2
        cmpi.b  #3,-8(a6,a5.l)
        beq.s   L5AD8
L5B38   cmpi.b  #1,-7(a6,a5.l)
        beq.s   L5AD8
        bra.s   L5AD4

L5B42   bsr     L5AA0
        bne     L5CA2
        cmpi.w  #$8406,0(a6,a0.l)       ;token symbol ')' ?
        bne.s   L5BAC
        addq.w  #2,a0
        bra.s   L5B38

L5B56   dc.b    0,5,5,6,6,4,4,4,4,4,4,4,1,2,1,7
        dc.b    9,1,2,1,6,6,8,11,11,3,3,0

L5B72   move.w  #$8508,0(a6,a0.l)       ;token operator '='
L5B78   tst.b   d6
        beq.s   L5BAC
        move.b  1(a6,a0.l),d4
        addq.w  #2,a0
        move.b  L5B56(pc,d4.w),d5
        swap    d4
        bra     L5D1A

L5B8C   moveq   #22,d4
        add.b   1(a6,a0.l),d4
        addq.w  #2,a0
        move.b  L5B56(pc,d4.w),d5
        bra.s   L5BA2

L5B9A   tst.b   d5
        beq     L5D06
        swap    d4
L5BA2   move.b  d5,-(a7)
        move.b  d4,1(a7)
        bra     L5ADC

L5BAC   moveq   #err.xp,d0
        bra     L5CA2

L5BB2   bra     L5C6A

L5BB6   movea.l bv_ntp(a6),a5
        addq.l  #8,bv_ntp(a6)
        moveq   #0,d4
        move.w  2(a6,a0.l),d4
        move.l  d4,d0
        addq.w  #4,a0
        lsl.l   #3,d4
        movea.l bv_ntbas(a6),a3
        adda.l  d4,a3
        move.b  0(a6,a3.l),d4
        cmpi.b  #9,d4
        beq.s   L5C0C
        cmpi.b  #5,d4
        beq.s   L5C12
        cmpi.b  #8,d4
        beq.s   L5BAC
        cmpi.b  #4,d4
        beq.s   L5BAC
        move.w  0(a6,a3.l),0(a6,a5.l)
        andi.b  #$0F,1(a6,a5.l)
        move.w  d0,2(a6,a5.l)
        move.l  4(a6,a3.l),4(a6,a5.l)
        addq.w  #8,a5
        subq.b  #3,d4
        beq.s   L5C1E
        bra     L5C94

L5C0C   jsr     L60B8(pc)
        bra.s   L5C16

L5C12   jsr     L6060(pc)
L5C16   bne     L5CA2
        bra     L5AD4

L5C1E   moveq   #0,d1
        move.l  -4(a6,a5.l),d0
        blt.s   L5BAC
        movea.l bv_vvbas(a6),a3
        adda.l  d0,a3
        move.w  4(a6,a3.l),d1
        lsl.w   #2,d1
        addq.w  #6,d1
        move.l  a3,-(a7)
        bsr     L5FC2
        movea.l (a7)+,a3
        move.l  a2,d2
        sub.l   bv_vvbas(a6),d2
        move.l  d2,-4(a6,a5.l)
L5C46   move.w  0(a6,a3.l),0(a6,a2.l)
        addq.w  #2,a3
        addq.w  #2,a2
        subq.w  #2,d1
        bne.s   L5C46
        bra     L5AD8

L5C58   movea.l a0,a4
        bsr     L5F6C
        addq.w  #6,a0
        andi.b  #$0F,0(a6,a1.l)
        moveq   #2,d4
        bra.s   L5C76

L5C6A   addq.w  #2,a0
        movea.l a0,a4
        bsr     L5F80
        movea.l a4,a0
        moveq   #1,d4
L5C76   movea.l bv_ntp(a6),a5
        addq.l  #8,bv_ntp(a6)
        move.b  #1,0(a6,a5.l)
        move.b  d4,1(a6,a5.l)
        move.w  #-1,2(a6,a5.l)
        clr.l   4(a6,a5.l)
        addq.w  #8,a5
L5C94   cmpi.b  #1,-7(a6,a5.l)
        beq     L5AD8
        bra     L5AD4

L5CA2   tst.w   (a7)+
        bge.s   L5CA2
        movem.l (a7)+,d4-d6
L5CAA   cmpa.l  (a7),a5
        beq.s   L5CB4
        moveq   #1,d2
        bsr.s   L5CBC
        bra.s   L5CAA

L5CB4   move.l  a5,bv_ntp(a6)
        addq.w  #4,a7
        bra.s   L5D12

L5CBC   move.b  -8(a6,a5.l),d1
        subq.b  #1,d1
        bne.s   L5CEC
        subq.b  #2,-7(a6,a5.l)
        blt.s   L5CD6
        bgt.s   L5CD0
        addq.l  #4,bv_rip(a6)
L5CD0   addq.l  #2,bv_rip(a6)
        bra.s   L5CE8

L5CD6   movea.l bv_rip(a6),a1
        moveq   #3,d1
        add.w   0(a6,a1.l),d1
        bclr    #0,d1
        add.l   d1,bv_rip(a6)
L5CE8   subq.w  #8,a5
L5CEA   rts

L5CEC   tst.b   d2
        beq.s   L5CEA
        subq.b  #2,d1
        bne.s   L5CE8
        move.l  -4(a6,a5.l),d1
        blt.s   L5CE8
        movea.l bv_vvbas(a6),a3
        adda.l  d1,a3
        bsr     L5F30
        bra.s   L5CE8

L5D06   moveq   #0,d0
        addq.w  #2,a7
        movem.l (a7)+,d4-d6
        cmpa.l  (a7)+,a5
        seq     d0
L5D12   tst.l   d0
        rts

L5D16   tst.b   (a7)
        ble.s   L5D06
L5D1A   bsr     L5EC8
L5D1E   bne.s   L5CA2
L5D20   cmp.b   (a7),d5
        bgt     L5B9A
        move.w  (a7)+,d4
        andi.w  #$00FF,d4
        jsr     L58BE(pc)
        bne.s   L5D1E
        move.l  a5,bv_ntp(a6)
        add.w   d4,d4
        move.w  L5D4E(pc,d4.w),d4
        beq     L5BAC
        moveq   #2,d0
        jsr     L5D4E(pc,d4.w)
        bne.s   L5D1E
        move.l  a1,bv_rip(a6)
        bra.s   L5D20

L5D4E   dc.w    0
        dc.w    L4838-L5D4E             ;ri.add
        dc.w    L482A-L5D4E             ;ri.sub
        dc.w    L48DE-L5D4E             ;ri.mult
        dc.w    L497E-L5D4E             ;ri.div
        dc.w    L5D98-L5D4E
        dc.w    L5D8A-L5D4E
        dc.w    L5D90-L5D4E
        dc.w    L5D92-L5D4E
        dc.w    L5DA4-L5D4E
        dc.w    L5D9E-L5D4E
        dc.w    L5D84-L5D4E
        dc.w    L5E6C-L5D4E
        dc.w    L5E74-L5D4E
        dc.w    L5E7C-L5D4E
        dc.w    L43C2-L5D4E             ;ri.powfp
        dc.w    L57F0-L5D4E
        dc.w    L5E42-L5D4E
        dc.w    L5E48-L5D4E
        dc.w    L5E4E-L5D4E
        dc.w    L5E92-L5D4E
        dc.w    L5E98-L5D4E
        dc.w    L5E26-L5D4E
        dc.w    L5E10-L5D4E
        dc.w    L5E0C-L5D4E
        dc.w    L5E84-L5D4E
        dc.w    L5E54-L5D4E

L5D84   bsr.s   L5DAA
        blt.s   L5DF4
        bra.s   L5E04

L5D8A   bsr.s   L5DAA
        bgt.s   L5DF4
        bra.s   L5E04

L5D90   moveq   #3,d0
L5D92   bsr.s   L5DAA
        beq.s   L5DF4
        bra.s   L5E04

L5D98   bsr.s   L5DAA
        bge.s   L5DF4
        bra.s   L5E04

L5D9E   bsr.s   L5DAA
L5DA0   ble.s   L5DF4
        bra.s   L5E04

L5DA4   bsr.s   L5DAA
        bne.s   L5DF4
        bra.s   L5E04

L5DAA   cmpi.b  #1,-7(a6,a5.l)
        bne.s   L5DCC
        move.l  a0,-(a7)
        bsr.s   L5E16
        jsr     L3A9C(pc)               ;ut_cstr
        bsr.s   L5E14
        lea     -6(a0),a1
        move.b  #2,-7(a6,a5.l)
L5DC6   movea.l (a7)+,a0
        tst.l   d0
        rts

L5DCC   move.w  d0,d4
        jsr     L482A(pc)               ;ri.sub
        bne.s   L5DF0
        subq.w  #3,d4
        beq.s   L5DDE
        tst.b   2(a6,a1.l)
        rts

L5DDE   addq.w  #4,a7
        move.w  0(a6,a1.l),d0
        beq.s   L5DF4
        addi.w  #24,d0
        sub.w   -6(a6,a1.l),d0
        bra.s   L5DA0

L5DF0   addq.w  #4,a7
        rts

L5DF4   move.w  #$0801,0(a6,a1.l)
        move.l  #$40000000,2(a6,a1.l)   ;fp[1]
        bra.s   L5E0C

L5E04   clr.w   0(a6,a1.l)
        clr.l   2(a6,a1.l)
L5E0C   moveq   #err.ok,d0
        rts

L5E10   jmp     L4A0C(pc)               ;ri.neg

L5E14   movea.l a0,a1
L5E16   move.w  0(a6,a1.l),d2
        addq.w  #3,d2
        bclr    #0,d2
        lea     0(a1,d2.w),a0
        rts

L5E26   moveq   #1,d0
        move.l  a0,-(a7)
        bsr.s   L5E16
        jsr     L3A6E(pc)
        bsr.s   L5E14
        lea     -2(a0),a1
        move.b  #3,-7(a6,a5.l)
        move.w  d1,0(a6,a1.l)
        bra.s   L5DC6

L5E42   bsr.s   L5E5C
        or.b    d1,d0
        bra.s   L5E58

L5E48   bsr.s   L5E5C
        and.b   d1,d0
        bra.s   L5E58

L5E4E   bsr.s   L5E5C
        eor.b   d1,d0
        bra.s   L5E58

L5E54   bsr.s   L5E64
        not.b   d0
L5E58   beq.s   L5E04
        bra.s   L5DF4

L5E5C   addq.w  #6,a1
        tst.l   -4(a6,a1.l)
        sne     d1
L5E64   tst.l   2(a6,a1.l)
        sne     d0
        rts

L5E6C   bsr.s   L5E8A
        or.w    d0,0(a6,a1.l)
L5E72   bra.s   L5E0C

L5E74   bsr.s   L5E8A
        and.w   d0,0(a6,a1.l)
        bra.s   L5E0C

L5E7C   bsr.s   L5E8A
        eor.w   d0,0(a6,a1.l)
        bra.s   L5E0C

L5E84   not.w   0(a6,a1.l)
        bra.s   L5E0C

L5E8A   move.w  0(a6,a1.l),d0
L5E8E   addq.w  #2,a1
        rts

L5E92   bsr.s   L5EA0
        move.w  d0,d3
        bra.s   L5E9A

L5E98   bsr.s   L5EA0
L5E9A   move.w  d3,0(a6,a1.l)
        bra.s   L5E72

L5EA0   move.w  2(a6,a1.l),d0
        ext.l   d0
        move.w  0(a6,a1.l),d1
        beq.s   L5EC2
        move.w  d0,d2
        divs.w  d1,d0
        move.w  d0,d3
        swap    d0
        eor.w   d1,d2
        bpl.s   L5E8E
        tst.w   d0
        beq.s   L5E8E
        add.w   d1,d0
        subq.w  #1,d3
        bra.s   L5E8E

L5EC2   moveq   #err.ov,d0
        addq.w  #4,a7
        bra.s   L5E8E

L5EC8   andi.b  #$0F,-7(a6,a5.l)
        move.b  -8(a6,a5.l),d0
        beq.s   L5EE8
        subq.b  #1,d0
        beq     L5FB4
        subq.b  #2,d0
        blt.s   L5F42
        beq.s   L5EF6
        subq.b  #3,d0
        blt.s   L5EE8
        subq.b  #1,d0
        ble.s   L5F42
L5EE8   moveq   #1,d2
        bsr     L5CBC
        move.l  a5,bv_ntp(a6)
        moveq   #err.xp,d0
        rts

L5EF6   move.b  -7(a6,a5.l),d0
        subq.b  #1,d0
        bgt.s   L5EE8
        movea.l -4(a6,a5.l),a3
        adda.l  bv_vvbas(a6),a3
        movea.l 0(a6,a3.l),a4
        adda.l  bv_vvbas(a6),a4
        cmpi.w  #1,4(a6,a3.l)
        bgt.s   L5EE8
        move.b  #1,-8(a6,a5.l)
        move.b  #1,-7(a6,a5.l)
        moveq   #0,d1
        move.w  6(a6,a3.l),d1
        bsr.s   L5F30
        tst.b   d0
        beq.s   L5F80
        bra.s   L5F88

L5F30   movem.l d0-d1/a0-a1,-(a7)
        movea.l a3,a0
        moveq   #10,d1
        jsr     L4FE8(pc)
        movem.l (a7)+,d0-d1/a0-a1
        rts

L5F42   move.b  -7(a6,a5.l),d0
        move.l  -4(a6,a5.l),d1
        blt.s   L5EE8
        move.b  #1,-8(a6,a5.l)
        movea.l d1,a4
        adda.l  bv_vvbas(a6),a4
        subq.b  #2,d0
        blt.s   L5F80
        beq.s   L5F6C
        moveq   #2,d1
        bsr.s   L5FBC
        subq.w  #2,a1
        move.w  0(a6,a4.l),0(a6,a1.l)
        bra.s   L5FB4

L5F6C   moveq   #6,d1
        bsr.s   L5FBC
        subq.w  #6,a1
        move.l  2(a6,a4.l),2(a6,a1.l)
        move.w  0(a6,a4.l),0(a6,a1.l)
        bra.s   L5FB4

L5F80   moveq   #0,d1
        move.w  0(a6,a4.l),d1
        addq.w  #2,a4
L5F88   move.l  d1,-(a7)
        addq.l  #3,d1
        bsr.s   L5FBC
        move.l  (a7),d1
        beq.s   L5FAC
        addq.l  #1,d1
        bclr    #0,d1
        move.l  d1,d0
        suba.l  d0,a1
L5F9C   move.b  0(a6,a4.l),0(a6,a1.l)
        addq.w  #1,a1
        addq.w  #1,a4
        subq.l  #1,d0
        bgt.s   L5F9C
        suba.l  d1,a1
L5FAC   subq.w  #2,a1
        addq.w  #2,a7
        move.w  (a7)+,0(a6,a1.l)
L5FB4   move.l  a1,bv_rip(a6)
        moveq   #err.ok,d0
        rts

L5FBC   jsr     L4E4E(pc)               ;bv_chrix
        bra.s   L5FCC

L5FC2   move.l  a0,-(a7)
        jsr     L4DF6(pc)
        movea.l a0,a2
        movea.l (a7)+,a0
L5FCC   movea.l bv_rip(a6),a1
        rts

L5FD2   jsr     L5ABA(pc)
        bne.s   L6048
        andi.b  #$0F,-7(a6,a5.l)
        move.b  -8(a6,a5.l),d0
        cmpi.b  #1,d0
        bne.s   L6046
        move.w  #-1,-6(a6,a5.l)
        move.b  #2,-8(a6,a5.l)
        move.b  -7(a6,a5.l),d0
        subq.b  #2,d0
        blt.s   L6018
        beq.s   L6008
        moveq   #2,d1
        bsr.s   L604A
        addq.w  #2,a2
        addq.w  #2,a1
        bra.s   L6042

L6008   moveq   #6,d1
        bsr.s   L604A
        move.l  2(a6,a1.l),2(a6,a2.l)
        addq.w  #6,a2
        addq.w  #6,a1
        bra.s   L6042

L6018   moveq   #3,d1
        add.w   0(a6,a1.l),d1
        bclr    #0,d1
        bsr.s   L604A
        move.w  0(a6,a1.l),d1
        addq.w  #2,a1
        addq.w  #2,a2
        beq.s   L6042
        addq.w  #1,d1
        bclr    #0,d1
L6034   move.w  0(a6,a1.l),0(a6,a2.l)
        addq.w  #2,a1
        addq.w  #2,a2
        subq.w  #2,d1
        bgt.s   L6034
L6042   move.l  a1,bv_rip(a6)
L6046   moveq   #err.ok,d0
L6048   rts

L604A   jsr     L5FC2(pc)
        move.l  a2,d2
        sub.l   bv_vvbas(a6),d2
        move.l  d2,-4(a6,a5.l)
        move.w  0(a6,a1.l),0(a6,a2.l)
        rts

L6060   movea.l a3,a2
        bsr.s   L6094
        move.b  1(a6,a2.l),-7(a6,a5.l)
        bsr.s   L60A6
        bne.s   L6074
        addq.w  #2,a0
        moveq   #2,d5
        bra.s   L6076

L6074   moveq   #3,d5
L6076   movea.l a0,a4
        move.l  d0,d4
        suba.l  bv_tkbas(a6),a0
        move.l  a0,-(a7)
        dc.w    $41F9,0,L4BFA *** lea L4BFA,a0  ;
        move.l  a0,-(a7)
        dc.w    $41F9,0,LA8EA *** lea LA8EA,a0  ;
        move.l  a0,-(a7)
        bra     L94CA

L6094   clr.w   0(a6,a5.l)
        move.w  #-1,2(a6,a5.l)
        clr.l   4(a6,a5.l)
        addq.w  #8,a5
        rts

L60A6   cmpi.b  #$80,0(a6,a0.l)
        bne.s   L60B0
        addq.w  #2,a0
L60B0   cmpi.w  #$8405,0(a6,a0.l)               ;token symbol '(' ?
        rts

L60B8   bsr.s   L6094
        move.l  a5,d0
        sub.l   bv_ntbas(a6),d0
        move.l  d0,-(a7)
        move.l  4(a6,a3.l),-(a7)
        bsr.s   L60A6
        beq.s   L60CC
        bra.s   L60DE

L60CC   addq.w  #2,a0
        jsr     L614A(pc)
        bne.s   L6144
        cmpi.w  #$8406,0(a6,a0.l)               ;token symbol ')' ?
        bne.s   L6142
        addq.w  #2,a0
L60DE   movea.l (a7)+,a2
        movea.l bv_ntbas(a6),a3
        move.l  a5,d0
        sub.l   a3,d0
        adda.l  (a7),a3
        move.l  d0,-(a7)
        suba.l  bv_tkbas(a6),a0
        movea.l bv_rip(a6),a1
        suba.l  bv_ribas(a6),a1
        movem.l d5-d7/a0-a1,-(a7)
        jsr     (a2)
        move.l  d0,d2
        movem.l (a7)+,d5-d7/a0-a1
        adda.l  bv_tkbas(a6),a0
        beq.s   L6112
        adda.l  bv_ribas(a6),a1
        move.l  a1,bv_rip(a6)
L6112   movea.l bv_rip(a6),a1
        movea.l bv_ntbas(a6),a5
        movea.l a5,a3
        adda.l  (a7)+,a5
        adda.l  (a7)+,a3
        move.l  a0,-(a7)
        move.l  a3,-(a7)
        jsr     L5702(pc)
        movea.l (a7)+,a5
        move.l  d2,d0
        bne.s   L6146
        move.b  d4,-7(a6,a5.l)
        move.b  #1,-8(a6,a5.l)
        clr.l   -4(a6,a5.l)
        movea.l (a7)+,a0
        moveq   #err.ok,d0
        rts

L6142   moveq   #err.xp,d0
L6144   addq.w  #4,a7
L6146   addq.w  #4,a7
        rts

L614A   move.l  d3,-(a7)
        move.l  d4,-(a7)
        sf      d4
L6150   jsr     L5FD2(pc)
        blt.s   L61B6
        bgt.s   L6162
        move.b  0(a6,a0.l),d0
        move.w  0(a6,a0.l),d1
        bra.s   L618C

L6162   move.b  0(a6,a0.l),d0
        move.w  0(a6,a0.l),d1
        cmpi.b  #sep.b,d0
        beq.s   L617C
        cmpi.w  #$8403,d1               ;token symbol '#' ?
        bne.s   L61AA
        st      d4
        addq.w  #2,a0
        bra.s   L6150

L617C   clr.w   0(a6,a5.l)
        move.w  #-1,2(a6,a5.l)
        clr.l   4(a6,a5.l)
        addq.w  #8,a5
L618C   move.l  a5,bv_ntp(a6)
        cmpi.b  #sep.b,d0
        bne.s   L61AA
        tst.b   d4
        beq.s   L619E
        bset    #3,d1
L619E   sf      d4
        addq.w  #2,a0
        lsl.b   #4,d1
        or.b    d1,-7(a6,a5.l)
        bra.s   L6150

L61AA   tst.b   d4
        beq.s   L61B4
        bset    #7,-7(a6,a5.l)
L61B4   moveq   #err.ok,d0
L61B6   move.l  (a7)+,d4
        move.l  (a7)+,d3
        tst.l   d0
        rts

L61BE   bsr.s   L61DA                   ;ca_gtint
        bra.s   L61CC

L61C2   bsr.s   L61E2                   ;ca_gtlin
        bra.s   L61CC

L61C6   bsr.s   L61DE                   ;ca_gtfp
        bra.s   L61CC

L61CA   bsr.s   L61D6                   ;ca_gstr
L61CC   bne.s   L61D4
        subq.w  #1,d3
        beq.s   L61D4
        moveq   #err.bp,d0
L61D4   rts

;Vector $116 CA_GTSTR - Get strings

L61D6   moveq   #1,d0
        bra.s   L61E6

;Vector $112 CA_GTINT - Get word integers

L61DA   moveq   #$03,d0
        bra.s   L61E6

;Vector $114 CA_GTFP - Get floating points

L61DE   moveq   #2,d0
        bra.s   L61E6

;Vector $118 CA_GTLIN - Get long integers

L61E2   moveq   #$05,d0
        ror.l   #1,d0
L61E6   movem.l d5/a4-a5,-(a7)
        move.l  a3,-(a7)
        move.l  d0,-(a7)
        moveq   #0,d5
L61F0   cmpa.l  4(a7),a5                ;any parameters to get?
        ble.s   L6228                   ;no, return zero found
        moveq   #$0F,d6
        and.b   -7(a6,a5.l),d6
        move.b  d6,-7(a6,a5.l)
        jsr     L5EC8(pc)
        bne.s   L622A
        move.l  (a7),d0
        jsr     L5996(pc)
        move.b  d6,-7(a6,a5.l)
        tst.l   d0
        bne.s   L622A
        tst.l   (a7)
        bpl.s   L6222
        jsr     L47AA(pc)
        bne.s   L622A
        move.l  a1,bv_rip(a6)
L6222   addq.w  #1,d5
        subq.w  #8,a5
        bra.s   L61F0

L6228   moveq   #err.ok,d0
L622A   addq.w  #4,a7
        move.l  d5,d3
        movea.l (a7)+,a3
        movem.l (a7)+,d5/a4-a5
        tst.l   d0
        rts

L6238   andi.b  #$0F,-7(a6,a5.l)
        movea.l -4(a6,a5.l),a4
        adda.l  bv_vvbas(a6),a4
        movea.l a4,a2
        move.l  0(a6,a4.l),d6
        move.w  4(a6,a4.l),d4
        addq.w  #6,a4
        rts

L6254   suba.l  bv_vvbas(a6),a2
        suba.l  bv_vvbas(a6),a4
        movem.l a2/a4-a5,-(a7)
        jsr     L5A82(pc)
        movem.l (a7)+,a2/a4-a5
        adda.l  bv_vvbas(a6),a4
        adda.l  bv_vvbas(a6),a2
        rts

L6272   bsr.s   L6238
L6274   moveq   #0,d5
        bsr.s   L6254
        blt.s   L62D0
        beq.s   L6288
        cmpi.w  #$8E05,0(a6,a0.l)       ;token separator 'TO' ?
        beq.s   L6292
L6284   moveq   #err.xp,d0
        bra.s   L62D0

L6288   addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),d5
        blt.s   L62CE
L6292   cmp.w   0(a6,a4.l),d5
        bgt.s   L62CE
        move.w  d5,d0
        mulu.w  2(a6,a4.l),d0
        move.b  -7(a6,a5.l),d1
        subq.b  #2,d1
        blt.s   L62B0
        beq.s   L62AC
        add.l   d0,d0
        bra.s   L62B0

L62AC   mulu.w  #6,d0
L62B0   add.l   d0,d6
        moveq   #0,d0
        move.l  d6,0(a6,a2.l)
        cmpi.w  #$8E05,0(a6,a0.l)       ;token separator 'TO' ?
        bne.s   L62E6
        addq.w  #2,a0
        bsr.s   L6254
        blt.s   L62D0
        beq.s   L62D4
        move.w  0(a6,a4.l),d0
        bra.s   L62E2

L62CE   moveq   #err.or,d0
L62D0   bra     L63CE

L62D4   addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),d0
        cmp.w   0(a6,a4.l),d0
        bgt.s   L62CE
L62E2   sub.w   d5,d0
        blt.s   L62CE
L62E6   move.w  d0,0(a6,a4.l)
        addq.w  #4,a4
        subq.w  #1,d4
        bne.s   L632A
        cmpi.b  #1,-7(a6,a5.l)
        bgt.s   L632A
        beq.s   L6308
        subq.l  #1,0(a6,a2.l)
        addq.w  #1,-4(a6,a4.l)
        tst.w   d5
        beq.s   L62CE
        bra.s   L632A

L6308   clr.b   -7(a6,a5.l)
        addq.w  #1,-4(a6,a4.l)
        addq.l  #1,0(a6,a2.l)
        tst.w   d5
        bne.s   L632A
        tst.w   d0
        bne.s   L62CE
        subq.l  #1,0(a6,a2.l)
        subq.w  #1,-4(a6,a4.l)
        move.b  #3,-7(a6,a5.l)
L632A   move.w  0(a6,a0.l),d0
        addq.w  #2,a0
        cmpi.w  #$8406,d0               ;token symbol ')' ?
        beq.s   L6346
        cmpi.w  #$8E01,d0               ;token separator ',' ?
        bne     L6284
        tst.w   d4
        bgt     L6274
        bra.s   L62CE

L6346   bsr     L6238
        movea.l (a6),a3                 ;bv_bfbas
        move.l  0(a6,a2.l),0(a6,a3.l)
        addq.w  #6,a3
        moveq   #0,d0
L6356   tst.w   0(a6,a4.l)
        beq.s   L6366
        move.l  0(a6,a4.l),0(a6,a3.l)
        addq.w  #4,a3
        addq.w  #1,d0
L6366   addq.w  #4,a4
        subq.w  #1,d4
        bgt.s   L6356
        movea.l (a6),a3                 ;bv_bfbas
        move.w  d0,4(a6,a3.l)
        move.w  4(a6,a2.l),d1
        lsl.l   #2,d1
        addq.w  #6,d1
        move.l  a0,-(a7)
        move.l  a3,-(a7)
        movea.l a2,a0
        jsr     L4FE8(pc)
        movea.l (a7)+,a3
        movea.l (a7)+,a0
        move.w  4(a6,a3.l),d1
        bgt.s   L63A2
        tst.b   -7(a6,a5.l)
        beq.s   L63A2
        move.l  0(a6,a3.l),-4(a6,a5.l)
        move.b  #2,-8(a6,a5.l)
        bra.s   L63CC

L63A2   lsl.l   #2,d1
        addq.w  #6,d1
        move.l  a0,-(a7)
        move.l  a3,-(a7)
        jsr     L4DF6(pc)
        movea.l a0,a2
        movea.l (a7)+,a3
        movea.l (a7)+,a0
        move.l  a2,d0
        sub.l   bv_vvbas(a6),d0
        move.l  d0,-4(a6,a5.l)
L63BE   move.w  0(a6,a3.l),0(a6,a2.l)
        addq.w  #2,a3
        addq.w  #2,a2
        subq.l  #2,d1
        bgt.s   L63BE
L63CC   moveq   #err.ok,d0
L63CE   rts

L63D0   movem.l d5-d6/a2/a4,-(a7)
        cmpi.b  #1,-8(a6,a5.l)
        beq.s   L63EE
        move.l  -4(a6,a5.l),d0
        blt.s   L63EA
        movea.l bv_vvbas(a6),a2
        adda.l  d0,a2
        bra.s   L6404

L63EA   moveq   #err.xp,d0
        bra.s   L643E

L63EE   move.l  a0,-(a7)
        jsr     L5A34(pc)
        moveq   #0,d7
        move.l  a1,bv_rip(a6)
        movea.l a0,a2
        movea.l (a7)+,a0
        move.l  a2,d0
        sub.l   bv_vvbas(a6),d0
L6404   move.l  d0,-(a7)
        bsr.s   L6446
        movea.l bv_vvbas(a6),a2
        adda.l  (a7)+,a2
        bne.s   L6420
        movea.l a2,a4
        addq.w  #2,a4
        subq.w  #1,d5
        adda.w  d5,a4
        move.w  d6,d1
        sub.w   d5,d1
        jsr     L5F88(pc)
L6420   move.l  d0,-(a7)
        cmpi.b  #1,-8(a6,a5.l)
        bne.s   L6434
        move.l  a0,-(a7)
        movea.l a2,a0
        jsr     L5A66(pc)
        movea.l (a7)+,a0
L6434   move.l  (a7)+,d0
        bne.s   L643E
        move.b  #1,-8(a6,a5.l)
L643E   movem.l (a7)+,d5-d6/a2/a4
        tst.l   d0
        rts

L6446   move.w  0(a6,a2.l),-(a7)
        jsr     L5A82(pc)
        blt.s   L6496
        bgt.s   L649E
        addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),d5
        ble.s   L649E
        cmp.w   (a7),d5
        bgt.s   L649E
        move.w  d5,d6
        cmpi.w  #$8E05,0(a6,a0.l)       ;token separator 'TO' ?
        bne.s   L6482
        addq.w  #2,a0
        jsr     L5A82(pc)
        blt.s   L6496
        bgt.s   L6480
        addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),d6
        cmp.w   (a7),d6
        ble.s   L6482
L6480   move.w  (a7),d6
L6482   move.w  d6,d0
        addq.w  #1,d0
        sub.w   d5,d0
        blt.s   L649E
        cmpi.w  #$8406,0(a6,a0.l)       ;token symbol ')'
        bne.s   L649A
        addq.w  #2,a0
        moveq   #err.ok,d0
L6496   addq.w  #2,a7
        rts

L649A   moveq   #err.xp,d0
        bra.s   L6496

L649E   moveq   #err.or,d0
        bra.s   L6496

L64A2   move.l  a4,-(a7)
        moveq   #$0F,d0
        and.b   -7(a6,a5.l),d0
        subq.b  #1,d0
        beq.s   L64D6
        moveq   #0,d0
        move.w  -6(a6,a5.l),d0
        blt.s   L64DC
        movea.l bv_ntbas(a6),a1
        lsl.l   #3,d0
        adda.l  d0,a1
        movea.l bv_nlbas(a6),a0
        adda.w  2(a6,a1.l),a0
        moveq   #0,d1
        move.b  0(a6,a0.l),d1
        lea     1(a0),a4
        jsr     L5F88(pc)
        bra.s   L64DE

L64D6   jsr     L5EC8(pc)
        bra.s   L64DE

L64DC   moveq   #err.bn,d0
L64DE   movea.l (a7)+,a4
        rts

;BASIC - Procedure 'BAUD'

L64E2   jsr     L803E(pc)
        bne.s   L64F6
        move.w  0(a6,a1.l),d1
        moveq   #mt.baud,d0
        trap    #1
        tst.l   d0
        bmi.s   L64F8
        moveq   #err.ok,d0
L64F6   rts

L64F8   moveq   #err.bp,d0
        rts

;Displacements for BEEP parameters to IPC command

L64FC   dc.w    6,0,2,4,8,10,14,12

L650C   add.l   d3,d3
        add.l   d3,bv_rip(a6)
L6512   adda.w  #$18,a7
        movem.l (a7)+,d4-d7/a3-a5
        tst.l   d0
        rts

;BASIC - Procedure 'BEEP'

L651E   jsr     L61DA(pc)               ;ca_gtint
        movem.l d4-d7/a3-a5,-(a7)
        adda.w  #-24,a7
        movea.l a7,a3
        bne.s   L650C
        moveq   #$F1,d0
        cmpi.w  #0,d3
        beq.s   L65A8
        cmpi.w  #1,d3
        beq.s   L650C
        cmpi.w  #3,d3
        beq.s   L650C
        cmpi.w  #4,d3
        beq.s   L650C
        cmpi.w  #8,d3
        bgt.s   L650C
        lea     L64FC(pc),a5
        moveq   #1,d2
L6554   move.w  0(a6,a1.l),d0
        cmpi.w  #2,d2
        beq.s   L6564
        cmpi.w  #3,d2
        bne.s   L6566
L6564   addq.w  #1,d0
L6566   ror.w   #8,d0
        jsr     L65C0(pc)
        adda.w  #2,a1
        cmp.w   d3,d2
        ble.s   L6554
        cmpi.w  #3,d2
        beq.s   L6582
L657A   moveq   #0,d0
        cmpi.w  #8,d2
        bgt.s   L6588
L6582   jsr     L65C0(pc)
        bra.s   L657A

L6588   move.b  #inso_cmd,(a3)          ;IPC command initiate sound process
        move.b  #16,1(a3)
        move.l  #$4444AA66,2(a3)
        move.b  #1,22(a3)
        moveq   #mt.ipcom,d0
        trap    #1
        bra     L650C

L65A8   move.b  #kiso_cmd,(a3)          ;IPC command kill sound process
        move.b  #0,1(a3)
        move.b  #1,6(a3)
        moveq   #mt.ipcom,d0
        trap    #1
        bra     L6512

L65C0   movea.w (a5)+,a4
        move.w  d0,6(a3,a4.w)
        addq.w  #1,d2
        rts

;BASIC - Procedure 'CALL'

L65CA   jsr     L61E2(pc)               ;ca_gtlin
        bne.s   L65E4
        lsl.l   #2,d3
        beq.s   L65E2
        add.l   d3,bv_rip(a6)
        move.l  0(a6,a1.l),-(a7)
        movem.l 4(a6,a1.l),d1-d7/a0-a5
L65E2   moveq   #err.bp,d0
L65E4   rts

L65E6   moveq   #1,d1
L65E8   move.l  a5,-(a7)
        cmpa.l  a3,a5
        bls.s   L661C
        bclr    #7,1(a6,a3.l)
        beq.s   L661C
        move.l  a3,-(a7)
        lea     8(a3),a5
        andi.b  #$0F,1(a6,a3.l)
        jsr     L5EC8(pc)
        bne.s   L6642
        moveq   #3,d0
        jsr     L5996(pc)
        bne.s   L6642
        movea.l (a7)+,a3
        addq.w  #8,a3
        move.w  0(a6,a1.l),d1
        addq.l  #2,bv_rip(a6)
L661C   movea.l (a7)+,a5
L661E   move.l  d1,d0
        movea.l bv_chbas(a6),a0
        mulu.w  #ch.lench,d0
        adda.l  d0,a0
        cmpa.l  bv_chp(a6),a0
        bge.s   L663E
        move.l  0(a6,a0.l),d0           ;get channel ID
        blt.s   L663E
        movea.l a0,a2
        movea.l d0,a0
        moveq   #err.ok,d0
        rts

L663E   moveq   #err.no,d0
        rts

L6642   addq.w  #8,a7
        rts

L6646   move.l  a0,-(a7)
        bsr.s   L661E
        beq.s   L669E
        cmpa.l  bv_chp(a6),a0
        blt.s   L6680
        move.l  d1,-(a7)
        move.l  a0,d1
        addi.l  #ch.lench,d1
        sub.l   bv_chp(a6),d1
        jsr     L4E7A(pc)               ;reserve space in channel table
        move.l  (a7)+,d1
        bsr.s   L661E
L6668   movea.l bv_chp(a6),a2
        addi.l  #ch.lench,bv_chp(a6)
        move.l  #-1,0(a6,a2.l)
        cmpa.l  a0,a2
        blt.s   L6668
L6680   movea.l a0,a2
        moveq   #10,d0
L6684   clr.l   0(a6,a0.l)
        addq.w  #4,a0
        subq.w  #1,d0
        bgt.s   L6684
        movea.l (a7)+,a0
        move.l  a0,ch.id(a6,a2.l)
        move.w  #80,ch.width(a6,a2.l)
        moveq   #err.ok,d0
        rts

L669E   moveq   #err.ex,d0
        rts

;BASIC - Procedure 'CSIZE'

L66A2   jsr     L8038(pc)
        bne.s   L66C4
        subq.w  #1,d3
        jsr     L8028(pc)
        bne.s   L66C4
        move.w  -2(a6,a1.l),d2
        exg     d1,d2
        cmpi.w  #3,d1
        bhi.s   L66C2
        moveq   #sd.setsz,d4
        jmp     L7FC4(pc)

L66C2   moveq   #err.bp,d0
L66C4   rts

;BASIC - Procedure 'CURSOR'

L66C6   moveq   #-32,d0
        add.l   a5,d0
        sub.l   a3,d0
        beq     L6BE0
        moveq   #sd.pixp,d4
        bra.s   L66D6

;BASIC - Procedure 'AT'

L66D4   moveq   #sd.pos,d4
L66D6   jsr     L8038(pc)
        bne.s   L66F8
        cmpi.w  #2,d3
        bne.s   L66F6
        move.w  0(a6,a1.l),d2
        move.w  -2(a6,a1.l),d1
        cmpi.b  #sd.pos,d4
        bne.s   L66F2
        exg     d1,d2
L66F2   jmp     L7FC4(pc)

L66F6   moveq   #err.bp,d0
L66F8   rts

L66FA   move.l  bv_linum(a6),-(a7)
        move.l  bv_inlin(a6),-(a7)
        move.w  bv_stmnt(a6),-(a7)
        move.l  a4,-(a7)
        jsr     L958E(pc)
        bne.s   L6784
        move.w  bv_dalno(a6),d4
        jsr     L9FA2(pc)
        jsr     LA96A(pc)
        bne.s   L6784
        move.b  bv_dastm(a6),d4
        jsr     LA00A(pc)
        jsr     LA56C(pc)
        cmpi.w  #$8118,d1               ;token keyword 'DATA' ?
        bne.s   L674A
        move.b  bv_daitm(a6),d5
        addq.b  #1,bv_daitm(a6)
L6736   addq.w  #2,a4
        jsr     LA56C(pc)
        subq.b  #1,d5
        beq.s   L676E
        move.w  #$8404,d4               ;token symbol ','
        jsr     LA5E0(pc)
        beq.s   L6736
L674A   jsr     LA60E(pc)
        bne.s   L6784
        jsr     LA56C(pc)
        cmpi.w  #$8118,d1               ;token keyword 'DATA' ?
        bne.s   L674A
        addq.w  #2,a4
        move.w  bv_linum(a6),bv_dalno(a6)
        move.b  bv_stmnt(a6),bv_dastm(a6)
        move.b  #2,bv_daitm(a6)
L676E   movea.l a4,a0
        moveq   #err.ok,d0
L6772   movea.l (a7)+,a4
        move.w  (a7)+,bv_stmnt(a6)
        move.l  (a7)+,bv_inlin(a6)
        move.l  (a7)+,bv_linum(a6)
        tst.l   d0
        rts

L6784   moveq   #err.ef,d0
        bra.s   L6772

;BASIC - Procedure 'ADATE'

L6788   jsr     L61C2(pc)
        bne.s   L67AE
        addq.l  #4,bv_rip(a6)
        move.l  0(a6,a1.l),d1
        moveq   #mt.aclck,d0
        trap    #1
        bra.s   L680C

;BASIC - Procedure 'SDATE'

L679C   jsr     L61E2(pc)               ;ca_gtlin
        bne.s   L680E
        moveq   #err.bp,d0
        lsl.l   #2,d3
        add.l   d3,bv_rip(a6)
        lsr.l   #2,d3
        subq.w  #6,d3
L67AE   bne.s   L680E
        move.l  0(a6,a1.l),d0
        subi.l  #1961,d0
        move.l  d0,d1
        mulu.w  #365,d1
        move.l  4(a6,a1.l),d2
        divu.w  #4,d0
        swap    d0
        cmpi.w  #3,d0
        bne.s   L67D8
        cmpi.w  #2,d2
        ble.s   L67D8
        addq.l  #1,d1
L67D8   clr.w   d0
        swap    d0
        add.l   d0,d1
        subq.l  #1,d2
        asl.w   #1,d2
        lea     L6812(pc,d2.w),a2
        clr.l   d0
        move.w  (a2),d0
        add.l   d0,d1
        add.l   8(a6,a1.l),d1
        subq.w  #1,d1
        moveq   #24,d0
        bsr.s   L682A
        add.l   12(a6,a1.l),d1
        moveq   #60,d0
        bsr.s   L682A
        add.l   16(a6,a1.l),d1
        bsr.s   L682A
        add.l   20(a6,a1.l),d1
        moveq   #mt.sclck,d0
        trap    #1
L680C   moveq   #err.ok,d0
L680E   tst.l   d0
        rts

;Days in month (cumulative)

L6812   dc.w    0,31,59,90,120,151,181,212,243,273,304,334

L682A   bsr.s   L684E
        move.l  d4,d3
        swap    d0
        swap    d1
        bsr.s   L684E
        move.l  d4,d2
        swap    d0
        swap    d3
        bsr.s   L684E
        bsr.s   L6854
        swap    d0
        swap    d1
        bsr.s   L684E
        bsr.s   L6854
        swap    d3
        swap    d0
        move.l  d3,d1
        rts

L684E   move.l  d0,d4
        mulu.w  d1,d4
        rts

L6854   add.w   d4,d3
        clr.w   d4
        swap    d4
        addx.l  d4,d2
        rts

;BASIC - Procedure 'READ'

L685E   move.l  a5,-(a7)
L6860   cmpa.l  (a7),a3
        bge.s   L6886
        jsr     L7A8E(pc)
        bne.s   L6888
        jsr     L66FA(pc)
        bne.s   L6888
        move.b  1(a6,a3.l),d0
        move.l  a3,-(a7)
        jsr     L5A84(pc)
        movea.l (a7)+,a3
        bne.s   L6888
        jsr     L72C2(pc)               ;bp_let
        addq.w  #8,a3
        bra.s   L6860

L6886   moveq   #err.ok,d0
L6888   movea.l (a7)+,a5
        rts

;BASIC - Procedure 'EXEC'

L688C   moveq   #0,d5
        bra.s   L6892

;BASIC - Procedure 'EXEC_W'

L6890   moveq   #-1,d5
L6892   moveq   #io.share,d4
        bsr     L69C4
        bne.s   L68F8
        moveq   #fs.headr,d0
        moveq   #14,d2
        movea.l (a6),a1                 ;bv_bfbas
        bsr     L69A4
        bne.s   L6912
        cmpi.b  #1,-9(a6,a1.l)
        bne     L6990
        moveq   #mt.cjob,d0
        moveq   #-1,d1
        move.l  -14(a6,a1.l),d2
        move.l  -8(a6,a1.l),d3
        suba.l  a1,a1
        movem.l d2/a0/a3,-(a7)
        trap    #1
        movea.l a0,a1
        movem.l (a7)+,d2/a0/a3
        tst.l   d0
        bne.s   L6912
        move.l  d1,d6
        moveq   #fs.load,d0
        bsr     L69A6
        bsr     L693A
        bne.s   L68FA
        moveq   #mt.activ,d0
        move.l  d6,d1
        moveq   #32,d2
        move.l  d5,d3
        trap    #1
        tst.l   d0
        bne.s   L68FA
        tst.l   d5
        bne.s   L68F8
        moveq   #mt.susjb,d0
        moveq   #my.job,d1
        moveq   #25,d3
        suba.l  a1,a1
        trap    #1
L68F8   rts

L68FA   move.l  d0,d4
        moveq   #mt.rjob,d0
        move.l  d6,d1
        trap    #1
        move.l  d4,d0
        rts

;BASIC - Procedure 'LBYTES'

L6906   moveq   #io.share,d4
        bsr     L69C4
        bne.s   L68F8
        jsr     L61E2(pc)               ;ca_gtlin
L6912   bne.s   L6986
        subq.w  #1,d3
        bne.s   L6990
        btst    #0,3(a6,a1.l)
        bne.s   L6990
        movea.l 0(a6,a1.l),a1
        addq.l  #4,bv_rip(a6)
        moveq   #fs.headr,d0
        moveq   #14,d2
        move.l  a1,-(a7)
        bsr.s   L69A6
        movea.l (a7)+,a1
        bne.s   L6986
        move.l  (a1),d2
        moveq   #fs.load,d0
        bsr.s   L69A6
L693A   bra.s   L6986

;BASIC - Procedure 'SEXEC'

L693C   moveq   #1,d5
        bra.s   L6942

;BASIC - Procedure 'SBYTES'

L6940   moveq   #0,d5
L6942   move.l  a3,bv_ptemp(a6)
        moveq   #io.new,d4
        bsr.s   L69C4
        bne.s   L698E
        jsr     L61E2(pc)               ;ca_gtlin
        bne.s   L6986
        subq.w  #2,d3
        sub.w   d5,d3
        bne.s   L699A
        movea.l 0(a6,a1.l),a2
        addq.w  #2,a1
        move.l  2(a6,a1.l),0(a6,a1.l)
        move.w  d5,4(a6,a1.l)
        moveq   #fs.heads,d0
        move.l  a1,-(a7)
        bsr.s   L69A4
        movea.l (a7)+,a1
        bne.s   L6986
        move.l  0(a6,a1.l),d2
        addq.w  #6,a1
        lsl.w   #4,d5
        adda.w  d5,a1
        move.l  a1,bv_rip(a6)
        movea.l a2,a1
        moveq   #fs.save,d0
        bsr.s   L69A6
L6986   move.l  d0,d4
L6988   moveq   #io.close,d0
        trap    #2
L698C   move.l  d4,d0
L698E   rts

L6990   moveq   #err.bp,d0
        bra.s   L6986

L6994   addq.w  #4,a7
L6996   moveq   #err.bp,d0
        rts

L699A   bsr.s   L6988
        movea.l bv_ptemp(a6),a3
        bsr.s   L69E6
        bra.s   L6996

L69A4   trap    #4
L69A6   moveq   #forever,d3
        trap    #3
        tst.l   d0
        rts

L69AE   cmpa.l  a5,a3
        bge.s   L6994
        move.l  a5,-(a7)
        addq.w  #8,a3
        movea.l a3,a5
        jsr     L64A2(pc)
        movea.l (a7)+,a5
        beq.s   L698E
        addq.w  #4,a7
        rts

L69C4   bsr.s   L69AE
        moveq   #io.open,d0
        moveq   #my.job,d1
        move.l  d4,d3
L69CC   movea.l a1,a0
        move.w  0(a6,a1.l),-(a7)
        trap    #4
        trap    #2
        moveq   #3,d3
        add.w   (a7)+,d3
        bclr    #0,d3
        add.l   d3,bv_rip(a6)
        tst.l   d0
        rts

;BASIC - Procedure 'DELETE'

L69E6   bsr.s   L69AE
        moveq   #io.delet,d0
        bra.s   L69CC

;BASIC - Procedure 'DIR'

L69EC   jsr     L65E6(pc)
        bne.s   L698E
        movea.l a0,a4
        moveq   #io.dir,d4
        bsr.s   L69C4
        bne.s   L698E
        movea.l a0,a5
        moveq   #fs.mdinf,d0
        movea.l (a6),a1                 ;bv_bfbas
        movea.l a5,a0
        bsr.s   L69A4
        bne.s   L6986
        move.l  d1,-(a7)
        movea.l (a6),a1                 ;bv_bfbas
        moveq   #10,d2
        bsr.s   L6A6A
        moveq   #io.sbyte,d0
        moveq   #10,d1
        trap    #3
        movem.w (a7)+,d1-d2
        bsr.s   L6A40
L6A1A   moveq   #io.fstrg,d0
        moveq   #64,d2
        movea.l (a6),a1                 ;bv_bfbas
        movea.l a5,a0
        moveq   #0,d4
        bsr     L69A4
        bne     L6988
        suba.w  #$30,a1
        move.w  -2(a6,a1.l),d2
        beq.s   L6A1A
        bsr.s   L6A6A
        moveq   #io.sbyte,d0
        moveq   #10,d1
        trap    #3
        bra.s   L6A1A

L6A40   move.w  d2,d4
        bsr.s   L6A5A
        moveq   #io.sbyte,d0
        move.b  #'/',d1
        trap    #3
        move.w  d4,d1
        bsr.s   L6A5A
        moveq   #err.sc,d0
        jsr     L3968(pc)               ;ut_err
        moveq   #err.ok,d0
        rts

L6A5A   movea.l (a6),a1                 ;bv_bfbas
        lea     2(a1),a0
        move.w  d1,0(a6,a1.l)
        jsr     L3E54(pc)               ;cn_itod
        move.w  d1,d2
L6A6A   moveq   #io.sstrg,d0
        movea.l a4,a0
        moveq   #-1,d3
        bra     L69A4

;BASIC - Procedure 'FORMAT'

L6A74   jsr     L65E6(pc)
        bne     L698E
        movea.l a0,a4
        bsr     L69AE
        moveq   #io.formt,d0
        bsr     L69CC
        bne     L698E
        bra.s   L6A40

;BASIC - Procedure 'COPY'

L6A8E   moveq   #0,d5
        bra.s   L6A94

;BASIC - Procedure 'COPY_N'

L6A92   moveq   #-1,d5
L6A94   moveq   #io.share,d4
        bsr     L69C4
        bne     L698E
        movea.l a0,a4
        moveq   #io.new,d4
        bsr     L69C4
        bne.s   L6B20
        tst.b   d5
        bne.s   L6ACA
        moveq   #-1,d5
        movea.l (a6),a1                 ;bv_bfbas
        moveq   #fs.headr,d0
        moveq   #14,d2
        exg     a0,a4
        bsr     L69A4
        exg     a0,a4
        bne.s   L6ACA
        movea.l (a6),a1                 ;bv_bfbas
        move.l  0(a6,a1.l),d5
        moveq   #fs.heads,d0
        bsr     L69A4
L6ACA   movea.l (a6),a1                 ;bv_bfbas
        move.l  bv_tkbas(a6),d2
        sub.l   a1,d2
        moveq   #0,d3
        exg     a0,a4
        tst.l   d5
        ble.s   L6AE0
        cmp.l   d5,d2
        ble.s   L6AE0
        move.l  d5,d2
L6AE0   tas     bv_brk(a6)
        beq.s   L6B1E
        moveq   #io.fstrg,d0
        trap    #4
        trap    #3
        cmpi.l  #err.nc,d0
        bne.s   L6AFA
        tst.w   d1
        beq.s   L6AE0
        bra.s   L6B0C

L6AFA   cmpi.l  #err.ef,d0
        bne.s   L6B08
        moveq   #0,d0
        tst.w   d1
        beq.s   L6B1E
L6B08   tst.l   d0
        bne.s   L6B1E
L6B0C   moveq   #io.sstrg,d0
        move.w  d1,d2
        exg     a0,a4
        suba.w  d1,a1
        bsr     L69A4
        bne.s   L6B1E
        sub.l   d2,d5
        bne.s   L6ACA
L6B1E   bsr.s   L6B22
L6B20   exg     a0,a4
L6B22   bra     L6986

;BASIC - Procedure 'CLOSE'

L6B26   cmpa.l  a3,a5
        ble.s   L6B44
        tst.b   1(a6,a3.l)
        bpl.s   L6B44
        jsr     L65E6(pc)
        bne.s   L6B42
        move.l  #-1,0(a6,a2.l)
        moveq   #io.close,d0
        trap    #2
L6B42   rts

L6B44   moveq   #err.bp,d0
        bra.s   L6B42

;BASIC - Procedure 'OPEN'

L6B48   moveq   #io.old,d4
        bra.s   L6B52

;BASIC - Procedure 'OPEN_IN'

L6B4C   moveq   #io.share,d4
        bra.s   L6B52

;BASIC - Procedure 'OPEN_NEW'

L6B50   moveq   #io.new,d4
L6B52   bsr.s   L6B26
        tst.l   d0
        beq.s   L6B5E
        moveq   #err.no,d2
        cmp.l   d0,d2
        bne.s   L6B42
L6B5E   exg     d1,d6
        bsr     L69C4
        exg     d6,d1
        bne.s   L6B42
        jmp     L6646(pc)

;BASIC - Procedure 'SAVE'

L6B6C   move.l  a3,bv_ptemp(a6)
        moveq   #io.new,d4
        bsr     L69C4
        bne.s   L6B42
        jsr     L7484(pc)
        beq.s   L6B22
        move.l  d0,d4
        bsr     L699A
        bra     L698C

;BASIC - Procedure 'FILL'

L6B88   jsr     L65E6(pc)
        bne.s   L6BA4
        jsr     L61C2(pc)
        bne.s   L6BA4
        move.l  0(a6,a1.l),d1
        addq.l  #4,a1
        move.l  a1,bv_rip(a6)
        moveq   #sd.flood,d0
        moveq   #forever,d3
        trap    #3
L6BA4   rts

;BASIC - Procedure 'UNDER'

L6BA6   move.b  #sd.setul,d4
        bra.s   L6BB0

;BASIC - Procedure 'FLASH'

L6BAC   move.b  #sd.setfl,d4
L6BB0   jsr     L8038(pc)
        bne.s   L6BC0
        jsr     L8028(pc)
        bne.s   L6BC0
L6BBC   jmp     L7FC4(pc)
L6BC0   rts

;BASIC - Procedure 'OVER'

L6BC2   jsr     L8038(pc)
        bne.s   L6BC0
        move.b  #sd.setmd,d4
        move.w  0(a6,a1.l),d1
        cmpi.w  #1,d1
        bgt.s   L6BDC
        cmpi.w  #-1,d1
        bge.s   L6BBC
L6BDC   moveq   #err.bp,d0
        rts

L6BE0   bsr     L6CB6
        jsr     L61DE(pc)               ;ca_gtfp
        bne.s   L6BF2
        moveq   #sd.gcur,d0
        moveq   #forever,d3
        trap    #4
        trap    #3
L6BF2   bra     L6CAE

;BASIC - Procedure 'SCALE'

L6BF6   moveq   #sd.scale,d4
        bsr     L6CB6
        bsr     L6D42
        bsr     L6D46
        bra     L6C96

;BASIC - Procedure 'POINT'

L6C08   moveq   #sd.point,d4
        bra.s   L6C10

;BASIC - Procedure 'POINT_R'

L6C0C   move.w  #sd.point+$80,d4
L6C10   bsr     L6CB6
        bsr     L6D1C
        bra.s   L6C96

;BASIC - Procedure 'LINE'

L6C1A   moveq   #sd.line,d4
        bra.s   L6C22

;BASIC - Procedure 'LINE_R'

L6C1E   move.w  #sd.line+$80,d4
L6C22   bsr     L6CB6
        bsr     L6D0A
        bne.s   L6CA4
        bsr     L6D1C
        bra.s   L6C96

;BASIC - Procedure 'ELLIPSE'

L6C32   moveq   #sd.elips,d4
        bra.s   L6C3A

;BASIC - Procedure 'ELLIPSE_R'

L6C36   move.w  #sd.elips+$80,d4
L6C3A   bsr.s   L6CB6
        bsr     L6D1C
        bsr     L6D46
        cmpi.b  #1,d5
        bne.s   L6C50
        bsr     L6D42
        bra.s   L6C68

L6C50   subq.l  #8,a1
        subq.l  #4,a1
        clr.l   0(a6,a1.l)
        move.l  #$0801,4(a6,a1.l)
        move.l  #$40000000,8(a6,a1.l)   ;fp[1]
L6C68   movem.w 6(a6,a1.l),d0-d2
        move.w  12(a6,a1.l),6(a6,a1.l)
        move.l  14(a6,a1.l),8(a6,a1.l)
        movem.w d0-d2,12(a6,a1.l)
        bra.s   L6C96

;BASIC - Procedure 'ARC'

L6C82   moveq   #sd.arc,d4
        bra.s   L6C8A

;BASIC - Procedure 'ARC_R'

L6C86   move.w  #sd.arc+$80,d4
L6C8A   bsr.s   L6CB6
        bsr.s   L6D0A
        bsr     L6D1C
        bsr     L6D46
L6C96   move.l  d4,d0
        swap    d0
        bclr    #7,d0
        moveq   #forever,d3
        trap    #4
        trap    #3
L6CA4   cmpa.l  d6,a3
        bge.s   L6CAC
        move.l  a4,-(a7)
        move.w  d5,d4
L6CAC   moveq   #err.ok,d0
L6CAE   move.l  d7,bv_rip(a6)
        movea.l d7,a1
        rts

L6CB6   swap    d4
        clr.w   d4
        clr.w   d5
        move.l  bv_rip(a6),d7
        move.l  a5,d6
        cmpa.l  d6,a3
        blt.s   L6CCA
        moveq   #err.bp,d0
        bra.s   L6D06

L6CCA   move.b  1(a6,a3.l),d0
        andi.b  #$0F,d0
        bne.s   L6CDC
        jsr     L7452(pc)
        move.w  d5,d4
        bra.s   L6CEE

L6CDC   btst    #7,1(a6,a3.l)
        beq.s   L6CEE
        move.b  1(a6,a3.l),d4
        bclr    #7,d4
        lsr.b   #4,d4
L6CEE   jsr     L65E6(pc)
        bne.s   L6D06
        move.w  #256,d1
        jsr     L4E4E(pc)               ;bv_chrix
        move.l  bv_rip(a6),d7
        movea.l d7,a1
        movea.l (a7),a4
        rts

L6D06   addq.l  #4,a7
        bra.s   L6CAE

L6D0A   cmpi.b  #5,d4
        bne.s   L6D1C
        bsr.s   L6D6E
        bsr.s   L6D7E
        move.l  a1,bv_rip(a6)
        moveq   #err.ok,d0
        rts

L6D1C   bsr.s   L6D4C
        bne.s   L6D06
        btst    #23,d4
        beq.s   L6D2C
        bsr.s   L6D6E
        jsr     L4838(pc)               ;ri.add
L6D2C   bsr.s   L6D4C
        bne.s   L6D06
        btst    #23,d4
        beq.s   L6D3C
        bsr.s   L6D7E
        jsr     L4838(pc)               ;ri.add
L6D3C   bsr.s   L6D8E
        moveq   #1,d0
        rts

L6D42   bsr.s   L6D4C
        bne.s   L6D06
L6D46   bsr.s   L6D4C
        bne.s   L6D06
        rts

L6D4C   move.l  a4,-(a7)
        cmpa.l  d6,a3
        blt.s   L6D56
        moveq   #err.bp,d0
        bra.s   L6D6A

L6D56   move.w  d5,d4
        jsr     L7452(pc)
        movea.l a3,a5
        jsr     L5EC8(pc)
        bne.s   L6D6A
        moveq   #2,d0
        jsr     L5996(pc)
L6D6A   movea.l (a7)+,a4
        rts

L6D6E   subq.w  #6,a1
        move.l  10(a6,a2.l),0(a6,a1.l)
        move.w  14(a6,a2.l),4(a6,a1.l)
        rts

L6D7E   subq.w  #6,a1
        move.l  4(a6,a2.l),0(a6,a1.l)
        move.w  8(a6,a2.l),4(a6,a1.l)
        rts

L6D8E   move.l  0(a6,a1.l),4(a6,a2.l)
        move.l  4(a6,a1.l),8(a6,a2.l)
        move.l  8(a6,a1.l),12(a6,a2.l)
        rts

;Introduce BASIC keywords

L6DA2   lea     L6E22(pc),a1            ;list of BASIC keywords

;Vector $110 BP_INIT - Add SuperBASIC extensions

L6DA6   movem.l d1-d2/d5-d7/a0/a2-a3,-(a7)
        moveq   #8,d6
L6DAC   moveq   #0,d7
        moveq   #0,d5
        move.w  (a1)+,d5
        lsl.l   #3,d5
        move.l  d5,d1
        move.l  a1,-(a7)
        jsr     L4E60(pc)               ;reserve space on name table stack
        move.l  d5,d1
        jsr     L4E72(pc)               ;reserve space on name list stack
        movea.l (a7)+,a3
L6DC4   movea.l a3,a1
        move.w  (a3)+,d0
        beq.s   L6E0C
        adda.w  d0,a1
        move.l  a1,-(a7)
        move.b  (a3)+,d5
        move.b  d5,-(a7)
        move.b  d6,-(a7)
        move.b  d5,d1
        movea.l (a6),a1                 ;bv_bfbas
L6DD8   move.b  (a3)+,0(a6,a1.l)
        addq.w  #1,a1
        subq.b  #1,d1
        bgt.s   L6DD8
        move.l  a3,-(a7)
        movea.l (a6),a3                 ;bv_bfbas
        lea     L8B5A(pc),a2            ;(commands syntax table)
        movea.l (a2),a2
        jsr     L8622(pc)
        bra.s   L6E16                   ;fail
        movea.l (a7)+,a3                ;OK
        move.b  (a7)+,d6
        move.b  (a7)+,d5
        move.l  (a7)+,4(a6,a2.l)
        move.b  d6,0(a6,a2.l)
        move.b  d7,1(a6,a2.l)
        btst    d7,d5
        bne.s   L6DC4
        addq.w  #1,a3
        bra.s   L6DC4

L6E0C   subq.w  #8,d6
        bne.s   L6E1C
        moveq   #9,d6
        movea.l a3,a1
        bra.s   L6DAC

L6E16   adda.w  #12,a7
        moveq   #err.bn,d0
L6E1C   movem.l (a7)+,d1-d2/d5-d7/a0/a2-a3
        rts

;BASIC keywords

L6E22   dc.w    81                      ;procedures
        dc.w    L76A8-*
        dc.b    5,'PRINT'
        dc.w    L7DA4-*
        dc.b    3,'RUN'
        dc.w    L7E0A-*
        dc.b    4,'STOP',0
        dc.w    L76A6-*
        dc.b    5,'INPUT'
        dc.w    L7F7E-*
        dc.b    6,'WINDOW',0
        dc.w    L7FA4-*
        dc.b    6,'BORDER',0
        dc.w    L75CA-*
        dc.b    3,'INK'
        dc.w    L75CE-*
        dc.b    5,'STRIP'
        dc.w    L75D2-*
        dc.b    5,'PAPER'
        dc.w    L7F8C-*
        dc.b    5,'BLOCK'
        dc.w    L75F4-*
        dc.b    3,'PAN'
        dc.w    L75F8-*
        dc.b    6,'SCROLL',0
        dc.w    L66A2-*
        dc.b    5,'CSIZE'
        dc.w    L6BAC-*
        dc.b    5,'FLASH'
        dc.w    L6BA6-*
        dc.b    5,'UNDER'
        dc.w    L6BC2-*
        dc.b    4,'OVER',0
        dc.w    L66C6-*
        dc.b    6,'CURSOR',0
        dc.w    L66D4-*
        dc.b    2,'AT',0
        dc.w    L6BF6-*
        dc.b    5,'SCALE'
        dc.w    L6C08-*
        dc.b    5,'POINT'
        dc.w    L6C1A-*
        dc.b    4,'LINE',0
        dc.w    L6C32-*
        dc.b    7,'ELLIPSE'
        dc.w    L6C32-*
        dc.b    6,'CIRCLE',0
        dc.w    L6C82-*
        dc.b    3,'ARC'
        dc.w    L6C0C-*
        dc.b    7,'POINT_R'
        dc.w    L7E98-*
        dc.b    4,'TURN',0
        dc.w    L7E90-*
        dc.b    6,'TURNTO',0
        dc.w    L7ED2-*
        dc.b    5,'PENUP'
        dc.w    L7ED6-*
        dc.b    7,'PENDOWN'
        dc.w    L7EE4-*
        dc.b    4,'MOVE',0
        dc.w    L7480-*
        dc.b    4,'LIST',0
        dc.w    L6B48-*
        dc.b    4,'OPEN',0
        dc.w    L6B26-*
        dc.b    5,'CLOSE'
        dc.w    L6A74-*
        dc.b    6,'FORMAT',0
        dc.w    L6A8E-*
        dc.b    4,'COPY',0
        dc.w    L6A92-*
        dc.b    6,'COPY_N',0
        dc.w    L69E6-*
        dc.b    6,'DELETE',0
        dc.w    L69EC-*
        dc.b    3,'DIR'
        dc.w    L688C-*
        dc.b    4,'EXEC',0
        dc.w    L6890-*
        dc.b    6,'EXEC_W',0
        dc.w    L6906-*
        dc.b    6,'LBYTES',0
        dc.w    L693C-*
        dc.b    5,'SEXEC'
        dc.w    L6940-*
        dc.b    6,'SBYTES',0
        dc.w    L6B6C-*
        dc.b    4,'SAVE',0
        dc.w    L7DCA-*
        dc.b    5,'MERGE'
        dc.w    L7DD4-*
        dc.b    4,'MRUN',0
        dc.w    L7DF4-*
        dc.b    4,'LOAD',0
        dc.w    L7DFA-*
        dc.b    4,'LRUN',0
        dc.w    L7E06-*
        dc.b    3,'NEW'
        dc.w    L7D98-*
        dc.b    5,'CLEAR'
        dc.w    L6B4C-*
        dc.b    7,'OPEN_IN'
        dc.w    L6B50-*
        dc.b    8,'OPEN_NEW',0
        dc.w    L75F0-*
        dc.b    3,'CLS'
        dc.w    L65CA-*
        dc.b    4,'CALL',0
        dc.w    L7ACC-*
        dc.b    5,'RECOL'
        dc.w    L7998-*
        dc.b    9,'RANDOMISE'
        dc.w    L7648-*
        dc.b    5,'PAUSE'
        dc.w    L766C-*
        dc.b    4,'POKE',0
        dc.w    L7674-*
        dc.b    6,'POKE_W',0
        dc.w    L767A-*
        dc.b    6,'POKE_L',0
        dc.w    L64E2-*
        dc.b    4,'BAUD',0
        dc.w    L651E-*
        dc.b    4,'BEEP',0
        dc.w    L7E50-*
        dc.b    8,'CONTINUE',0
        dc.w    L7E46-*
        dc.b    5,'RETRY'
        dc.w    L685E-*
        dc.b    4,'READ',0
        dc.w    L75AE-*
        dc.b    3,'NET'
        dc.w    L7590-*
        dc.b    4,'MODE',0
        dc.w    L7B24-*
        dc.b    5,'RENUM'
        dc.w    L7462-*
        dc.b    5,'DLINE'
        dc.w    L679C-*
        dc.b    5,'SDATE'
        dc.w    L6788-*
        dc.b    5,'ADATE'
        dc.w    L6C1E-*
        dc.b    6,'LINE_R',0
        dc.w    L6C36-*
        dc.b    9,'ELLIPSE_R'
        dc.w    L6C36-*
        dc.b    8,'CIRCLE_R',0
        dc.w    L6C86-*
        dc.b    5,'ARC_R'
        dc.w    L7AFA-*
        dc.b    4,'AUTO',0
        dc.w    L7AF6-*
        dc.b    4,'EDIT',0
        dc.w    L6B88-*
        dc.b    4,'FILL',0
        dc.w    L7F68-*
        dc.b    5,'WIDTH'
        dc.w    L7D78-*
        dc.b    6,'REPORT',0
        dc.w    L7E58-*
        dc.b    3,'TRA'
        dc.w    0

        dc.w    58                      ;functions
        dc.w    L8054-*
        dc.b    4,'ACOS',0
        dc.w    L805A-*
        dc.b    4,'ACOT',0
        dc.w    L8060-*
        dc.b    4,'ASIN',0
        dc.w    L8066-*
        dc.b    4,'ATAN',0
        dc.w    L806C-*
        dc.b    3,'COS'
        dc.w    L8072-*
        dc.b    3,'COT'
        dc.w    L8078-*
        dc.b    3,'EXP'
        dc.w    L807E-*
        dc.b    2,'LN',0
        dc.w    L8084-*
        dc.b    5,'LOG10'
        dc.w    L808A-*
        dc.b    3,'SIN'
        dc.w    L8090-*
        dc.b    4,'SQRT',0
        dc.w    L8096-*
        dc.b    3,'TAN'
        dc.w    L809C-*
        dc.b    3,'DEG'
        dc.w    L80A2-*
        dc.b    3,'RAD'
        dc.w    L80EA-*
        dc.b    3,'RND'
        dc.w    L8154-*
        dc.b    3,'INT'
        dc.w    L80C2-*
        dc.b    3,'ABS'
        dc.w    L8142-*
        dc.b    2,'PI',0
        dc.w    L816C-*
        dc.b    4,'PEEK',0
        dc.w    L8174-*
        dc.b    6,'PEEK_W',0
        dc.w    L817E-*
        dc.b    6,'PEEK_L',0
        dc.w    L81A2-*
        dc.b    5,'RESPR'
        dc.w    L81C8-*
        dc.b    3,'EOF'
        dc.w    L820E-*
        dc.b    6,'INKEY$',0
        dc.w    L8266-*
        dc.b    4,'CHR$',0
        dc.w    L82DE-*
        dc.b    4,'CODE',0
        dc.w    L836E-*
        dc.b    6,'KEYROW',0
        dc.w    L81B8-*
        dc.b    7,'BEEPING'
        dc.w    L82C8-*
        dc.b    3,'LEN'
        dc.w    L8306-*
        dc.b    4,'DIMN',0
        dc.w    L83BA-*
        dc.b    4,'DAY$',0
        dc.w    L8356-*
        dc.b    4,'DATE',0
        dc.w    L83B4-*
        dc.b    5,'DATE$'
        dc.w    L8278-*
        dc.b    5,'FILL$'
        dc.w    L81F2-*
        dc.b    4,'VER$',0
        dc.w    L8410-*
        dc.b    6,'ERR_NC',0
        dc.w    L840E-*
        dc.b    6,'ERR_NJ',0
        dc.w    L840C-*
        dc.b    6,'ERR_OM',0
        dc.w    L840A-*
        dc.b    6,'ERR_OR',0
        dc.w    L8408-*
        dc.b    6,'ERR_BO',0
        dc.w    L8406-*
        dc.b    6,'ERR_NO',0
        dc.w    L8404-*
        dc.b    6,'ERR_NF',0
        dc.w    L8402-*
        dc.b    6,'ERR_EX',0
        dc.w    L8400-*
        dc.b    6,'ERR_IU',0
        dc.w    L83FE-*
        dc.b    6,'ERR_EF',0
        dc.w    L83FC-*
        dc.b    6,'ERR_DF',0
        dc.w    L83FA-*
        dc.b    6,'ERR_BN',0
        dc.w    L83F8-*
        dc.b    6,'ERR_TE',0
        dc.w    L83F6-*
        dc.b    6,'ERR_FF',0
        dc.w    L83F4-*
        dc.b    6,'ERR_BP',0
        dc.w    L83F2-*
        dc.b    6,'ERR_FE',0
        dc.w    L83F0-*
        dc.b    6,'ERR_XP',0
        dc.w    L83EE-*
        dc.b    6,'ERR_OV',0
        dc.w    L83EC-*
        dc.b    6,'ERR_NI',0
        dc.w    L83EA-*
        dc.b    6,'ERR_RO',0
        dc.w    L83E8-*
        dc.b    6,'ERR_BL',0
        dc.w    L845A-*
        dc.b    5,'ERNUM'
        dc.w    L8468-*
        dc.b    5,'ERLIN'
        dc.w    0

;Vector $120 BP_LET - Return parameter values

L72C2   movem.l d4/a0/a3-a5,-(a7)
        move.b  1(a6,a3.l),d0
        andi.b  #$0F,d0
        subq.b  #2,d0
        bgt.s   L72EE
        beq.s   L72F8
        cmpi.b  #3,0(a6,a3.l)
        beq.s   L72E4
        addq.b  #2,d0
        bgt.s   L7350
        bra     L7396

L72E4   addq.b  #2,d0
        bgt     L73B4
        bra     L73BE

L72EE   moveq   #2,d1
        bsr.s   L7308
        add.l   d1,bv_rip(a6)
        bra.s   L7330

L72F8   moveq   #6,d1
        bsr.s   L7308
        move.l  2(a6,a1.l),2(a6,a0.l)
        add.l   d1,bv_rip(a6)
        bra.s   L7330

L7308   move.l  4(a6,a3.l),d4
        bge.s   L7310
        bsr.s   L7338
L7310   movea.l bv_vvbas(a6),a0
        adda.l  d4,a0
        movea.l bv_rip(a6),a1
        move.w  0(a6,a1.l),0(a6,a0.l)
L7320   moveq   #1,d0
        sub.b   0(a6,a3.l),d0
        blt.s   L7334
        move.b  #2,0(a6,a3.l)
        bra.s   L7334

L7330   movem.l (a7)+,d4/a0/a3-a5
L7334   moveq   #err.ok,d0
        rts

L7338   movem.l a2-a3,-(a7)
        jsr     L4DF6(pc)
        movem.l (a7)+,a2-a3
        move.l  a0,d4
        sub.l   bv_vvbas(a6),d4
        move.l  d4,4(a6,a3.l)
        rts

L7350   bsr.s   L73C6
        andi.l  #$0000FFFF,d1
        move.l  4(a6,a3.l),d4
        blt.s   L7382
        movea.l bv_vvbas(a6),a0
        adda.l  d4,a0
        moveq   #1,d2
        add.w   0(a6,a0.l),d2
        bclr    #0,d2
        cmp.w   d1,d2
        beq.s   L7388
        addq.l  #2,d2
        movem.l d1/a1/a3,-(a7)
        move.l  d2,d1
        jsr     L4FE8(pc)
        movem.l (a7)+,d1/a1/a3
L7382   addq.l  #2,d1
        bsr.s   L7338
        subq.l  #2,d1
L7388   bsr.s   L73DE
        bsr.s   L73EC
        moveq   #0,d0
        moveq   #0,d2
        bsr.s   L7400
        bsr.s   L7320
L7394   bra.s   L7330

L7396   move.b  #1,1(a6,a3.l)
        bsr.s   L73C6
        subq.w  #1,d5
        addq.w  #2,a2
        adda.w  d5,a2
        movea.l a2,a0
        sub.w   d5,d6
        move.w  d6,d2
        bsr     L743C
        addq.w  #2,a1
L73B0   bsr.s   L7400
        bra.s   L7394

L73B4   bsr.s   L73C6
        bsr.s   L742C
        bsr.s   L73DE
        bsr.s   L73EC
        bra.s   L73B0

L73BE   bsr.s   L73C6
        bsr.s   L742C
        bsr.s   L73DE
        bra.s   L73B0

L73C6   movea.l bv_rip(a6),a1
        moveq   #0,d1
        move.w  0(a6,a1.l),d1
        move.w  d1,d0
        swap    d1
        move.w  d0,d1
        addq.w  #1,d1
        bclr    #0,d1
        rts

L73DE   movea.l bv_vvbas(a6),a0
        adda.l  d4,a0
        movea.l bv_rip(a6),a1
        addq.w  #2,a1
        rts

L73EC   move.w  -2(a6,a1.l),0(a6,a0.l)
        cmp.w   0(a6,a0.l),d1
        bcc.s   L73FC
        move.w  d1,0(a6,a0.l)
L73FC   addq.w  #2,a0
        rts

L7400   tst.w   d1
L7402   beq.s   L7412
        move.b  0(a6,a1.l),0(a6,a0.l)
        addq.w  #1,a0
        addq.w  #1,a1
        subq.w  #1,d1
        bra.s   L7402

L7412   tst.w   d2
L7414   beq.s   L7422
        move.b  #32,0(a6,a0.l)
        addq.w  #1,a0
        subq.w  #1,d2
        bra.s   L7414

L7422   adda.w  d0,a1
        move.l  a1,bv_rip(a6)
        moveq   #err.ok,d0
        rts

L742C   movea.l 4(a6,a3.l),a0
        adda.l  bv_vvbas(a6),a0
        move.w  6(a6,a0.l),d2
        move.l  0(a6,a0.l),d4
L743C   move.w  d1,d0
        swap    d1
        cmp.w   d2,d1
        bhi.s   L744A
        sub.w   d1,d2
        sub.w   d1,d0
        rts

L744A   move.w  d2,d1
        sub.w   d2,d0
        moveq   #0,d2
        rts

L7452   move.b  1(a6,a3.l),d5
        lsr.b   #4,d5
        andi.b  #$0F,1(a6,a3.l)
        addq.w  #8,a3
        rts

;BASIC - Procedure 'DLINE'

L7462   cmpa.l  a5,a3
        bge     L750E
        st      d7
        jsr     L7E30(pc)
        beq.s   L7476
        st      bv_undo(a6)
L7474   rts

L7476   bsr.s   L747A
        bra.s   L7492

L747A   moveq   #2,d1
        jmp     L65E8(pc)

;BASIC - Procedure 'LIST'

L7480   bsr.s   L747A
        blt.s   L7474
L7484   st      bv_print(a6)
        clr.w   bv_lsfil(a6)
        cmpa.l  a5,a3
        bge     L7512
L7492   move.l  a5,-(a7)
L7494   cmpa.l  (a7),a3
        bge.s   L74DE
        bsr.s   L7452
        bne.s   L74A0
        moveq   #0,d4
        bra.s   L74A6

L74A0   bsr.s   L74F0
        bne.s   L74DA
        move.w  d1,d4
L74A6   subq.b  #5,d5
        beq.s   L74B6
        addq.b  #5,d5
        move.w  d4,d6
        bne.s   L74CE
        tst.b   d7
        bne.s   L7494
        bra.s   L74BE

L74B6   cmpa.l  (a7),a3
        bge.s   L74BE
        bsr.s   L7452
        bne.s   L74C4
L74BE   move.w  #$7FFF,d6
        bra.s   L74CE

L74C4   bsr.s   L74F0
        bne.s   L74DA
        move.w  d1,d6
        cmp.w   d4,d6
        blt.s   L74DA
L74CE   bsr.s   L7518                   ;(convert precompiled basic to ascii)
        cmpi.b  #1,d5
        beq.s   L7494
        tst.b   d5
        beq.s   L74DE
L74DA   moveq   #err.bp,d0
        bra.s   L74EC

L74DE   tst.b   d7
        beq.s   L74EA
        moveq   #0,d2
        moveq   #0,d5
        jsr     L8FE6(pc)
L74EA   moveq   #err.ok,d0
L74EC   movea.l (a7)+,a5
        rts

L74F0   movea.l a3,a5
        jsr     L5EC8(pc)
        bne.s   L7510
        moveq   #3,d0
        jsr     L5996(pc)
        bne.s   L7510
        addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),d1
        bgt.s   L750E
        moveq   #err.bp,d0
        rts

L750E   moveq   #err.ok,d0
L7510   rts

L7512   moveq   #0,d4
        move.w  #$7FFF,d6

;Vector 'convert precompiled basic to ascii'

L7518   movea.l bv_pfbas(a6),a4
        clr.l   bv_linum(a6)
        tst.w   d4
        beq.s   L7530
        move.l  a0,-(a7)
        jsr     L9FBE(pc)
        movea.l (a7)+,a0
        move.w  d2,bv_linum(a6)
L7530   tst.b   d7
        bne.s   L753C
        lea     L8B5A(pc),a2            ;(commands syntax table)
        jmp     L90B6(pc)

L753C   cmp.w   4(a6,a4.l),d6
        blt.s   L750E
        move.l  a4,-(a7)
        move.w  bv_length(a6),-(a7)
L7548   cmpa.l  bv_pfp(a6),a4
        bge.s   L7588
        move.w  0(a6,a4.l),d1
        addq.w  #2,a4
        add.w   d1,bv_length(a6)
        adda.w  bv_length(a6),a4
        cmp.w   4(a6,a4.l),d6
        bge.s   L7548
        move.w  0(a6,a4.l),d1
        add.w   bv_length(a6),d1
        sub.w   (a7)+,d1
        move.w  d1,0(a6,a4.l)
        movea.l (a7)+,a2
L7572   move.w  0(a6,a4.l),0(a6,a2.l)
        addq.w  #2,a4
        addq.w  #2,a2
        cmpa.l  bv_pfp(a6),a4
        blt.s   L7572
        move.l  a2,bv_pfp(a6)
L7586   bra.s   L750E

L7588   addq.w  #2,a7
        move.l  (a7)+,bv_pfp(a6)
        bra.s   L7586

;BASIC - Procedure 'MODE'

L7590   jsr     L61BE(pc)
        bne.s   L75AC
        addq.l  #2,bv_rip(a6)
        move.w  #$0108,d1
        and.w   d1,0(a6,a1.l)
        bne.s   L75A6
        moveq   #0,d1
L75A6   moveq   #-1,d2
        moveq   #mt.dmode,d0
        trap    #1
L75AC   rts

;BASIC - Procedure 'NET'

L75AE   jsr     L61BE(pc)
        bne.s   L75C4
        addq.l  #2,bv_rip(a6)
        move.b  1(a6,a1.l),d1
        ble.s   L75C6
        move.b  d1,sv_netnr+sv_base
L75C4   rts

L75C6   moveq   #err.bp,d0
        rts

;BASIC - Procedure 'INK'

L75CA   moveq   #sd.setin,d4
        bra.s   L75D4

;BASIC - Procedure 'STRIP'

L75CE   moveq   #sd.setst,d4
        bra.s   L75D4

;BASIC - Procedure 'PAPER'

L75D2   moveq   #sd.setpa,d4
L75D4   jsr     L8038(pc)
        bne.s   L75EE
        bsr     L7FD4
        bne.s   L75EE
        cmpi.b  #sd.setpa,d4
        bne.s   L75EA
        bsr.s   L75EA
        moveq   #sd.setst,d4
L75EA   jmp     L7FC4(pc)

L75EE   rts

;BASIC - Procedure 'CLS'

L75F0   moveq   #sd.clear,d4
        bra.s   L75FA

;BASIC - Procedure 'PAN'

L75F4   moveq   #sd.pan,d4
        bra.s   L75FA

;BASIC - Procedure 'SCROLL'

L75F8   moveq   #sd.scrol,d4
L75FA   jsr     L8038(pc)
        bne.s   L7642
        move.w  0(a6,a1.l),d1
        cmpi.b  #sd.clear,d4
        beq.s   L760E
        subq.l  #2,a1
        subq.l  #1,d3
L760E   subq.w  #1,d3
        bhi.s   L7640
        blt.s   L762C
        move.w  d4,d3
        subi.w  #23,d3
        lsr.w   #2,d3
        lea     L7644(pc),a2
        btst    d1,0(a2,d3.w)
        beq.s   L7640
        add.l   d1,d4
        move.w  0(a6,a1.l),d1
L762C   jsr     L90A2(pc)
        bne.s   L763C
        move.l  bv_lnbas(a6),bv_lnp(a6)
        clr.w   bv_lsbas(a6)
L763C   jmp     L7FC4(pc)

L7640   moveq   #err.bp,d0
L7642   rts

L7644   btst    d3,(a1)+
        move.b  d0,-(a7)

;BASIC - Procedure 'PAUSE'

L7648   jsr     L61DA(pc)               ;ca_gtint
        subq.w  #1,d3
        blt.s   L765A
        bgt.s   L7668
        addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),d3
L765A   moveq   #0,d1
        jsr     L661E(pc)
        moveq   #io.fbyte,d0
        trap    #3
        moveq   #err.ok,d0
        rts

L7668   moveq   #err.bp,d0
        rts

;BASIC - Procedure 'POKE'

L766C   moveq   #0,d4
        bsr.s   L7682
        move.b  d1,(a4)
        rts

;BASIC - Procedure 'POKE_W'

L7674   bsr.s   L7680
        move.w  d1,(a4)
        rts

;BASIC - Procedure 'POKE_L'

L767A   bsr.s   L7680
        move.l  d1,(a4)
        rts

L7680   moveq   #1,d4
L7682   jsr     L61E2(pc)               ;ca_gtlin
        bne.s   L76A4
        subq.w  #2,d3
        bne.s   L76A0
        addq.l  #8,bv_rip(a6)
        movea.l 0(a6,a1.l),a4
        move.l  4(a6,a1.l),d1
        move.l  a4,d0
        and.l   d4,d0
        bne.s   L76A0
        rts

L76A0   addq.w  #4,a7
        moveq   #err.bp,d0
L76A4   rts

;BASIC - Procedure 'INPUT'

L76A6   st      d7

;BASIC - Procedure 'PRINT'

L76A8   jsr     L4E4C(pc)
        moveq   #0,d4
        jsr     L65E6(pc)
        bne     L784E
        move.l  a5,-(a7)
        movea.l a2,a5
        moveq   #0,d5
        tst.b   d7
        beq.s   L76D0
        moveq   #sd.chenq,d0
        movea.l (a6),a1                 ;bv_bfbas
        bsr     L796E
        cmpi.w  #err.bp,d0
        bne.s   L76D0
        moveq   #1,d7
L76D0   cmpa.l  (a7),a3
        bge     L783E
        move.b  1(a6,a3.l),d0
        move.b  d0,d5
        lsr.b   #4,d5
        andi.b  #$0F,d0
        bne.s   L76F2
        tst.b   0(a6,a3.l)
        bne.s   L76F2
        bsr     L795A
        bra     L7832

L76F2   tst.w   2(a6,a3.l)
        sge     d1
        and.b   d7,d1
        beq.s   L7714
        move.w  d0,-(a7)
        bsr     L795A
        move.w  (a7)+,d0
        movea.l (a7),a4
        jsr     L7A0C(pc)
        bne     L7850
        move.l  a4,(a7)
        bra     L7832

L7714   tst.b   d7
        bgt     L7838
        movea.l bv_vvbas(a6),a0
        move.l  4(a6,a3.l),d1
        blt     L7828
        adda.l  d1,a0
        cmpi.b  #3,0(a6,a3.l)
        bne     L7824
        tst.b   d5
        beq.s   L7740
        cmpi.b  #5,d5
        bne.s   L7742
        swap    d5
        bra.s   L7742

L7740   moveq   #3,d5
L7742   move.b  d0,-(a7)
        movea.l a0,a2
        bsr     L781A
        suba.l  bv_vvbas(a6),a2
        suba.l  bv_chbas(a6),a5
        movem.l a2-a3/a5,-(a7)
        jsr     L4DF6(pc)
        movem.l (a7)+,a2-a3/a5
        adda.l  bv_chbas(a6),a5
        adda.l  bv_vvbas(a6),a2
        movea.l a0,a4
L7768   subq.w  #1,d1
        lea     0(a4,d1.w),a1
        sf      0(a6,a1.l)
        bne.s   L7768
        move.w  4(a6,a2.l),d1
        move.b  (a7),d0
        subq.b  #2,d0
        blt.s   L7788
        beq.s   L7784
        moveq   #2,d0
        bra.s   L778C

L7784   moveq   #6,d0
        bra.s   L778C

L7788   subq.w  #1,d1
        moveq   #1,d0
L778C   move.w  d0,0(a6,a4.l)
        lsl.w   #1,d1
        move.w  d1,2(a6,a4.l)
L7796   movea.l bv_vvbas(a6),a0
        adda.l  0(a6,a2.l),a0
        move.w  4(a6,a2.l),d0
        lsl.w   #2,d0
        lea     2(a2,d0.w),a1
        move.w  0(a6,a1.l),d3
        lsr.w   #1,d0
L77AE   lea     2(a4,d0.w),a1
        move.w  0(a6,a1.l),d1
        lsl.w   #1,d0
        lea     4(a2,d0.w),a1
        lsr.w   #1,d0
        mulu.w  0(a6,a1.l),d1
        mulu.w  0(a6,a4.l),d1
        adda.l  d1,a0
        subq.w  #2,d0
        bne.s   L77AE
        move.b  (a7),d0
        bsr     L785E
        bne.s   L77FE
        move.w  2(a6,a4.l),d0
        beq.s   L77FE
L77DA   lsl.w   #1,d0
        lea     2(a2,d0.w),a1
        move.w  0(a6,a1.l),d1
        lsr.w   #1,d0
        lea     2(a4,d0.w),a1
        cmp.w   0(a6,a1.l),d1
        beq.s   L77F6
        addq.w  #1,0(a6,a1.l)
        bra.s   L7796

L77F6   clr.w   0(a6,a1.l)
        subq.w  #2,d0
        bne.s   L77DA
L77FE   bsr.s   L781A
        movea.l a4,a0
        move.l  d0,-(a7)
        move.l  a3,-(a7)
        jsr     L4FE8(pc)
        movea.l (a7)+,a3
        move.l  (a7)+,d0
        addq.w  #2,a7
        bne.s   L784C
        tst.b   d5
        bne.s   L7838
        swap    d5
        bra.s   L7832

L781A   moveq   #2,d1
        add.w   4(a6,a2.l),d1
        lsl.w   #1,d1
        rts

L7824   bsr.s   L785E
        bra.s   L7836

L7828   bsr     L795A
        moveq   #'*',d1
        bsr     L7960
L7832   bsr     L78E0
L7836   bne.s   L784C
L7838   addq.w  #8,a3
        bra     L76D0

L783E   tst.b   d5
        bne.s   L784A
        tst.b   d7
        bgt.s   L784A
        bsr     L7934
L784A   moveq   #err.ok,d0
L784C   movea.l (a7)+,a5
L784E   rts

L7850   move.l  a4,(a7)
        move.l  d0,-(a7)
        moveq   #sd.curs,d0
        bsr     L7970
        move.l  (a7)+,d0
        bra.s   L784C

L785E   movea.l bv_rip(a6),a1
        subq.w  #6,a1
        tst.b   d0
        beq.s   L7878
        subq.b  #2,d0
        bgt.s   L787E
        beq.s   L788E
        move.w  0(a6,a0.l),d2
        movea.l a0,a1
        addq.w  #2,a1
        bra.s   L78AA

L7878   movea.l a0,a1
        move.w  d3,d2
        bra.s   L78AA

L787E   move.w  0(a6,a0.l),0(a6,a1.l)
        movea.l (a6),a0                 ;bv_bfbas
        addq.w  #1,a0
        jsr     L3E54(pc)               ;cn_itod
        bra.s   L78A2

L788E   move.l  2(a6,a0.l),2(a6,a1.l)
        move.w  0(a6,a0.l),0(a6,a1.l)
        movea.l (a6),a0                 ;bv_bfbas
        addq.w  #1,a0
        jsr     L3EF6(pc)               ;cn_ftod
L78A2   movea.l (a6),a1                 ;bv_bfbas
        addq.w  #1,a1
        move.l  a0,d2
        sub.l   a1,d2
L78AA   tst.b   d4
        beq.s   L78D2
        swap    d4
        addq.w  #1,d2
        subq.w  #1,a1
        move.b  0(a6,a1.l),-(a7)
        sub.w   d2,d4
        blt.s   L78C4
        move.b  #' ',0(a6,a1.l)
        bra.s   L78D0

L78C4   move.b  #10,0(a6,a1.l)
        move.w  #-1,32(a6,a5.l)
L78D0   swap    d4
L78D2   bsr     L7968
        tst.b   d4
        beq.s   L78E0
        suba.w  d2,a1
        move.b  (a7)+,0(a6,a1.l)
L78E0   tas     bv_brk(a6)
        bne.s   L78E8
        moveq   #err.nc,d0
L78E8   tst.l   d0
        bne.s   L7958
        tst.b   d7
        bgt.s   L7956
        sf      d4
        cmpi.b  #1,d5
        beq.s   L793E
        cmpi.b  #3,d5
        beq.s   L7934
        cmpi.b  #4,d5
        beq.s   L7926
        cmpi.b  #5,d5
        bne.s   L7956
        move.l  a5,-(a7)
        addq.w  #8,a3
        lea     8(a3),a5
        jsr     L61BE(pc)
        movea.l (a7)+,a5
        bne.s   L7958
        move.w  0(a6,a1.l),d2
        addq.l  #2,bv_rip(a6)
        bsr.s   L797A
        bra.s   L794A

L7926   bsr.s   L797A
        sub.w   d0,d1
        move.w  d1,d4
        swap    d4
        tst.w   d0
        sne     d4
        bra.s   L7956

L7934   moveq   #10,d1
        bsr.s   L7960
        clr.w   32(a6,a5.l)
        bra.s   L7956

L793E   bsr.s   L797A
        move.w  d0,d2
        addq.w  #8,d2
        andi.w  #$00F8,d2
        subq.w  #8,d1
L794A   sub.w   d2,d1
        blt.s   L7934
        sub.w   d0,d2
L7950   bsr.s   L795E
        subq.w  #1,d2
        bgt.s   L7950
L7956   moveq   #err.ok,d0
L7958   rts

L795A   tst.b   d4
        beq.s   L7958
L795E   moveq   #' ',d1
L7960   moveq   #io.sbyte,d0
        addq.w  #1,32(a6,a5.l)
        bra.s   L7970

L7968   moveq   #io.sstrg,d0
        add.w   d2,32(a6,a5.l)
L796E   trap    #4
L7970   moveq   #forever,d3
        movea.l 0(a6,a5.l),a0
        trap    #3
        rts

L797A   moveq   #sd.chenq,d0
        movea.l (a6),a1                 ;bv_bfbas
        bsr.s   L796E
        tst.l   d0
        bne.s   L798E
        move.w  4(a6,a1.l),d0
        move.w  0(a6,a1.l),d1
        rts

L798E   move.w  34(a6,a5.l),d1
        move.w  32(a6,a5.l),d0
        rts

;BASIC - Procedure 'RANDOMISE'

L7998   jsr     L61E2(pc)               ;ca_gtlin
        bne.s   L79BE
        subq.w  #1,d3
        bgt.s   L79C0
        beq.s   L79AA
        moveq   #mt.rclck,d0
        trap    #1
        bra.s   L79B4

L79AA   move.l  0(a6,a1.l),d1
        addq.l  #4,bv_rip(a6)
        moveq   #err.ok,d0
L79B4   move.l  d1,d2
        swap    d1
        add.l   d2,d1
        move.l  d1,bv_rand(a6)
L79BE   rts

L79C0   moveq   #err.bp,d0
        rts

L79C4   move.l  d0,-(a7)
        moveq   #0,d4
L79C8   moveq   #4,d0
        trap    #4
        movea.l bv_bfp(a6),a1
        move.l  bv_tkbas(a6),d2
        sub.l   (a6),d2                 ;bv_bfbas
        move.l  a1,d1
        sub.l   (a6),d1                 ;bv_bfbas
        move.w  d1,d4
        move.l  d4,d1
        tst.l   (a7)
        beq.s   L79E6
        moveq   #io.fline,d0
        sub.w   d1,d2
L79E6   moveq   #forever,d3
        trap    #3
        tst.l   d0
        bge.s   L7A06
        cmpi.b  #$FB,d0                 ;err.bo ?
        bne.s   L7A06
        move.l  d1,d4
        move.l  a1,bv_bfp(a6)
        move.l  a0,-(a7)
        moveq   #126,d1
        jsr     L4E6A(pc)               ;reserve space in buffer
        movea.l (a7)+,a0
        bra.s   L79C8

L7A06   addq.w  #4,a7
L7A08   tst.l   d0
        rts

L7A0C   move.l  d7,-(a7)
        move.w  d0,-(a7)
        bsr.s   L7A8E
        beq.s   L7A1A
L7A14   addq.w  #2,a7
L7A16   move.l  (a7)+,d7
        bra.s   L7A08

L7A1A   movea.l 0(a6,a5.l),a0
        move.l  (a6),bv_bfp(a6)         ;bv_bfbas
        tst.b   d7
        sgt     d0
        suba.l  bv_tkbas(a6),a3
        suba.l  bv_tkbas(a6),a4
        suba.l  bv_tkbas(a6),a5
        movem.l a3-a5,-(a7)
        bsr.s   L79C4
        movem.l (a7)+,a3-a5
        adda.l  bv_tkbas(a6),a3
        adda.l  bv_tkbas(a6),a4
        adda.l  bv_tkbas(a6),a5
        bne.s   L7A14
        move.l  a3,-(a7)
        moveq   #sd.pcol,d0
        trap    #3
        moveq   #sd.ncol,d0
        trap    #3
        movea.l (a7)+,a3
        movea.l (a6),a0                 ;bv_bfbas
        move.l  a1,d7
        movea.l bv_rip(a6),a1
        move.w  (a7)+,d0
        subq.b  #2,d0
        blt.s   L7A74
        beq.s   L7A6C
        jsr     L3DC2(pc)               ;cn_dtoi
        bra.s   L7A70

L7A6C   jsr     L3D16(pc)               ;cn_dtof
L7A70   bne.s   L7A16
        bra.s   L7A7C

L7A74   movea.l d7,a0
        subq.w  #1,a0
        jsr     L5A20(pc)
L7A7C   move.l  a1,bv_rip(a6)
        suba.l  bv_chbas(a6),a5
        jsr     L72C2(pc)               ;bp_let
        adda.l  bv_chbas(a6),a5
        bra.s   L7A16

L7A8E   move.b  0(a6,a3.l),d0
        subq.b  #2,d0
        ble.s   L7AC0
        subq.b  #1,d0
        bne.s   L7AB8
        move.b  1(a6,a3.l),d0
        andi.b  #$0F,d0
        subq.b  #1,d0
        bgt.s   L7AC8
        movea.l 4(a6,a3.l),a2
        adda.l  bv_vvbas(a6),a2
        cmpi.w  #1,4(a6,a2.l)
        bgt.s   L7AC8
        bra.s   L7AC0

L7AB8   subq.b  #3,d0
        blt.s   L7AC4
        subq.b  #1,d0
        bgt.s   L7AC4
L7AC0   moveq   #err.ok,d0
        rts

L7AC4   moveq   #err.bn,d0
        rts

L7AC8   moveq   #err.ni,d0
        rts

;BASIC - Procedure 'RECOL'

L7ACC   jsr     L8038(pc)
        bne.s   L7AF4
        cmpi.w  #8,d3
        bne.s   L7AF2
        movea.l a1,a2
        moveq   #io.sstrg,d0
L7ADC   move.b  1(a6,a2.l),1(a6,a1.l)
        subq.w  #2,a2
        subq.w  #1,a1
        dbf     d0,L7ADC
        addq.w  #2,a1
        moveq   #sd.recol,d4
        jmp     L7FC4(pc)

L7AF2   moveq   #err.bp,d0
L7AF4   rts

;BASIC - Procedure 'EDIT'

L7AF6   moveq   #0,d4
        bra.s   L7AFC

;BASIC - Procedure 'AUTO'

L7AFA   moveq   #10,d4
L7AFC   st      bv_auto(a6)
        tst.b   bv_unrvl(a6)
        bne.s   L7B32
        bra.s   L7B26

L7B08   moveq   #0,d5
        cmpa.l  4(a7),a3
        beq.s   L7B20
        jsr     L7452(pc)
        beq.s   L7B20
        jsr     L74F0(pc)
        beq.s   L7B22
        addq.w  #4,a7
        bra.s   L7B84

L7B20   moveq   #1,d0
L7B22   rts

;BASIC - Procedure 'RENUM'

L7B24   moveq   #10,d4
L7B26   jsr     L7E30(pc)
        beq.s   L7B32
        st      bv_undo(a6)
        rts

L7B32   move.l  a5,-(a7)
        move.w  #$7FFF,d7
        moveq   #100,d6
        swap    d4
        bsr.s   L7B08
        cmpi.b  #5,d5
        bne.s   L7B5A
        tst.b   d0
        bne.s   L7B4A
        move.w  d1,d4
L7B4A   bsr.s   L7B08
        cmpi.b  #2,d5
        bne.s   L7B84
        tst.b   d0
        bne.s   L7B66
        move.w  d1,d7
        bra.s   L7B66

L7B5A   cmpi.b  #2,d5
        bne.s   L7B68
        tst.b   d0
        bne.s   L7B66
        move.w  d1,d4
L7B66   bsr.s   L7B08
L7B68   tst.b   d0
        bne.s   L7B6E
        move.w  d1,d6
L7B6E   swap    d4
        cmpi.b  #1,d5
        bne.s   L7B7C
        bsr.s   L7B08
        bne.s   L7B7C
        move.w  d1,d4
L7B7C   move.w  d4,d3
        swap    d4
        tst.b   d5
        beq.s   L7B94
L7B84   sf      bv_auto(a6)
        addq.w  #4,a7
        moveq   #err.bp,d0
        rts

L7B8E   addq.w  #2,a7
        moveq   #err.or,d0
        rts

L7B94   move.w  d7,d5
        addq.w  #4,a7
        tst.b   bv_auto(a6)
        beq.s   L7BAA
        move.w  d6,bv_edlin(a6)
        move.w  d3,bv_edinc(a6)
L7BA6   moveq   #err.ok,d0
        rts

L7BAA   movea.l bv_pfbas(a6),a4
        cmpa.l  bv_pfp(a6),a4
        bge.s   L7BA6
        clr.w   -(a7)
        clr.l   bv_linum(a6)
        tst.w   d4
        beq.s   L7BD8
        jsr     L9FBE(pc)
        cmpa.l  (a6),a4                 ;bv_bfbas
        beq.s   L7BD8
        suba.w  bv_length(a6),a4
        move.w  2(a6,a4.l),d2
        cmp.w   d2,d6
        ble.s   L7B8E
        adda.w  bv_length(a6),a4
        move.w  d2,(a7)
L7BD8   moveq   #0,d0
L7BDA   addq.w  #1,d0
        bsr.s   L7C4E
        beq.s   L7BDA
        bgt.s   L7BE6
        move.w  #$7FFF,d2
L7BE6   move.l  d0,d1
        subq.w  #1,d1
        mulu.w  d3,d1
        add.l   d6,d1
        ext.l   d2
        cmp.l   d2,d1
        bge.s   L7B8E
        move.w  d2,-(a7)
        moveq   #2,d1
        add.l   d0,d1
        lsl.l   #2,d1
        jsr     L4DF6(pc)
        movea.l a0,a3
        suba.l  bv_vvbas(a6),a3
        move.l  d1,0(a6,a0.l)
        suba.w  bv_length(a6),a4
        move.w  2(a6,a4.l),bv_linum(a6)
        adda.w  bv_length(a6),a4
        jsr     L9FBE(pc)
        bsr     L7D56
        move.w  d6,d0
        move.w  4(a6,a4.l),d2
        move.w  2(a7),d6
        swap    d4
L7C2C   move.w  d2,0(a6,a0.l)
        move.w  d0,2(a6,a0.l)
        move.w  d0,4(a6,a4.l)
        addq.w  #4,a0
        add.w   d4,d0
        bsr.s   L7C4E
        beq.s   L7C2C
        move.w  (a7),d5
        move.w  d5,0(a6,a0.l)
        move.w  d5,2(a6,a0.l)
        addq.w  #4,a7
        bra.s   L7C7A

L7C4E   move.w  0(a6,a4.l),d1
        addq.w  #2,a4
        add.w   d1,bv_length(a6)
        adda.w  bv_length(a6),a4
        cmpa.l  bv_pfp(a6),a4
        bge.s   L7C72
        move.w  4(a6,a4.l),d2
        cmp.w   d2,d5
        blt.s   L7C6E
        moveq   #0,d1
        rts

L7C6E   moveq   #1,d1
        rts

L7C72   moveq   #-$01,d1
        rts

L7C76   jmp     LA56C(pc)

L7C7A   st      bv_edit(a6)
        jsr     L958E(pc)
        bra.s   L7C8A

L7C84   jsr     LA60E(pc)
        bne.s   L7D06
L7C8A   bsr.s   L7C76
        cmpi.w  #$8111,d1               ;token keyword 'RESTORE' ?
        beq.s   L7CB2
        cmpi.w  #$810A,d1               ;token keyword 'GO' ?
        beq.s   L7CAE
        cmpi.w  #$8115,d1               ;token keyword 'ON' ?
        bne.s   L7C84
L7C9E   jsr     L9072(pc)
        jsr     L9686(pc)
        beq.s   L7C84
        cmpi.w  #$810A,d1               ;token keyword 'GO' ?
        bne.s   L7C9E
L7CAE   addq.w  #2,a4
        bsr.s   L7C76
L7CB2   addq.w  #2,a4
        bsr.s   L7C76
        cmpi.w  #$840A,d1               ;token symbol LF ?
        beq.s   L7C84
        subi.w  #$F000,d1
        bge.s   L7CCE
L7CC2   move.w  #$8404,d4               ;token symbol ','
        jsr     LA5E0(pc)
        bne.s   L7C84
        bra.s   L7CB2

L7CCE   movea.l bv_rip(a6),a1
        subq.w  #6,a1
        move.w  d1,0(a6,a1.l)
        move.l  2(a6,a4.l),2(a6,a1.l)
        jsr     L4796(pc)               ;ri.nint
        move.w  0(a6,a1.l),d1
        bsr.s   L7D60
        ble.s   L7CC2
        move.w  d1,0(a6,a1.l)
        jsr     L47B8(pc)               ;ri.float
        move.w  0(a6,a1.l),d0
        addi.w  #$F000,d0
        move.w  d0,0(a6,a4.l)
        move.l  2(a6,a1.l),2(a6,a4.l)
        bra.s   L7CC2

L7D06   move.w  bv_lsbef(a6),d1
        bsr.s   L7D60
        move.w  d1,bv_lsbef(a6)
        move.w  bv_lsbas(a6),d1
        bsr.s   L7D60
        move.w  d1,bv_lsbas(a6)
        move.w  bv_lsaft(a6),d1
        bsr.s   L7D60
        move.w  d1,bv_lsaft(a6)
        movea.l bv_lnbas(a6),a1
        move.w  0(a6,a1.l),d1
        bsr.s   L7D60
        move.w  d1,0(a6,a1.l)
        st      bv_cont(a6)
        bsr.s   L7D56
        subq.w  #4,a0
        move.l  0(a6,a0.l),d1
        jsr     L4FE8(pc)
        moveq   #0,d2
        moveq   #0,d5
        suba.l  a0,a0
        jsr     L90A2(pc)
        movea.l d0,a0
        jsr     L8FE6(pc)
        moveq   #err.ok,d0
        rts

L7D56   movea.l bv_vvbas(a6),a0
        adda.l  a3,a0
        addq.w  #4,a0
        rts

L7D60   cmp.w   d6,d1
        ble.s   L7D76
        cmp.w   d1,d5
        blt.s   L7D76
        bsr.s   L7D56
L7D6A   cmp.w   0(a6,a0.l),d1
        addq.w  #4,a0
        bgt.s   L7D6A
        move.w  -2(a6,a0.l),d1
L7D76   rts

;BASIC - Procedure 'REPORT'

L7D78   moveq   #0,d1
        jsr     L65E8(pc)
        move.l  bv__C2(a6),d0
        move.w  bv_linum(a6),-(a7)
        move.w  bv__C6(a6),bv_linum(a6)
        jsr     L9BDC(pc)
        move.w  (a7)+,bv_linum(a6)
        moveq   #err.ok,d0
        rts

;BASIC - Procedure 'CLEAR'

L7D98   moveq   #0,d6
        moveq   #err.ok,d0
        move.w  #-1,bv_nxlin(a6)
        bra.s   L7DE0

;BASIC - Procedure 'RUN'

L7DA4   jsr     L61DA(pc)               ;ca_gtint
        bne.s   L7E1E
        moveq   #6,d6
        move.w  d7,bv_nxlin(a6)
        subq.w  #1,d3
        blt.s   L7E14
        beq.s   L7DBA
        moveq   #err.bp,d0
        rts

L7DBA   move.w  0(a6,a1.l),bv_nxlin(a6)
        move.b  d7,bv_nxstm(a6)
        addq.l  #2,bv_rip(a6)
        bra.s   L7E14

;BASIC - Procedure 'MERGE'

L7DCA   bsr.s   L7E30
        moveq   #14,d6
        tst.b   bv_sing(a6)
        bne.s   L7DF6

;BASIC - Procedure 'MRUN'

L7DD4   bsr.s   L7E30
        moveq   #12,d6
        bsr.s   L7E20
        bne.s   L7E0E
        move.w  d7,bv_nxlin(a6)
L7DE0   tst.b   bv_sing(a6)
        bne.s   L7E14
        move.w  bv_linum(a6),bv_nxlin(a6)
        move.b  bv_stmnt(a6),bv_nxstm(a6)
        bra.s   L7E14

;BASIC - Procedure 'LOAD'

L7DF4   moveq   #10,d6
L7DF6   bsr.s   L7E20
        bra.s   L7E0E

;BASIC - Procedure 'LRUN'

L7DFA   moveq   #8,d6
        bsr.s   L7E20
        bne.s   L7E0E
        move.w  d7,bv_nxlin(a6)
        bra.s   L7E14

;BASIC - Procedure 'NEW'

L7E06   moveq   #2,d6
        bra.s   L7E0C

;BASIC - Procedure 'STOP'

L7E0A   moveq   #4,d6
L7E0C   moveq   #err.ok,d0
L7E0E   move.w  #-1,bv_nxlin(a6)
L7E14   sf      bv_cont(a6)
        move.w  d6,bv_stopn(a6)
        tst.l   d0
L7E1E   rts

L7E20   moveq   #io.share,d4
        jsr     L69C4(pc)
        bne.s   L7E1E
        move.l  a0,bv_comch(a6)
        moveq   #err.ok,d0
        rts

L7E30   tst.b   bv_unrvl(a6)
        bne.s   L7E44
        move.l  bv_rtp(a6),d0
        sub.l   bv_rtbas(a6),d0
        beq.s   L7E44
        addq.w  #4,a7
        moveq   #err.ni,d0
L7E44   rts

;BASIC - Procedure 'RETRY'

L7E46   subq.b  #1,bv_cnstm(a6)
        bge.s   L7E50
        sf      bv_cnstm(a6)

;BASIC - Procedure 'CONTINUE'

L7E50   sf      bv__C0(a6)
        moveq   #16,d6
        bra.s   L7E0C

;BASIC - Procedure 'TRA'

L7E58   jsr     L61E2(pc)               ;ca_gtlin
        bne.s   L7E8A
        clr.l   d2
        subq.w  #1,d3
        beq.s   L7E78
        subq.w  #1,d3
        bne.s   L7E8C
        addq.l  #4,bv_rip(a6)
        move.l  4(a6,a1.l),d2
        bne.s   L7E78
        move.l  MBFE6,d2                ;message table
L7E78   addq.l  #4,bv_rip(a6)
        move.l  0(a6,a1.l),d1
        moveq   #mt.cntry,d0
        trap    #1
        tst.l   d0
        bmi.s   L7E8C
        moveq   #err.ok,d0
L7E8A   rts

L7E8C   moveq   #err.bp,d0
        rts

;BASIC - Procedure 'TURNTO'

L7E90   bsr     L7F24
        bne.s   L7ED0
        bra.s   L7EA6

;BASIC - Procedure 'TURN'

L7E98   bsr     L7F24
        bne.s   L7ED0
        bsr     L7F42
        jsr     L4838(pc)               ;ri.add
L7EA6   lea     L7F52(pc),a3
        lea     6(a1),a4
        subq.w  #6,a1
        move.w  #$0809,0(a6,a1.l)
        move.l  #$5A000000,2(a6,a1.l)   ;fp[360]
        jsr     L41B4(pc)               ;ri_execb
        move.w  0(a6,a1.l),16(a6,a2.l)
        move.l  2(a6,a1.l),18(a6,a2.l)
L7ECE   moveq   #err.ok,d0
L7ED0   rts

;BASIC - Procedure 'PENUP'

L7ED2   moveq   #0,d4
        bra.s   L7ED8

;BASIC - Procedure 'PENDOWN'

L7ED6   moveq   #1,d4
L7ED8   jsr     L65E6(pc)
        bne.s   L7ED0
        move.b  d4,22(a6,a2.l)
        bra.s   L7ECE

;BASIC - Procedure 'MOVE'

L7EE4   bsr.s   L7F24
        bne.s   L7F22
        bsr.s   L7F42
        subq.w  #6,a1
        move.w  #$07FB,0(a6,a1.l)
        move.l  #$477D1A89,2(a6,a1.l)   ;fp[1.745329E-2]
        jsr     L48DE(pc)               ;ri.mult
        jsr     L6D6E(pc)
        jsr     L6D7E(pc)
        lea     L7F5A(pc),a3
        lea     24(a1),a4
        jsr     L41B4(pc)               ;ri_execb
        jsr     L6D8E(pc)
        tst.b   22(a6,a2.l)
        beq.s   L7ECE
        moveq   #sd.line,d0
        trap    #4
        trap    #3
L7F22   rts

L7F24   move.w  #256,d1
        jsr     L4E4E(pc)               ;bv_chrix
        jsr     L65E6(pc)
        bne.s   L7F40
        move.l  bv_rip(a6),-(a7)
        jsr     L61C6(pc)
        move.l  (a7)+,bv_rip(a6)
        tst.l   d0
L7F40   rts

L7F42   subq.w  #6,a1
        move.w  16(a6,a2.l),0(a6,a1.l)
        move.l  18(a6,a2.l),2(a6,a1.l)
        rts

;RI operation list for TURN

L7F52   dc.b    ri.load+ri.var00
        dc.b    ri.load+ri.var01
        dc.b    ri.div,ri.int,ri.float,ri.mult,ri.sub,ri.term

;RI operation list for MOVE

L7F5A   dc.b    ri.load+ri.var00
        dc.b    ri.load+ri.var01
        dc.b    ri.cos,ri.mult
        dc.b    ri.load+ri.var02
        dc.b    ri.add
        dc.b    ri.load+ri.var00
        dc.b    ri.load+ri.var01
        dc.b    ri.sin,ri.mult
        dc.b    ri.load+ri.var03
        dc.b    ri.add,ri.term,0

;BASIC - Procedure 'WIDTH'

L7F68   jsr     L8038(pc)
        bne.s   L7F78
        subq.w  #1,d3
        bne.s   L7F7A
        move.w  0(a6,a1.l),ch.width(a6,a2.l)
L7F78   rts

L7F7A   moveq   #err.bp,d0
        rts

;BASIC - Procedure 'WINDOW'

L7F7E   jsr     L8038(pc)
        bne.s   L7FA2
        moveq   #sd.wdef,d4
        moveq   #$80,d1                 ;transparent border
        clr.w   d2                      ;border colour
        bra.s   L7F9C

;BASIC - Procedure 'BLOCK'

L7F8C   jsr     L8038(pc)
        bne.s   L7FA2
        moveq   #sd.fill,d4
        subq.w  #4,d3
        jsr     L7FD4(pc)
        bne.s   L7FA2
L7F9C   subq.w  #6,a1
        jmp     L7FC4(pc)

L7FA2   rts

;BASIC - Procedure 'BORDER'

L7FA4   jsr     L8038(pc)
        bne.s   L7FA2
        moveq   #sd.bordr,d4
        moveq   #$80,d1                 ;transparent border
        cmpi.w  #1,d3
        bls.s   L7FBC
        subq.w  #1,d3
        jsr     L7FD4(pc)
        bne.s   L7FA2
L7FBC   move.w  0(a6,a1.l),d2
        jmp     L7FC4(pc)

L7FC4   move.l  d4,d0
        moveq   #forever,d3
        move.l  a1,-(a7)
        trap    #4
        trap    #3
        movea.l (a7)+,a1
        tst.l   d0
        rts

L7FD4   move.w  0(a6,a1.l),d1
        subq.l  #2,a1
        subq.w  #1,d3
        beq.s   L801A
        cmpi.w  #7,d1
        bhi.s   L8024
        ori.w   #$0018,d1
        cmpi.w  #1,d3
        beq.s   L8004
        andi.w  #$0007,d1
        move.w  0(a6,a1.l),d2
        subq.l  #2,a1
        subq.w  #1,d3
        lsl.w   #3,d1
        cmpi.w  #7,d2
        bhi.s   L8024
        or.w    d2,d1
L8004   move.w  0(a6,a1.l),d2
        subq.l  #2,a1
        subq.w  #1,d3
        bne.s   L8024
        cmpi.w  #7,d2
        bhi.s   L8024
        eor.w   d2,d1
        lsl.w   #3,d1
        or.w    d2,d1
L801A   cmpi.w  #$00FF,d1
        bhi.s   L8024
L8020   moveq   #err.ok,d0
        rts

L8024   moveq   #err.bp,d0
        rts

L8028   cmpi.w  #1,d3
        bne.s   L8024
        move.w  0(a6,a1.l),d1
        cmp.w   d3,d1
        bhi.s   L8024
        bra.s   L8020

L8038   jsr     L65E6(pc)
        bne.s   L8052
L803E   jsr     L61DA(pc)               ;ca_gtint
        bne.s   L8052
        add.l   d3,d3
        add.l   d3,bv_rip(a6)
        adda.l  d3,a1
        subq.l  #2,a1
        lsr.w   #1,d3
        moveq   #err.ok,d0
L8052   rts

;BASIC - Function  'ACOS'

L8054   lea     L42E4(pc),a4            ;ri.acos
        bra.s   L80C6

;BASIC - Function  'ACOT'

L805A   lea     L431E(pc),a4            ;ri.acot
        bra.s   L80C6

;BASIC - Function  'ASIN'

L8060   lea     L42F2(pc),a4            ;ri.asin
        bra.s   L80C6

;BASIC - Function  'ATAN'

L8066   lea     L4326(pc),a4            ;ri.atan
        bra.s   L80C6

;BASIC - Function  'COS'

L806C   lea     L423E(pc),a4            ;ri.cos
        bra.s   L80C6

;BASIC - Function  'COT'

L8072   lea     L426A(pc),a4            ;ri.cot
        bra.s   L80C6

;BASIC - Function  'EXP'

L8078   lea     L44DE(pc),a4            ;ri.exp
        bra.s   L80C6

;BASIC - Function  'LN'

L807E   lea     L4446(pc),a4            ;ri.ln
        bra.s   L80C6

;BASIC - Function  'LOG10'

L8084   lea     L442C(pc),a4            ;ri.log10
        bra.s   L80C6

;BASIC - Function  'SIN'

L808A   lea     L4236(pc),a4            ;ri.sin
        bra.s   L80C6

;BASIC - Function  'SQRT'

L8090   lea     L452C(pc),a4            ;ri.sqrt
        bra.s   L80C6

;BASIC - Function  'TAN'

L8096   lea     L4262(pc),a4            ;ri.tan
        bra.s   L80C6

;BASIC - Function  'DEG'

L809C   lea     L497E(pc),a4            ;ri.div
        bra.s   L80A6

;BASIC - Function  'RAD'

L80A2   lea     L48DE(pc),a4            ;ri.mult
L80A6   jsr     L61C6(pc)
        bne.s   L80DE
        bsr     L8162
        subq.w  #6,a1
        move.w  #$07FB,0(a6,a1.l)
        move.l  #$477D1A89,2(a6,a1.l)   ;fp[1.745329E-2]
        bra.s   L80D6

;BASIC - Function  'ABS'

L80C2   lea     L4A06(pc),a4            ;ri.abs
L80C6   jsr     L61C6(pc)
        bne.s   L80DE
        moveq   #48,d1
        jsr     L4E4E(pc)               ;bv_chrix
        movea.l bv_rip(a6),a1
L80D6   jsr     (a4)
L80D8   moveq   #2,d4
L80DA   move.l  a1,bv_rip(a6)
L80DE   rts

L80E0   moveq   #3,d4
        moveq   #err.ok,d0
        bra.s   L80DA

        addq.w  #4,a7
        rts

;BASIC - Function  'RND'

L80EA   moveq   #1,d5
        or.l    bv_rand(a6),d5
        move.l  d5,d6
        mulu.w  #355,d5
        swap    d6
        mulu.w  #355,d6
        swap    d6
        clr.w   d6
        add.l   d6,d5
        move.l  d5,bv_rand(a6)
        jsr     L61DA(pc)               ;ca_gtint
        bne.s   L80DE
        subq.w  #1,d3
        beq.s   L812A
        bgt.s   L8124
        bsr     L8162
        move.l  d5,d1
        lsr.l   #1,d1
        move.w  #$0800,d0
L811E   jsr     L4830(pc)
        bra.s   L80D8

L8124   move.w  0(a6,a1.l),d3
        addq.w  #2,a1
L812A   move.w  0(a6,a1.l),d2
        sub.w   d3,d2
        blt.s   L819E
        addq.w  #1,d2
        swap    d5
        mulu.w  d2,d5
        swap    d5
        add.w   d3,d5
L813C   move.w  d5,0(a6,a1.l)
        bra.s   L80E0

;BASIC - Function  'PI'

L8142   cmpa.l  a3,a5
        bne.s   L819E
        bsr.s   L8162
        move.w  #$0802,d0
        move.l  #$6487ED51,d1           ;PI fp[3.141593]
        bra.s   L811E

;BASIC - Function  'INT'

L8154   jsr     L61C2(pc)
        bne.s   L80DE
        move.l  0(a6,a1.l),d1
        addq.w  #4,a1
        bra.s   L8186

L8162   jsr     L4E4C(pc)
        movea.l bv_rip(a6),a1
        rts

;BASIC - Function  'PEEK'

L816C   bsr.s   L818C
        moveq   #0,d5
        move.b  (a4),d5
        bra.s   L817A

;BASIC - Function  'PEEK_W'

L8174   bsr.s   L818C
        bcs.s   L819E
        move.w  (a4),d5
L817A   addq.w  #2,a1
        bra.s   L813C

;BASIC - Function  'PEEK_L'

L817E   bsr.s   L818C
        bcs.s   L819E
        move.l  (a4),d1
L8184   addq.w  #4,a1
L8186   move.w  #$081F,d0
        bra.s   L811E

L818C   jsr     L61C2(pc)
        bne     L821A
        movea.l 0(a6,a1.l),a4
        move.l  a4,d1
        ror.w   #1,d1
        rts

L819E   moveq   #err.bp,d0
L81A0   rts

;BASIC - Function  'RESPR'

L81A2   bsr.s   L818C
        moveq   #mt.alres,d0
        move.l  0(a6,a1.l),d1
        movea.l a1,a4
        trap    #1
        move.l  a0,d1
        movea.l a4,a1
        tst.l   d0
        beq.s   L8184
        bra.s   L81A0

;BASIC - Function  'BEEPING'

L81B8   cmpa.l  a3,a5
        bne.s   L819E
        bsr.s   L8162
        moveq   #1,d1
        and.b   sv_sound+sv_base,d1
        bra.s   L8186

;BASIC - Function  'EOF'

L81C8   cmpa.l  a3,a5
        bne.s   L81D6
        jsr     L66FA(pc)
        subq.b  #1,bv_daitm(a6)
        bra.s   L81E0

L81D6   bsr     L825A
        moveq   #io.pend,d0
        moveq   #0,d3
        trap    #3
L81E0   moveq   #-10,d4
        sub.l   d0,d4
        bsr     L8162
        moveq   #0,d1
        tst.l   d4
        bne.s   L8186
        moveq   #1,d1
        bra.s   L8186

;BASIC - Function  'VER$'

L81F2   cmpa.l  a3,a5
        bne.s   L819E
        bsr     L8162
        subq.w  #6,a1
        move.l  MBFFA,0(a6,a1.l)
        move.w  MBFFA+4,4(a6,a1.l)
        bra.s   L8252

;BASIC - Function  'INKEY$'

L820E   bsr.s   L825A
        move.l  a0,-(a7)
        bsr     L8162
        jsr     L61DA(pc)               ;ca_gtint
L821A   bne.s   L8262
        movea.l (a7)+,a0
        cmpi.w  #1,d3
        bgt     L819E
        blt.s   L822E
        move.w  0(a6,a1.l),d3
        addq.w  #2,a1
L822E   movea.l a1,a4
        moveq   #io.fbyte,d0
        trap    #3
        movea.l a4,a1
        addq.l  #1,d0
        beq.s   L824C
        subq.l  #1,d0
        bne.s   L8264
        subq.w  #4,a1
L8240   move.b  d1,2(a6,a1.l)
        move.w  #1,0(a6,a1.l)
        bra.s   L8252

L824C   subq.w  #2,a1
        clr.w   0(a6,a1.l)
L8252   moveq   #1,d4
        moveq   #err.ok,d0
        bra     L80DA

L825A   moveq   #0,d1
        jsr     L65E8(pc)
        beq.s   L8264
L8262   addq.w  #4,a7
L8264   rts

;BASIC - Function  'CHR$'

L8266   bsr     L8162
        jsr     L61BE(pc)
        bne.s   L8264
        move.w  0(a6,a1.l),d1
        subq.w  #2,a1
        bra.s   L8240

;BASIC - Function  'FILL$'

L8278   subq.w  #8,a5
        bsr.s   L82CE
        beq     L819E
        subq.l  #1,d1
        bgt.s   L828A
        move.b  2(a6,a1.l),3(a6,a1.l)
L828A   move.w  2(a6,a1.l),d5
        addq.l  #4,d1
        bclr    #0,d1
        adda.l  d1,a1
        move.l  a1,bv_rip(a6)
        movea.l a5,a3
        addq.w  #8,a5
        jsr     L61BE(pc)
        bne.s   L8264
        addq.l  #2,bv_rip(a6)
        moveq   #0,d4
        move.w  0(a6,a1.l),d4
        blt     L819E
        beq.s   L8252
        move.l  d4,d1
        bsr.s   L82F2
L82B8   subq.w  #2,a1
        move.w  d5,0(a6,a1.l)
        subq.l  #2,d1
        bgt.s   L82B8
        move.w  d4,0(a6,a1.l)
        bra.s   L8252

;BASIC - Function  'LEN'

L82C8   bsr.s   L82CE
        move.w  d1,d5
        bra.s   L82E6

L82CE   jsr     L61CA(pc)
        bne.s   L8262
        moveq   #0,d5
        moveq   #0,d1
        move.w  0(a6,a1.l),d1
        rts

;BASIC - Function  'CODE'

L82DE   bsr.s   L82CE
        beq.s   L82EE
        move.b  2(a6,a1.l),d5
L82E6   addq.l  #1,d1
        bclr    #0,d1
        adda.l  d1,a1
L82EE   bra     L813C

L82F2   addq.l  #3,d1
        bclr    #0,d1
        move.l  d1,-(a7)
        jsr     L4E4E(pc)               ;bv_chrix
        movea.l bv_rip(a6),a1
        move.l  (a7)+,d1
L8304   rts

;BASIC - Function  'DIMN'

L8306   move.b  0(a6,a3.l),d1
        subq.b  #3,d1
        bne.s   L834E
        move.l  a3,-(a7)
        addq.w  #8,a3
        jsr     L61DA(pc)               ;ca_gtint
        movea.l (a7)+,a3
        bne.s   L8304
        subq.w  #1,d3
        bgt.s   L8304
        beq.s   L8328
        bsr.s   L836A
        subq.w  #2,a1
        moveq   #1,d1
        bra.s   L832E

L8328   move.w  0(a6,a1.l),d1
        ble.s   L8352
L832E   movea.l 4(a6,a3.l),a2
        adda.l  bv_vvbas(a6),a2
        move.w  4(a6,a2.l),d2
        sub.w   d1,d2
        blt.s   L8352
        addq.w  #2,a2
        lsl.w   #2,d1
        adda.w  d1,a2
        move.w  0(a6,a2.l),0(a6,a1.l)
        bra     L80E0

L834E   bsr.s   L836A
        subq.w  #2,a1
L8352   moveq   #0,d5
        bra.s   L82EE

;BASIC - Function  'DATE'

L8356   cmpa.l  a3,a5
        bne     L819E
        bsr.s   L836A
        moveq   #mt.rclck,d0
        trap    #1
        bclr    #31,d1
        bra     L8186

L836A   bra     L8162

;BASIC - Function  'KEYROW'

L836E   jsr     L61BE(pc)
        subq.w  #8,a7
        bne.s   L83B0
        movea.l a7,a3
        move.b  #kbdr_cmd,(a3)          ;IPC command keyboard direct read
        move.b  #1,1(a3)
        move.l  #0,2(a3)
        move.b  1(a6,a1.l),6(a3)
        move.b  #2,7(a3)
        move.l  a1,-(a7)
        moveq   #mt.ipcom,d0
        trap    #1
        movea.l (a7)+,a1
        move.w  d1,0(a6,a1.l)
        moveq   #3,d4
        movea.l sv_keyq+sv_base,a2
        move.l  q_nxtout(a2),q_nextin(a2)
L83B0   addq.w  #8,a7
        rts

;BASIC - Function  'DATE$'

L83B4   lea     L405E(pc),a4            ;cn_date
        bra.s   L83BE

;BASIC - Function  'DAY$'

L83BA   lea     L40BE(pc),a4            ;cn_day
L83BE   jsr     L4E4C(pc)
        cmpa.l  a3,a5
        ble.s   L83D4
        jsr     L61C2(pc)
        bne.s   L83E6
        move.l  0(a6,a1.l),d1
        addq.l  #4,a1
        bra.s   L83DC

L83D4   moveq   #mt.rclck,d0
        trap    #1
        movea.l bv_rip(a6),a1
L83DC   jsr     (a4)
        moveq   #1,d4
        move.l  a1,bv_rip(a6)
        moveq   #err.ok,d0
L83E6   rts

;BASIC - Function  'ERR_BL'

L83E8   bsr.s   L8414

;BASIC - Function  'ERR_RO'

L83EA   bsr.s   L8414

;BASIC - Function  'ERR_NI'

L83EC   bsr.s   L8414

;BASIC - Function  'ERR_OV'

L83EE   bsr.s   L8414

;BASIC - Function  'ERR_XP

L83F0   bsr.s   L8414

;BASIC - Function  'ERR_FE'

L83F2   bsr.s   L8414

;BASIC - Function  'ERR_BP'

L83F4   bsr.s   L8414

;BASIC - Function  'ERR_FF'

L83F6   bsr.s   L8414

;BASIC - Function  'ERR_TE'

L83F8   bsr.s   L8414

;BASIC - Function  'ERR_BN'

L83FA   bsr.s   L8414

;BASIC - Function  'ERR_DF'

L83FC   bra.s   L8414                   ;should be bsr ?

;BASIC - Function  'ERR_EF'

L83FE   bsr.s   L8414

;BASIC - Function  'ERR_IU'

L8400   bsr.s   L8414

;BASIC - Function  'ERR_EX'

L8402   bsr.s   L8414

;BASIC - Function  'ERR_NF'

L8404   bsr.s   L8414

;BASIC - Function  'ERR_NO'

L8406   bsr.s   L8414

;BASIC - Function  'ERR_BO'

L8408   bsr.s   L8414

;BASIC - Function  'ERR_OR'

L840A   bsr.s   L8414

;BASIC - Function  'ERR_OM'

L840C   bsr.s   L8414

;BASIC - Function  'ERR_NJ'

L840E   bsr.s   L8414

;BASIC - Function  'ERR_NC'

L8410   bsr.s   L8414
        nop

L8414   pea     L8414(pc)
        move.l  (a7)+,d4
        sub.l   (a7)+,d4
        lsr.l   #1,d4
        bsr.s   L8446
        subq.w  #6,a1
        clr.w   0(a6,a1.l)
        clr.l   2(a6,a1.l)
        add.l   bv__C2(a6),d4
        bne.s   L843C
        move.w  #$0801,0(a6,a1.l)
        move.b  #64,2(a6,a1.l)
L843C   moveq   #2,d4
L843E   move.l  a1,bv_rip(a6)
        moveq   #err.ok,d0
        rts

L8446   cmpa.l  a3,a5
        bne.s   L8454
        jsr     L4E4C(pc)
        movea.l bv_rip(a6),a1
        rts

L8454   addq.w  #4,a7
        moveq   #err.bp,d0
        rts

;BASIC - Function  'ERNUM'

L845A   bsr.s   L8446
        subq.w  #2,a1
        move.w  bv__C4(a6),0(a6,a1.l)
L8464   moveq   #3,d4
        bra.s   L843E

;BASIC - Function  'ERLIN'

L8468   bsr.s   L8446
        subq.w  #2,a1
        move.w  bv__C6(a6),0(a6,a1.l)
        bra.s   L8464

L8474   jsr     L4E4C(pc)
        movea.l bv_rip(a6),a1
        jsr     L3DC2(pc)               ;cn_dtoi
        bne.s   L84A4
        tst.w   0(a6,a1.l)
        ble.s   L84A4
        tst.b   bv_unrvl(a6)
        beq.s   L8496
L848E   adda.w  #12,a7
        moveq   #1,d0
        rts

;Token $9D - Line number

L8496   moveq   #4,d1
        moveq   #lno.b-256,d4
        move.w  0(a6,a1.l),d5
        jsr     L8E0A(pc)
        addq.l  #2,(a7)
L84A4   rts

;Token $86 - Mono operation symbol

L84A6   dc.b    4                       ;number of mono operation symbols

        dc.b    L84AB-L84A6             ;offsets to mono operation symbols
        dc.b    L84AD-L84A6
        dc.b    L84AF-L84A6
        dc.b    L84B2-L84A6

L84AB   dc.b    $01,'-'                 ;mono operation symbol list
L84AD   dc.b    $01,'+'
L84AF   dc.b    $02,'~~'
L84B2   dc.b    $13,'NOT'

L84B6   lea     L84A6(pc),a2
        jsr     L8748(pc)
        bra.s   L84C8                   ;fail
        moveq   #mon.b-256,d4           ;OK
        jsr     L8DFA(pc)
        addq.l  #2,(a7)
L84C8   rts

;Token $88 - Name

L84CA   moveq   #1,d3
        bra.s   L84D0
L84CE   moveq   #0,d3
L84D0   movea.l a0,a3
        jsr     L8706(pc)
        bra.s   L84F8                   ;fail
        move.l  a0,d5                   ;OK
        sub.l   a3,d5
        cmpi.w  #$00FF,d5
        bgt.s   L84F8
        move.l  a0,-(a7)
        jsr     L8622(pc)
        bra.s   L84F6                   ;fail
        movea.l (a7)+,a0                ;OK
        moveq   #nam.b-256,d4
        jsr     L8E0A(pc)
        addq.l  #2,(a7)
        rts

L84F6   movea.l (a7)+,a0
L84F8   rts

;Token $85 - Operation symbol

L84FA   dc.b    22                      ;number of operation symbols

        dc.b    L8511-L84FA             ;offsets to operation symbols
        dc.b    L8513-L84FA
        dc.b    L8515-L84FA
        dc.b    L8517-L84FA
        dc.b    L8519-L84FA
        dc.b    L851C-L84FA
        dc.b    L851E-L84FA
        dc.b    L8521-L84FA
        dc.b    L8523-L84FA
        dc.b    L8526-L84FA
        dc.b    L8529-L84FA
        dc.b    L852B-L84FA
        dc.b    L852E-L84FA
        dc.b    L8531-L84FA
        dc.b    L8534-L84FA
        dc.b    L8536-L84FA
        dc.b    L8538-L84FA
        dc.b    L853B-L84FA
        dc.b    L853F-L84FA
        dc.b    L8543-L84FA
        dc.b    L8547-L84FA
        dc.b    L854B-L84FA

L8511   dc.b    $01,'+'                 ;operation symbol list
L8513   dc.b    $01,'-'
L8515   dc.b    $01,'*'
L8517   dc.b    $01,'/'
L8519   dc.b    $02,'>='
L851C   dc.b    $01,'>'
L851E   dc.b    $02,'=='
L8521   dc.b    $01,'='
L8523   dc.b    $02,'<>'
L8526   dc.b    $02,'<='
L8529   dc.b    $01,'<'
L852B   dc.b    $02,'||'
L852E   dc.b    $02,'&&'
L8531   dc.b    $02,'^^'
L8534   dc.b    $01,'^'
L8536   dc.b    $01,'&'
L8538   dc.b    $12,'OR'
L853B   dc.b    $13,'AND'
L853F   dc.b    $13,'XOR'
L8543   dc.b    $13,'MOD'
L8547   dc.b    $13,'DIV'
L854B   dc.b    $15,'INSTR'

        dc.b    0                       ;(pad out even)

L8552   lea     L84FA(pc),a2
        jsr     L8748(pc)
        bra.s   L8564                   ;fail
        moveq   #ops.b-256,d4           ;OK
        jsr     L8DFA(pc)
        addq.l  #2,(a7)
L8564   rts

;Token $8E - Separator

L8566   dc.b    5                       ;number of separator symbols

        dc.b    L856C-L8566             ;offsets to separator symbols
        dc.b    L856E-L8566
        dc.b    L8570-L8566
        dc.b    L8572-L8566
        dc.b    L8574-L8566

L856C   dc.b    $01,','                 ;separator symbol list
L856E   dc.b    $01,';'
L8570   dc.b    $01,'\'
L8572   dc.b    $01,'!'
L8574   dc.b    $12,'TO'

        dc.b    0                       ;(pad out even)

L8578   lea     L8566(pc),a2
        jsr     L8748(pc)
        bra.s   L858A                   ;fail
        moveq   #sep.b-256,d4           ;OK
        jsr     L8DFA(pc)
        addq.l  #2,(a7)
L858A   rts

;Token $80 - Space

L858C   moveq   #0,d1
        moveq   #0,d5
L8590   addq.w  #1,d5
        move.b  0(a6,a0.l),d1
        addq.w  #1,a0
        cmpi.b  #' ',d1
        beq.s   L8590
        subq.w  #1,a0
        subq.w  #1,d5
        beq.s   L85AA
        moveq   #spc.b-256,d4
        jsr     L8DFA(pc)
L85AA   rts

;Token $8B - String

L85AC   move.b  0(a6,a0.l),d2
        cmpi.b  #$22,d2
        beq.s   L85BC
        cmpi.b  #"'",d2
        bne.s   L85DE
L85BC   lea     1(a0),a2
        moveq   #-1,d5
        moveq   #10,d3
L85C4   addq.w  #1,a0
        addq.w  #1,d5
        move.b  0(a6,a0.l),d1
        cmp.b   d3,d1
        beq.s   L85DE
        cmp.b   d2,d1
        bne.s   L85C4
        addq.w  #1,a0
        moveq   #str.b-256,d4
        jsr     L8E38(pc)
        addq.l  #2,(a7)
L85DE   rts

;Token $8C - Text

L85E0   jsr     L872C(pc)
        bne.s   L85F2
        movea.l bv_tkp(a6),a3
        cmpi.b  #spc.b,-2(a6,a3.l)
        bne.s   L860A
L85F2   movea.l a0,a2
        move.l  bv_bfp(a6),d5
        subq.l  #1,d5
        movea.l d5,a0
        sub.l   a2,d5
        beq.s   L8608
        moveq   #txt.b-256,d4
        moveq   #0,d2
        jsr     L8E38(pc)
L8608   addq.l  #2,(a7)
L860A   rts

L860C   jsr     L4E4C(pc)
        movea.l bv_rip(a6),a1
        jsr     L3D16(pc)               ;cn_dtof
        bne.s   L8620
        jsr     L8E12(pc)
        addq.l  #2,(a7)
L8620   rts

L8622   move.l  a2,-(a7)
        move.b  d5,d2
        move.b  d2,d1
        moveq   #5,d3
        movea.l bv_ntbas(a6),a2
        movea.l a3,a1
L8630   cmpa.l  bv_ntp(a6),a2
        bge.s   L8684
        tst.l   0(a6,a2.l)
        beq.s   L864C
        movea.l bv_nlbas(a6),a4
        adda.w  2(a6,a2.l),a4
        move.b  0(a6,a4.l),d5
        cmp.b   d2,d5
        beq.s   L8650
L864C   addq.w  #8,a2
        bra.s   L8630

L8650   tst.b   d2
        beq.s   L8672
        move.b  0(a6,a3.l),d4
        addq.w  #1,a3
        bclr    d3,d4
        tst.b   d5
        beq.s   L867C
        addq.w  #1,a4
        move.b  0(a6,a4.l),d6
        bclr    d3,d6
        cmp.b   d4,d6
        bne.s   L867C
        subq.b  #1,d2
        subq.b  #1,d5
        bra.s   L8650

L8672   tst.b   d5
        bne.s   L867C
        move.l  a2,d5
        addq.w  #4,a7
        bra.s   L86FA

L867C   movea.l a1,a3
        move.b  d1,d2
        addq.w  #8,a2
        bra.s   L8630

L8684   moveq   #0,d6
        movea.l a3,a0
        movea.l (a7)+,a2
        jsr     L8966(pc)
        bra.s   L8692
        bra.s   L8704
L8692   tst.b   bv_unrvl(a6)
        beq.s   L869E
        addq.w  #8,a7
        jmp     L848E(pc)

L869E   moveq   #0,d4
        move.b  d2,d4
        jsr     L4DE8(pc)
        move.l  d4,d2
        move.l  #-1,4(a6,a2.l)
        movea.l bv_nlp(a6),a4
        move.l  a4,d3
        sub.l   bv_nlbas(a6),d3
        move.w  d3,2(a6,a2.l)
        add.l   a3,d2
        move.b  -1(a6,d2.l),d3
        moveq   #0,d1
        subi.b  #37,d3
        blt.s   L86D2
        bgt.s   L86D0
        addq.w  #1,d1
L86D0   addq.w  #1,d1
L86D2   addq.w  #1,d1
        move.w  d1,0(a6,a2.l)
        move.l  a2,d5
        moveq   #1,d1
        add.w   d4,d1
        jsr     L4E72(pc)               ;reserve space on name list stack
        move.b  d4,0(a6,a4.l)
L86E6   addq.w  #1,a4
        move.b  0(a6,a3.l),0(a6,a4.l)
        addq.w  #1,a3
        subq.b  #1,d4
        bne.s   L86E6
        addq.w  #1,a4
        move.l  a4,bv_nlp(a6)
L86FA   movea.l d5,a2
        sub.l   bv_ntbas(a6),d5
        lsr.l   #3,d5
        addq.l  #2,(a7)
L8704   rts

L8706   bsr.s   L872C
        bne.s   L872A
        cmpi.b  #1,d2
        bne.s   L872A
L8710   addq.w  #1,a0
        bsr.s   L872C
        beq.s   L8710
        cmpi.b  #'$',d2
        beq.s   L8722
        cmpi.b  #'%',d2
        bne.s   L8728
L8722   tst.b   d3
        bne.s   L872A
        addq.w  #1,a0
L8728   addq.l  #2,(a7)
L872A   rts

L872C   moveq   #0,d1
        move.b  0(a6,a0.l),d1
        blt.s   L8746
        lea     L3C28(pc),a1
        move.b  0(a1,d1.w),d2
        cmpi.b  #1,d2
        beq.s   L8746
        cmpi.b  #2,d2
L8746   rts

L8748   move.l  a0,-(a7)
        moveq   #0,d5
L874C   movea.l (a7),a0
        bsr.s   L8794
L8750   addq.w  #1,d5
        movea.l a2,a1
        cmp.b   (a1),d5
        bgt.s   L8790
        moveq   #0,d1
        move.b  0(a1,d5.w),d1
        adda.w  d1,a1
        move.b  (a1)+,d1
        cmp.b   (a1)+,d0
        bne.s   L8750
        move.b  d1,d0
        swap    d1
        move.b  d0,d1
        andi.b  #$0F,d1
L8770   subq.b  #1,d1
        ble.s   L877E
        addq.w  #1,a0
        bsr.s   L8794
        cmp.b   (a1)+,d0
        beq.s   L8770
        bra.s   L874C

L877E   swap    d1
        lsr.b   #4,d1
        addq.w  #1,a0
        beq.s   L878C
        jsr     L872C(pc)
        beq.s   L874C
L878C   addq.l  #2,4(a7)
L8790   addq.w  #4,a7
        rts

L8794   moveq   #0,d0
        move.b  0(a6,a0.l),d0
        blt.s   L87AC
        lea     L3C28(pc),a3
        cmpi.b  #1,0(a3,d0.w)
        bne.s   L87AC
        bclr    #5,d0
L87AC   rts

L87AE   dc.w    L84CE-L87C4
        dc.w    L860C-L87C4
        dc.w    L860C-L87C4
        dc.w    L4FFC-L87C4
        dc.w    L8552-L87C4
        dc.w    L84B6-L87C4
        dc.w    L8578-L87C4
        dc.w    L85AC-L87C4
        dc.w    L85E0-L87C4
        dc.w    L8474-L87C4
        dc.w    L84CA-L87C4

L87C4   lsr.b   #1,d6
        jsr     L858C(pc)
        add.b   d6,d6
        move.w  L87AE-2(pc,d6.w),d6
        jmp     L87C4(pc,d6.w)

;Vector 'basic syntax analyser'

L87D4   moveq   #0,d7
        moveq   #0,d6
        move.l  (a2)+,-(a7)             ;pointer to keywords list
        move.l  (a2),-(a7)              ;pointer to syntax tables
        movea.l (a7),a5
        move.w  (a5),d6
        adda.w  d6,a5
        moveq   #0,d4
        jsr     L8AA0(pc)               ;put d4.l onto temp graph stack
        movea.l (a6),a0                 ;bv_bfbas buffer base
        bra.s   L880E

L87EC   subi.b  #$80,d6
        lsr.b   #1,d6
        bne.s   L87FE
        subq.w  #1,a5
        jsr     L8A7A(pc)
        bra     L88CE

L87FE   move.l  a5,d4
        jsr     L8AA0(pc)               ;put d4.l onto temp graph stack
        add.b   d6,d6
        movea.l (a7),a5
        move.w  -2(a5,d6.w),d6
        adda.w  d6,a5
L880E   jsr     L8A7E(pc)               ;put 0, a0.l and bv_tkp onto backtrack stack
L8812   moveq   #0,d6
        move.b  (a5)+,d6
        beq.s   L887A
        bmi.s   L87EC
        bclr    #6,d6
        beq.s   L882C
        movea.l 4(a7),a2
        jsr     L8966(pc)
        bra.s   L8846
        bra.s   L8864
L882C   bclr    #5,d6
        beq.s   L883A
        jsr     L8B34(pc)
        bra.s   L8846
        bra.s   L8864
L883A   movea.l 4(a7),a2                ;pointer to keywords list
        jsr     L87C4(pc)
        bra.s   L8846
        bra.s   L8864
L8846   movea.l bv_btp(a6),a2
L884A   movea.l 0(a6,a2.l),a3
        movea.l 4(a6,a2.l),a0
        move.l  8(a6,a2.l),d3
        adda.w  #12,a2
        blt.s   L884A
        addq.w  #1,a5
        move.l  a3,bv_tkp(a6)
        bra.s   L8812

L8864   jsr     L8A7A(pc)
L8868   moveq   #0,d6
        move.b  (a5),d6
        beq.s   L88CE
        bmi.s   L8874
        adda.w  d6,a5
        bra.s   L8812

L8874   neg.b   d6
        suba.w  d6,a5
        bra.s   L8812

L887A   movea.l bv_btp(a6),a4
        cmpa.l  bv_btbas(a6),a4
        bgt.s   L88B2
        movea.l 0(a6,a4.l),a3
        movea.l 4(a6,a4.l),a0
        movea.l 8(a6,a4.l),a5
        adda.w  #12,a4
        move.l  a4,bv_btp(a6)
        move.l  a5,d3
        bgt.s   L88A6
        beq.s   L88B8
        move.l  a0,d4
        jsr     L8AA0(pc)               ;put d4.l onto temp graph stack
        bra.s   L887A

L88A6   btst    d7,-1(a5)
        beq.s   L8846
        move.l  -16(a6,a4.l),d3
        bmi.s   L8846
L88B2   addq.w  #8,a7
        moveq   #err.nc,d0
        rts

L88B8   movea.l bv_tgp(a6),a4
        movea.l 0(a6,a4.l),a5
        addq.w  #4,a4
        move.l  a4,bv_tgp(a6)
        move.l  a5,d3
        beq.s   L88B2
        bra     L8846

L88CE   jsr     L4E52(pc)               ;reserve 12 bytes on backtrack stack
        movea.l bv_btp(a6),a4
        movea.l bv_tgp(a6),a2
        subq.w  #8,a4
        move.l  #-1,4(a6,a4.l)
        move.l  0(a6,a2.l),0(a6,a4.l)
        movea.l 0(a6,a4.l),a5
        subq.w  #4,a4
        move.l  bv_tkp(a6),0(a6,a4.l)
        addq.w  #4,a2
        move.l  a2,bv_tgp(a6)
        move.l  a4,bv_btp(a6)
        move.l  a5,d3
        bne     L8868
        addq.w  #8,a7
        moveq   #err.ok,d0
        rts

;Vector 'initialise basic stacks'

L890C   move.l  bv_btbas(a6),bv_btp(a6)
        move.l  bv_tgbas(a6),bv_tgp(a6)
        move.l  bv_tkbas(a6),bv_tkp(a6)
        moveq   #126,d1
        jsr     L4E6E(pc)               ;reserve space on token list stack
        moveq   #32,d1
        jsr     L4E4E(pc)               ;bv_chrix
        move.l  bv_rip(a6),bv_tgbas(a6)
        subi.l  #32,bv_tgbas(a6)
        move.l  bv_tgbas(a6),bv_tgp(a6)
        moveq   #80,d1
        jsr     L4E5A(pc)               ;reserve space on temp graph stack
        move.l  bv_tgp(a6),bv_btbas(a6)
        subi.l  #80,bv_btbas(a6)
        move.l  bv_btbas(a6),bv_btp(a6)
        moveq   #0,d0
        jsr     L4E32(pc)
        move.w  #512,d1
        jmp     L4E54(pc)               ;reserve space on backtrack stack

L8966   movem.l d0-d5/a1-a5,-(a7)
        movea.l a2,a3
        movea.l a0,a1
        moveq   #0,d3
        moveq   #0,d4
        moveq   #0,d5
        moveq   #0,d0
        lsr.b   #1,d6
        beq.s   L8980
        jsr     L858C(pc)
        bra.s   L8986

L8980   st      d0
        swap    d0
        moveq   #1,d6
L8986   moveq   #0,d1
        cmp.b   (a2),d6
        bgt     L8A48
        move.b  0(a2,d6.w),d1
        adda.l  d1,a2
        move.b  (a2)+,d1
        ror.l   #4,d1
        move.b  d1,d4
        swap    d1
        rol.w   #4,d1
        move.b  d1,d3
L89A0   subq.b  #1,d4
        blt.s   L89FC
        move.b  (a2)+,d2
        cmpi.b  #96,d2
        bgt.s   L89B2
        st      d0
        sf      d5
        bra.s   L89BC

L89B2   sf      d0
        tst.b   d5
        bne.s   L89A0
        subi.b  #32,d2
L89BC   move.b  0(a6,a0.l),d1
        addq.w  #1,a0
        cmpi.b  #96,d1
        ble.s   L89CC
        subi.b  #32,d1
L89CC   cmp.b   d2,d1
        beq.s   L89A0
        tst.b   d0
        bne.s   L89DA
        st      d5
        subq.w  #1,a0
        bra.s   L89A0

L89DA   swap    d0
        tst.b   d0
        beq.s   L8A48
        swap    d5
        tst.b   d5
        swap    d5
        beq.s   L89EC
        tst.b   d3
        bne.s   L89F4
L89EC   addq.w  #1,d6
        movea.l a1,a0
        movea.l a3,a2
        bra.s   L8986

L89F4   movea.l a4,a0
        subq.b  #1,d3
        adda.l  d4,a2
        bra.s   L8A30

L89FC   swap    d0
        tst.b   d0
        beq.s   L8A38
        move.b  0(a6,a0.l),d1
        blt.s   L8A48
        lea     L3C28(pc),a5
        cmpi.b  #1,0(a5,d1.w)
        beq.s   L8A1E
        cmpi.b  #2,0(a5,d1.w)
        beq.s   L8A48
        bra.s   L8A40

L8A1E   swap    d5
        tst.b   d5
        bne.s   L8A48
        tst.b   d3
        beq.s   L8A48
        st      d5
        swap    d5
        subq.b  #1,d3
        movea.l a0,a4
L8A30   move.b  (a2)+,d4
        lsr.b   #4,d4
        bra     L89A0

L8A38   moveq   #key.b-256,d4
        move.b  d6,d5
        jsr     L8DFA(pc)
L8A40   movem.l (a7)+,d0-d5/a1-a5
        addq.l  #2,(a7)
        rts

L8A48   movem.l (a7)+,d0-d5/a1-a5
        rts

;Vector 'error when compiling'

L8A4E   move.l  bv_tkbas(a6),bv_tkp(a6)
        movea.l (a6),a0                 ;bv_bfbas
        jsr     L8474(pc)
        bra.s   L8A76

        moveq   #key.b-256,d4
        moveq   #31,d5
        jsr     L8DFA(pc)
        jsr     L85F2(pc)
        bra.s   L8A76

        moveq   #sym.b-256,d4
        moveq   #10,d5
        jsr     L8DFA(pc)
        moveq   #err.ok,d0
        rts

L8A76   moveq   #err.bl,d0
        rts

L8A7A   move.l  a5,d4
        bra.s   L8A80

L8A7E   moveq   #0,d4

;Put d4.l, a0.l and bv_tkp onto backtrack stack

L8A80   jsr     L4E52(pc)               ;reserve 12 bytes on backtrack stack
        movea.l bv_btp(a6),a4
        suba.w  #12,a4
        move.l  d4,8(a6,a4.l)
        move.l  a0,4(a6,a4.l)
        move.l  bv_tkp(a6),0(a6,a4.l)
        move.l  a4,bv_btp(a6)
        rts

;Put d4.l onto temporary graph stack

L8AA0   jsr     L4E58(pc)               ;reserve 4 bytes on temp graph stack
        movea.l bv_tgp(a6),a4
        subq.w  #4,a4
        move.l  d4,0(a6,a4.l)
        move.l  a4,bv_tgp(a6)
        rts

;Vector 'format precompiled basic line'

L8AB4   movea.l bv_tkbas(a6),a0
L8AB8   cmpa.l  bv_tkp(a6),a0
        bge.s   L8ADA
        cmpi.b  #key.b,0(a6,a0.l)
        bne.s   L8ACC
        addq.w  #2,a0
        bsr.s   L8AF4
        bra.s   L8AB8

L8ACC   cmpi.w  #$8409,0(a6,a0.l)       ;token symbol forced space ?
        addq.w  #2,a0
        bne.s   L8AB8
        bsr.s   L8B10
        bra.s   L8AB8

L8ADA   movea.l bv_tkbas(a6),a0
        cmpi.b  #spc.b,0(a6,a0.l)
        bne.s   L8AEA
        addq.w  #2,a0
        bsr.s   L8B10
L8AEA   cmpi.w  #$8D00,0(a6,a0.l)       ;token line number ?
        bne.s   L8B0E
        addq.w  #4,a0
L8AF4   cmpi.b  #spc.b,0(a6,a0.l)
        bne.s   L8B0E
        move.b  1(a6,a0.l),d1
        addq.w  #2,a0
        subq.b  #1,d1
        bgt.s   L8B0A
        bsr.s   L8B10
        bra.s   L8B0E

L8B0A   move.b  d1,-1(a6,a0.l)
L8B0E   rts

L8B10   lea     -2(a0),a2
L8B14   move.w  0(a6,a0.l),-2(a6,a0.l)
        addq.w  #2,a0
        cmpa.l  bv_tkp(a6),a0
        blt.s   L8B14
        subq.l  #2,bv_tkp(a6)
        movea.l a2,a0
        rts

L8B30   dc.b    '=',':','#',',','(',')','{','}',' ',$0A

L8B34   lsr.b   #1,d6
L8B36   move.b  0(a6,a0.l),d1
        cmp.b   L8B30-1(pc,d6.w),d1
        beq.s   L8B4C
        cmpi.b  #' ',d1
        bne.s   L8B58
        jsr     L858C(pc)
        bra.s   L8B36

L8B4C   addq.w  #1,a0
        moveq   #sym.b-256,d4
        move.b  d6,d5
        jsr     L8DFA(pc)
        addq.l  #2,(a7)
L8B58   rts

;Vector 'commands syntax table'

;--------------------------------------
L8B5A   include syntax_tables
;--------------------------------------

L8DFA   moveq   #2,d1
        bsr.s   L8E68
        move.b  d4,0(a6,a3.l)
        move.b  d5,1(a6,a3.l)
        addq.w  #2,a3
        bra.s   L8E5E

L8E0A   moveq   #4,d1
        moveq   #0,d2
        bsr.s   L8E76
        bra.s   L8E5E

L8E12   moveq   #6,d1
        move.l  a1,bv_rip(a6)
        bsr.s   L8E68
        movea.l bv_rip(a6),a1
        addq.l  #6,bv_rip(a6)
        move.w  0(a6,a1.l),d2
        addi.w  #$F000,d2
        move.w  d2,0(a6,a3.l)
        move.l  2(a6,a1.l),2(a6,a3.l)
        addq.w  #6,a3
        bra.s   L8E5E

L8E38   moveq   #1,d1
        add.w   d5,d1
        bvs.s   L8E64
        addq.w  #4,d1
        bsr.s   L8E76
        beq.s   L8E5E
        move.w  d5,d1
L8E46   move.b  0(a6,a2.l),0(a6,a3.l)
        addq.w  #1,a3
        addq.w  #1,a2
        subq.w  #1,d1
        bne.s   L8E46
        btst    d7,d5
        beq.s   L8E5E
        move.b  d7,0(a6,a3.l)
        addq.w  #1,a3
L8E5E   move.l  a3,bv_tkp(a6)
        rts

L8E64   addq.w  #4,a7
        rts

L8E68   move.b  d2,-(a7)
        jsr     L4E6E(pc)               ;reserve space on token list stack
        move.b  (a7)+,d2
        movea.l bv_tkp(a6),a3
        rts

L8E76   bsr.s   L8E68
        move.b  d4,0(a6,a3.l)
        move.b  d2,1(a6,a3.l)
        move.w  d5,2(a6,a3.l)
        addq.w  #4,a3
        rts

;Vector 'store precompiled line'

L8E88   movea.l bv_tkbas(a6),a1
        cmpi.b  #lno.b,0(a6,a1.l)
        bne     L8F82
        move.w  2(a6,a1.l),d2
        move.l  bv_tkp(a6),d1
        sub.l   a1,d1
        subq.w  #6,d1
        seq     d0
        addq.w  #6,d1
        moveq   #0,d6
        moveq   #0,d5
        movea.l bv_pfbas(a6),a0
        moveq   #0,d3
        bra.s   L8EB6

L8EB2   move.w  2(a6,a0.l),d3
L8EB6   add.w   d5,d6
        adda.l  d6,a0
        move.w  0(a6,a0.l),d5
        addq.w  #2,a0
        cmpa.l  bv_pfp(a6),a0
        blt.s   L8ECE
        addq.b  #1,d0
        bgt.s   L8F44
        subq.w  #2,a0
        bra.s   L8EFE

L8ECE   cmp.w   2(a6,a0.l),d2
        bgt.s   L8EB2
        beq.s   L8F00
        subq.w  #2,a0
        tst.b   d0
        bne.s   L8EFE
        addq.w  #2,d1
        bsr     L8F84
        bsr.s   L8F3A
        subq.w  #2,d1
        move.w  d1,d5
        sub.w   d6,d5
        move.w  d5,0(a6,a0.l)
        moveq   #1,d0
L8EF0   bsr     L8F92
        add.w   0(a6,a0.l),d6
        sub.w   d1,d6
        move.w  d6,0(a6,a0.l)
L8EFE   bra.s   L8F5C

L8F00   tst.b   d0
        beq.s   L8F06
        moveq   #-2,d1
L8F06   add.w   d6,d5
        lea     -2(a0),a3
        adda.l  d5,a0
        cmpa.l  bv_pfp(a6),a0
        bge.s   L8F3C
        sub.w   d5,d1
        blt.s   L8F1A
        bsr.s   L8F84
L8F1A   bsr.s   L8F3A
        movea.l a3,a0
        add.w   d5,d1
        bgt.s   L8F2E
        add.w   0(a6,a0.l),d5
        sub.w   d6,d5
        move.w  d5,0(a6,a0.l)
        bra.s   L8F5C

L8F2E   move.w  d1,d4
        sub.w   d6,d4
        move.w  d4,0(a6,a0.l)
        move.w  d5,d6
        bra.s   L8EF0

L8F3A   bra.s   L8FA6

L8F3C   lea     2(a3),a0
        move.l  a3,bv_pfp(a6)
L8F44   subq.w  #2,a0
        tst.b   d0
        blt.s   L8F5C
        addq.w  #2,d1
        bsr.s   L8F84
        subq.w  #2,d1
        sub.w   d6,d1
        move.w  d1,0(a6,a0.l)
        bsr.s   L8F92
        move.l  a0,bv_pfp(a6)
L8F5C   tst.b   bv_arrow(a6)
        beq.s   L8F80
        sf      bv_auto(a6)
        bgt.s   L8F70
        move.w  d3,bv_edlin(a6)
        bne.s   L8F7C
        bra.s   L8F80

L8F70   cmpa.l  bv_pfp(a6),a0
        bge.s   L8F7C
        move.w  4(a6,a0.l),bv_edlin(a6)
L8F7C   st      bv_auto(a6)
L8F80   addq.l  #2,(a7)
L8F82   rts

L8F84   movem.l d0-d3/a0-a3,-(a7)
        jsr     L4E82(pc)               ;reserve space in program file stack
        movem.l (a7)+,d0-d3/a0-a3
        rts

L8F92   addq.w  #2,a0
        move.w  0(a6,a1.l),0(a6,a0.l)
        addq.w  #2,a1
        cmpa.l  bv_tkp(a6),a1
        blt.s   L8F92
        addq.w  #2,a0
        rts

L8FA6   movem.l d0-d3/a0-a3,-(a7)
        ext.l   d1
        tst.l   d1
        beq.s   L8FE0
        movea.l bv_pfp(a6),a1
        blt.s   L8FCA
        lea     0(a1,d1.w),a2
L8FBA   subq.w  #2,a1
        subq.w  #2,a2
        move.w  0(a6,a1.l),0(a6,a2.l)
        cmpa.l  a0,a1
        bgt.s   L8FBA
        bra.s   L8FDC

L8FCA   lea     0(a0,d1.w),a2
L8FCE   move.w  0(a6,a0.l),0(a6,a2.l)
        addq.w  #2,a0
        addq.w  #2,a2
        cmpa.l  a1,a0
        blt.s   L8FCE
L8FDC   add.l   d1,bv_pfp(a6)
L8FE0   movem.l (a7)+,d0-d3/a0-a3
        rts

L8FE6   st      bv_print(a6)
        movea.l bv_lnbas(a6),a1
        move.l  bv_lnp(a6),d0
        sub.l   a1,d0
        bne.s   L9000
        move.w  d2,d4
        beq.s   L9060
        move.w  d2,d6
        bra.s   L9044

L8FFE   move.w  d4,d2
L9000   move.w  0(a6,a1.l),d4
        tst.w   d2
        beq.s   L8FFE
        move.w  bv_lsbas(a6),d6
        cmp.w   bv_lsbef(a6),d2
        blt.s   L9060
        cmp.w   d4,d2
        blt.s   L9032
        cmp.w   bv_lsaft(a6),d2
        bgt.s   L9060
        cmp.w   d6,d2
        bgt.s   L902A
        tst.b   d5
        bge.s   L9044
        move.w  bv_lsaft(a6),d6
        bra.s   L9044

L902A   tst.b   d5
        blt.s   L9060
        move.w  d2,d6
        bra.s   L9044

L9032   tst.b   d5
        blt.s   L9060
        move.w  d2,d4
        move.w  bv_maxln(a6),d0
        sub.w   bv_totln(a6),d0
        bgt.s   L9044
        subq.w  #1,d6
L9044   move.w  d2,bv_lsfil(a6)
        moveq   #sd.pos,d0
        moveq   #0,d2
        moveq   #0,d1
        bsr.s   L905C
        moveq   #0,d7
        jsr     L7518(pc)               ;(convert precompiled basic to ascii)
        moveq   #sd.clrrt,d0
        bsr.s   L905C
        moveq   #sd.clrbt,d0
L905C   moveq   #forever,d3
        trap    #3
L9060   rts

L9062   dc.b    2,2,4,4,2,2,2,4,4,2,4,-5,-5,4,2,0

L9072   moveq   #0,d0
        moveq   #6,d1
        move.b  0(a6,a4.l),d0
        beq.s   L9098
        subi.b  #$80,d0
        cmpi.b  #$70,d0
        bge.s   L9096
        move.b  L9062(pc,d0.w),d1
        bge.s   L9096
        neg.b   d1
        add.w   2(a6,a4.l),d1
        bclr    #0,d1
L9096   adda.l  d1,a4
L9098   move.b  0(a6,a4.l),d0
        move.w  0(a6,a4.l),d1
        rts

L90A2   move.l  a1,-(a7)
        movea.l bv_chbas(a6),a1
        adda.w  #80,a1
        move.l  0(a6,a1.l),d0
        sub.l   a0,d0
        movea.l (a7)+,a1
        rts

L90B6   move.l  a5,-(a7)
        movea.l a0,a5
        bsr.s   L90A2
        seq     bv_lsany(a6)
        bne.s   L9108
        moveq   #sd.chenq,d0
        lea     bv_lenln(a6),a1
        moveq   #forever,d3
        trap    #3
        move.l  bv_lnbas(a6),bv_lnp(a6)
        clr.w   bv_totln(a6)
        moveq   #100,d1
        jsr     L4E7E(pc)               ;reserve space in line number table
        move.w  bv_linum(a6),bv_lsbef(a6)
        move.w  #$7FFF,bv_lsaft(a6)
        bra.s   L9108

L90EA   dc.w    L92F8-L90EA
        dc.w    L9314-L90EA
        dc.w    L9292-L90EA
        dc.w    L9292-L90EA
        dc.w    L9338-L90EA
        dc.w    L930E-L90EA
        dc.w    L931A-L90EA
        dc.w    L9292-L90EA
        dc.w    L929C-L90EA
        dc.w    L9292-L90EA
        dc.w    L9292-L90EA
        dc.w    L92E2-L90EA
        dc.w    L92EC-L90EA
        dc.w    L9238-L90EA
        dc.w    L9308-L90EA

L9108   move.l  (a2),-(a7)
        addq.w  #2,a4
        moveq   #0,d4
        move.b  #lno.b,d4
L9112   move.l  (a6),bv_bfp(a6)         ;bv_bfbas
L9116   cmpa.l  bv_pfp(a6),a4
        blt.s   L9130
        tst.b   bv_print(a6)
        bne     L9202
        tst.b   bv_auto(a6)
        beq     L9202
        bsr     L9250
L9130   moveq   #16,d1
        bsr     L92C0
        movea.l (a7),a1
        subi.b  #$80,d4
        cmpi.b  #$70,d4
        bge     L91F2
        add.b   d4,d4
        move.w  L90EA(pc,d4.w),d4
        jsr     L90EA(pc,d4.w)
        cmpi.b  #lno.b,d4
        bne     L91F4
        tst.b   bv_print(a6)
        beq     L91FE
        tst.b   bv_lsany(a6)
        beq.s   L91B8
        movea.l (a6),a1                 ;bv_bfbas
        move.l  bv_bfp(a6),d2
        sub.l   a1,d2
        movea.l bv_lnp(a6),a2
        move.w  bv_lsbas(a6),0(a6,a2.l)
        subq.w  #2,d2
        divu.w  bv_lenln(a6),d2
        addq.w  #1,d2
        move.w  d2,2(a6,a2.l)
        add.w   bv_totln(a6),d2
L9186   cmp.w   bv_maxln(a6),d2
        ble.s   L91AC
        move.w  bv_lsfil(a6),d0
        beq.s   L9198
        cmp.w   bv_lsbas(a6),d0
        blt.s   L91E4
L9198   movea.l bv_lnbas(a6),a2
        sub.w   2(a6,a2.l),d2
        move.w  0(a6,a2.l),bv_lsbef(a6)
        addq.l  #4,bv_lnbas(a6)
        bra.s   L9186

L91AC   move.w  d2,bv_totln(a6)
        addq.l  #4,bv_lnp(a6)
        jsr     L4F9E(pc)
L91B8   moveq   #io.sstrg,d0
        movea.l (a6),a1                 ;bv_bfbas
        move.l  bv_bfp(a6),d2
        sub.l   a1,d2
        movea.l a5,a0
        moveq   #forever,d3
        trap    #4
        trap    #3
        tst.l   d0
        bne.s   L9204
        move.l  d1,d2
        moveq   #sd.clrrt,d0
        trap    #3
        tas     bv_brk(a6)
        bne     L9112
        move.w  bv_lsbas(a6),bv_lsaft(a6)
        bra.s   L9202

L91E4   move.w  bv_lsbas(a6),bv_lsaft(a6)
        move.w  -4(a6,a2.l),bv_lsbas(a6)
        bra.s   L9202

L91F2   bsr.s   L9216
L91F4   jsr     L9072(pc)
        move.w  d0,d4
        bra     L9116

L91FE   subq.l  #1,bv_bfp(a6)
L9202   moveq   #err.ok,d0
L9204   addq.w  #4,a7
        movea.l a5,a0
        movea.l (a7)+,a5
        rts

L920C   jsr     L4E4C(pc)
        movea.l bv_rip(a6),a1
        rts

L9216   bsr.s   L920C
        move.w  0(a6,a4.l),d2
        subi.w  #$F000,d2
        subq.w  #6,a1
        move.w  d2,0(a6,a1.l)
        move.l  2(a6,a4.l),2(a6,a1.l)
        movea.l bv_bfp(a6),a0
        jsr     L3EF6(pc)               ;cn_ftod
        bra     L9364

L9238   bsr.s   L920C
        move.w  2(a6,a4.l),d1
        tst.b   bv_print(a6)
        bne.s   L925C
        tst.b   bv_auto(a6)
        beq.s   L925C
        cmp.w   bv_edlin(a6),d1
        beq.s   L925C
L9250   bsr.s   L920C
        move.w  bv_edlin(a6),d1
        bsr.s   L9276
L9258   addq.w  #4,a7
        bra.s   L9202

L925C   tst.b   bv_lsany(a6)
        beq.s   L9288
        cmp.w   d6,d1
        ble.s   L9272
        tst.w   bv_lsfil(a6)
        bne.s   L9272
        move.w  d1,bv_lsaft(a6)
        bra.s   L9258

L9272   move.w  d1,bv_lsbas(a6)
L9276   subq.w  #2,a1
        move.w  d1,0(a6,a1.l)
        movea.l bv_bfp(a6),a0
        jsr     L3E54(pc)               ;cn_itod
        bra     L9316

L9288   cmp.w   d6,d1
        bgt.s   L9258
        bra.s   L9276

L928E   dc.l    $3F3F3F3F

L9292   moveq   #4,d1
        lea     L928E(pc),a1
        bra     L935A

L929C   move.w  2(a6,a4.l),d2
        lsl.l   #3,d2
        movea.l bv_ntbas(a6),a1
        adda.w  d2,a1
        move.w  2(a6,a1.l),d2
        blt.s   L9292
        movea.l bv_nlbas(a6),a1
        adda.w  d2,a1
        moveq   #0,d1
        move.b  0(a6,a1.l),d1
        addq.w  #1,a1
        bra     L9354

L92C0   suba.l  bv_pfbas(a6),a4
        suba.l  bv_pfbas(a6),a1
        movem.l d1/a1/a4,-(a7)
        jsr     L4E6A(pc)               ;reserve space in buffer
        movea.l bv_bfp(a6),a0
        movem.l (a7)+,d1/a1/a4
        adda.l  bv_pfbas(a6),a1
        adda.l  bv_pfbas(a6),a4
        rts

L92E2   bsr.s   L92E6
        bsr.s   L92EC
L92E6   move.b  1(a6,a4.l),d2
        bra.s   L934C

L92EC   move.w  2(a6,a4.l),d1
        beq.s   L9306
        lea     4(a4),a1
        bra.s   L9354

L92F8   move.b  1(a6,a4.l),d1
        bsr.s   L92C0
L92FE   moveq   #' ',d2
        bsr.s   L934C
        subq.b  #1,d1
        bgt.s   L92FE
L9306   rts

L9308   lea     L936C(pc),a1
        bra.s   L931E

L930E   lea     L938E(pc),a1
        bra.s   L931E

L9314   bsr.s   L931E
L9316   moveq   #$20,d2
        bra.s   L934C

L931A   lea     L937E(pc),a1
L931E   move.b  1(a6,a4.l),d1
        move.b  0(a1,d1.w),d1
        adda.l  d1,a1
        move.b  (a1)+,d1
        lsr.l   #4,d1
        bra.s   L935A

L932E   dc.b    '=',':','#',',','(',')','{','}',' ',$0A

L9338   move.b  1(a6,a4.l),d1
        move.b  L932E-1(pc,d1.w),d2
        moveq   #0,d4
        sub.b   d2,d1
        bne.s   L934C
        addq.w  #4,a4
        move.b  #lno.b,d4
L934C   move.b  d2,0(a6,a0.l)
        addq.w  #1,a0
        bra.s   L9364

L9354   bsr     L92C0
        adda.l  a6,a1
L935A   move.b  (a1)+,0(a6,a0.l)
        addq.w  #1,a0
        subq.w  #1,d1
        bgt.s   L935A
L9364   move.l  a0,bv_bfp(a6)
        moveq   #err.ok,d0
        rts

;Separators

L936C   dc.b    5                       ;number of separators symbols

        dc.b    L9372-L936C             ;offsets to separator symbols
        dc.b    L9374-L936C
        dc.b    L9376-L936C
        dc.b    L9378-L936C
        dc.b    L937A-L936C

L9372   dc.b    $10,','                 ;separator symbol list
L9374   dc.b    $10,';'
L9376   dc.b    $10,'\'
L9378   dc.b    $10,'!'
L937A   dc.b    $20,'TO'

        dc.b    $00                     ;(pad out even)

;Mono operators

L937E   dc.b    4                       ;number of mono operators symbols

        dc.b    L9383-L937E             ;offsets to mono operator symbols
        dc.b    L9385-L937E
        dc.b    L9387-L937E
        dc.b    L938A-L937E

L9383   dc.b    $10,'-'                 ;mono operator symbol list
L9385   dc.b    $10,'+'
L9387   dc.b    $20,'~~'
L938A   dc.b    $30,'NOT'

;Operators

L938E   dc.b    22                      ;number of operators symbols

        dc.b    L93A5-L938E             ;offsets to operator symbols
        dc.b    L93A7-L938E
        dc.b    L93A9-L938E
        dc.b    L93AB-L938E
        dc.b    L93AC-L938E
        dc.b    L93AF-L938E
        dc.b    L93B2-L938E
        dc.b    L93B5-L938E
        dc.b    L93B7-L938E
        dc.b    L93BA-L938E
        dc.b    L93BD-L938E
        dc.b    L93BF-L938E
        dc.b    L93C2-L938E
        dc.b    L93C5-L938E
        dc.b    L93C8-L938E
        dc.b    L93CA-L938E
        dc.b    L93CC-L938E
        dc.b    L93CF-L938E
        dc.b    L93D3-L938E
        dc.b    L93D7-L938E
        dc.b    L93DB-L938E
        dc.b    L93DF-L938E

L93A5   dc.b    $10,'+'                 ;operator symbol list
L93A7   dc.b    $10,'-'
L93A9   dc.b    $10,'*'
L93AB   dc.b    $10,'/'
L93AC   dc.b    $20,'>='
L93AF   dc.b    $10,'>'
L93B2   dc.b    $20,'=='
L93B5   dc.b    $10,'='
L93B7   dc.b    $20,'<>'
L93BA   dc.b    $20,'<='
L93BD   dc.b    $10,'<'
L93BF   dc.b    $20,'||'
L93C2   dc.b    $20,'&&'
L93C5   dc.b    $20,'^^'
L93C8   dc.b    $10,'^'
L93CA   dc.b    $10,'&'
L93CC   dc.b    $20,'OR'
L93CF   dc.b    $30,'AND'
L93D3   dc.b    $30,'XOR'
L93D7   dc.b    $30,'MOD'
L93DB   dc.b    $30,'DIV'
L93DF   dc.b    $50,'INSTR'

        dc.b    0                       ;(pad out even)

L93E6   andi.b  #$0F,1(a6,a2.l)
        tst.l   4(a6,a2.l)
        bge.s   L93F6
        moveq   #err.bn,d0
        rts

L93F6   jsr     L4E5E(pc)               ;reserve 8 bytes on name table stack
        movea.l bv_ntp(a6),a3
        addq.l  #8,bv_ntp(a6)
        move.w  0(a6,a2.l),0(a6,a3.l)
        move.w  d4,2(a6,a3.l)
        movea.l 4(a6,a2.l),a2
        adda.l  bv_vvbas(a6),a2
        move.w  4(a6,a2.l),d1
        lsl.l   #2,d1
        addq.w  #6,d1
        jsr     L7338(pc)
L9420   move.w  0(a6,a2.l),0(a6,a0.l)
        addq.w  #2,a2
        addq.w  #2,a0
        subq.w  #2,d1
        bgt.s   L9420
L942E   jsr     LA56C(pc)
        cmpi.w  #$8401,d1               ;token symbol '=' ?
        beq.s   L9456
        cmpi.w  #$8405,d1               ;token symbol '(' ?
        bne     L94C2
        addq.w  #2,a4
        movea.l a4,a0
        lea     8(a3),a5
        jsr     L6272(pc)
        lea     -8(a5),a3
        movea.l a0,a4
        bne.s   L948E
        bra.s   L942E

L9456   addq.w  #2,a4
        cmpi.b  #2,0(a6,a3.l)
        beq.s   L9478
        cmpi.b  #1,1(a6,a3.l)
        bgt.s   L94C6
        movea.l 4(a6,a3.l),a2
        adda.l  bv_vvbas(a6),a2
        cmpi.w  #1,4(a6,a2.l)
        bgt.s   L94C6
L9478   movea.l a4,a0
        move.b  1(a6,a3.l),d0
        move.l  a3,-(a7)
        jsr     L5A84(pc)
        movea.l (a7)+,a3
        movea.l a0,a4
        bne.s   L948E
        jsr     L72C2(pc)               ;bp_let
L948E   cmpi.b  #3,0(a6,a3.l)
        bne.s   L94AA
        movea.l 4(a6,a3.l),a2
        adda.l  bv_vvbas(a6),a2
        move.l  d0,-(a7)
        move.l  a3,-(a7)
        jsr     L9A3C(pc)
        movea.l (a7)+,a3
        move.l  (a7)+,d0
L94AA   clr.l   0(a6,a3.l)
        clr.l   4(a6,a3.l)
        addq.w  #8,a3
        cmpa.l  bv_ntp(a6),a3
        bne.s   L94BE
        subq.l  #8,bv_ntp(a6)
L94BE   tst.l   d0
        rts

L94C2   moveq   #err.xp,d0
        bra.s   L948E

L94C6   moveq   #err.ni,d0
        bra.s   L948E

L94CA   jsr     L4E64(pc)               ;reserve 22 bytes on return table stack
        movea.l bv_rtp(a6),a5
        tst.b   d5
        beq.s   L9504
        move.l  d4,d3
        movea.l bv_ntp(a6),a3
        move.l  a3,0(a6,a5.l)
        move.l  a3,4(a6,a5.l)
        move.l  a3,8(a6,a5.l)
        move.w  4(a6,a2.l),d4
        bne.s   L94F2
        moveq   #err.nf,d0
        rts

L94F2   move.w  d4,12(a6,a5.l)
        move.b  1(a6,a2.l),$0E(a6,a5.l)
        sf      $0F(a6,a5.l)
        adda.w  #16,a5
L9504   move.b  d5,0(a6,a5.l)
        move.b  bv_stmnt(a6),1(a6,a5.l)
        move.w  bv_linum(a6),2(a6,a5.l)
        move.l  bv_inlin(a6),4(a6,a5.l)
        addq.w  #8,a5
        move.l  a5,bv_rtp(a6)
        tst.b   d5
        beq.s   L9580
        cmpi.b  #3,d5
        beq.s   L9578
        movea.l a4,a0
        jsr     L614A(pc)
        movea.l bv_rtp(a6),a5
        move.l  bv_ntp(a6),-$14(a6,a5.l)
        move.l  bv_ntp(a6),-$10(a6,a5.l)
        tst.l   d0
        bne.s   L9552
        move.w  0(a6,a0.l),d1
        cmpi.b  #2,d5
        beq.s   L9562
        bsr.s   L959E
        beq.s   L9576
L9552   bsr     L97C8
        subi.l  #24,bv_rtp(a6)
        moveq   #err.bp,d0
        rts

L9562   cmpi.w  #$8406,d1               ;token symbol ')' ?
        bne.s   L9552
        addq.w  #2,a0
        suba.l  bv_tkbas(a6),a0
        move.l  a0,8(a7)
        adda.l  bv_tkbas(a6),a0
L9576   movea.l a0,a4
L9578   st      -9(a6,a5.l)
        movea.l -24(a6,a5.l),a3
L9580   tst.b   bv_sing(a6)
        beq.s   L95A2
        bsr.s   L958E
        beq.s   L95A2
        moveq   #err.nf,d0
        bra.s   L95B4

L958E   movea.l bv_pfbas(a6),a4
        sf      bv_sing(a6)
        clr.w   bv_length(a6)
        jmp     LA966(pc)

L959E   bra     L9686

L95A2   tst.b   d5
        beq     LA078
        bsr     L9760
        bsr     L9692
L95B0   bsr.s   L960C
        beq.s   L95DC
L95B4   movea.l bv_rtp(a6),a5
        move.l  bv_ntp(a6),-$10(a6,a5.l)
        move.l  bv_ntbas(a6),d1
        sub.l   d1,-$18(a6,a5.l)
        sub.l   d1,-$14(a6,a5.l)
        sub.l   d1,-$10(a6,a5.l)
        tst.l   d0
        bne.s   L95DA
        addq.w  #4,a7
        jmp     LA8C0(pc)

        moveq   #err.ok,d0
L95DA   rts

L95DC   addq.w  #2,a4
        bsr.s   L965C
        cmpi.b  #nam.b,d0
        bne.s   L95FE
        bsr.s   L9660
        bsr.s   L965C
        cmpi.w  #$8405,d1               ;token symbol '(' ?
        bne.s   L95FE
        addq.b  #1,0(a6,a2.l)
        subq.w  #4,a4
        jsr     L98E8(pc)
        bne.s   L95B4
        bsr.s   L965C
L95FE   cmpi.w  #$8404,d1               ;token symbol ',' ?
        beq.s   L95DC
        bsr.s   L959E
        beq.s   L95B0
        moveq   #-$15,d0
        bra.s   L95B4

L960C   jsr     LA60E(pc)
        bne.s   L9628
        bsr.s   L965C
        moveq   #err.ok,d0
        cmpi.w  #$811E,d1               ;token keyword 'REMark' ?
        beq.s   L960C
        cmpi.w  #$8118,d1               ;token keyword 'DATA' ?
        beq.s   L960C
        cmpi.w  #$811A,d1               ;token keyword 'LOCal' ?
        rts

L9628   moveq   #err.ef,d0
        rts

L962C   moveq   #0,d0
        move.w  2(a6,a4.l),d0
        lsl.l   #3,d0
        movea.l bv_ntbas(a6),a2
        adda.l  d0,a2
        move.w  0(a6,a2.l),d0
        move.w  0(a6,a3.l),0(a6,a2.l)
        move.w  d0,0(a6,a3.l)
        move.l  4(a6,a2.l),d0
        move.l  4(a6,a3.l),4(a6,a2.l)
        move.l  d0,4(a6,a3.l)
        addq.w  #8,a3
        addq.w  #4,a4
        rts

L965C   jmp     LA56C(pc)

L9660   movea.l bv_ntp(a6),a3
        move.b  #2,0(a6,a3.l)
        move.w  #-1,2(a6,a3.l)
        move.l  #-1,4(a6,a3.l)
        bsr.s   L962C
        move.b  -7(a6,a3.l),1(a6,a2.l)
        move.l  a3,bv_ntp(a6)
        rts

L9686   cmpi.w  #$840A,d1               ;token symbol LF ?
        beq.s   L9690
        cmpi.w  #$8402,d1               ;token symbol ':' ?
L9690   rts

L9692   bsr.s   L965C
        addq.w  #2,a4
        bsr.s   L965C
        addq.w  #2,a4
        bsr.s   L965C
        tst.w   d3
        blt.s   L96A6
        cmp.w   2(a6,a4.l),d3
        bne.s   L96B4
L96A6   addq.w  #4,a4
        bsr.s   L965C
        cmpi.w  #$8405,d1               ;token symbol '(' ?
        beq.s   L96C0
        bsr.s   L9686
        beq.s   L96BA
L96B4   addq.w  #4,a7
L96B6   moveq   #err.nf,d0
        rts

L96BA   movea.l -20(a6,a5.l),a3
        rts

L96C0   addq.w  #2,a4
L96C2   bsr.s   L965C
        cmpi.b  #nam.b,d0
        bne.s   L96DE
        cmpa.l  -20(a6,a5.l),a3
        blt.s   L96D8
        bsr.s   L9660
        move.l  a3,-20(a6,a5.l)
        bra.s   L96C2

L96D8   bsr     L962C
        bra.s   L96C2

L96DE   cmpi.w  #$8406,d1               ;token symbol ')' ?
        bne.s   L96C0
L96E4   rts

L96E6   movea.l bv_rtp(a6),a5
        cmpa.l  bv_rtbas(a6),a5
        ble.s   L96B6
        move.b  -8(a6,a5.l),d5
        beq.s   L9718
        move.b  d5,d0
        subq.b  #2,d0
        blt.s   L9714
        move.b  -10(a6,a5.l),d0
        movea.l a4,a0
        jsr     L5A84(pc)
        movea.l a0,a4
        blt.s   L96E4
        beq.s   L9710
        moveq   #err.xp,d0
        rts

L9710   movea.l bv_rtp(a6),a5
L9714   bsr.s   L9768
        bne.s   L96E4
L9718   move.l  -4(a6,a5.l),bv_inlin(a6)
        move.w  -6(a6,a5.l),d4
        bsr.s   L9760
        move.l  -4(a6,a5.l),bv_inlin(a6)
        move.b  -7(a6,a5.l),d4
        jsr     LA00A(pc)
        subq.w  #8,a5
        tst.b   d5
        beq.s   L973C
        suba.w  #16,a5
L973C   sf      bv_unrvl(a6)
        move.l  a5,bv_rtp(a6)
        moveq   #err.ok,d0
        subq.b  #2,d5
        blt.s   L975C
        movea.l bv_ntp(a6),a5
        move.b  #1,-8(a6,a5.l)
        addq.w  #8,a7
        movea.l (a7)+,a0
        adda.l  bv_tkbas(a6),a0
L975C   tst.l   d0
        rts

L9760   jsr     L9FA2(pc)
        jmp     LA966(pc)

L9768   move.l  bv_ntbas(a6),d0
        add.l   d0,-24(a6,a5.l)
        add.l   d0,-20(a6,a5.l)
        add.l   d0,-16(a6,a5.l)
        cmpa.l  bv_pfbas(a6),a4
        ble.s   L9784
        cmpa.l  bv_pfp(a6),a4
        blt.s   L9788
L9784   bsr     L958E
L9788   tst.b   -9(a6,a5.l)
        beq.s   L97C8
        move.w  -12(a6,a5.l),d4
        movea.l -20(a6,a5.l),a3
        bsr.s   L9760
L9798   bsr     L960C
        bne.s   L97B0
L979E   move.w  #$8800,d4               ;token nametable entry
        jsr     LA5E0(pc)
        bne.s   L9798
        bsr     L962C
        subq.w  #4,a4
        bra.s   L979E

L97B0   move.w  -12(a6,a5.l),d4
        movea.l -24(a6,a5.l),a3
        tst.b   d0
        beq.s   L97C0
        bsr     L958E
L97C0   bsr.s   L9760
        moveq   #-1,d3
        bsr     L9692
L97C8   movea.l -24(a6,a5.l),a3
        movea.l -16(a6,a5.l),a5
        jsr     L5702(pc)
        bne.s   L975C
        movea.l bv_rtp(a6),a5
        rts

L97DC   bsr.s   L97E0
        rts

L97E0   movea.l bv_chbas(a6),a0
        movea.l 0(a6,a0.l),a0
        move.l  d0,-(a7)
        moveq   #err.pf,d0
        jsr     L3968(pc)               ;ut_err
        move.l  (a7)+,d0
L97F2   movea.l bv_rtp(a6),a5
        cmpa.l  bv_rtbas(a6),a5
        ble.s   L9828
        move.b  -8(a6,a5.l),d5
        beq.s   L980A
        bsr     L9768
        suba.w  #16,a5
L980A   subq.w  #8,a5
        move.l  a5,bv_rtp(a6)
        subq.b  #2,d5
        blt.s   L97F2
        movea.l bv_ntp(a6),a5
        subq.w  #8,a5
L981A   moveq   #0,d2
        jsr     L5CBC(pc)
        bne.s   L981A
        move.l  a5,bv_ntp(a6)
        bra.s   L97F2

L9828   sf      bv_unrvl(a6)
        movea.l (a7)+,a3
        movea.l (a7)+,a5
        trap    #0
        movea.l bv_endpt(a6),a1
        adda.l  a6,a1
        subq.w  #4,a1
        move    a1,usp
        move.w  #$0000,sr               ;user mode, interrupts on
        move.l  a5,-(a7)
        move.l  a3,-(a7)
        st      bv_sing(a6)
        tst.b   bv_undo(a6)
        bne     LA6EE
        rts

L9852   move.l  a4,-(a7)
        jsr     L9B32(pc)
        blt.s   L9872
L985A   addq.w  #2,a4
        jsr     LA56C(pc)
        move.l  a4,-(a7)
        jsr     L9B32(pc)
        cmpa.l  (a7)+,a4
        bne.s   L9870
        tst.b   d0
        bge.s   L985A
        bra.s   L9872

L9870   moveq   #err.ok,d0
L9872   movea.l (a7)+,a4
        rts

L9876   moveq   #0,d6
        move.l  a4,d3
L987A   move.w  0(a6,a4.l),d1
        cmpi.w  #$810A,d1               ;token keyword 'GO' ?
        beq.s   L98A0
        jsr     L9686(pc)
        beq.s   L98A2
        cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   L989A
        tst.b   d6
        bne.s   L989A
        addq.w  #2,a4
        move.l  a4,d3
        moveq   #1,d6
L989A   jsr     L9072(pc)
        bra.s   L987A

L98A0   moveq   #err.nc,d0
L98A2   movea.l d3,a4
        rts

L98A6   moveq   #$07,d2
L98A8   jsr     LA60E(pc)
        bne.s   L98C2
        jsr     LA56C(pc)
        cmpi.w  #$8101,d1               ;token keyword 'END' ?
        bne.s   L98A8
        addq.w  #2,a4
        jsr     LA56C(pc)
        cmp.b   d2,d1
        bne.s   L98A8
L98C2   moveq   #err.ok,d0
        rts

L98C6   movem.l d4-d6/a5,-(a7)
        subq.w  #2,a4
L98CC   addq.w  #2,a4
        jsr     LA56C(pc)
        bsr.s   L98E8
        bne.s   L98E2
        jsr     LA56C(pc)
        cmpi.w  #$8404,d1               ;token symbol ',' ?
        beq.s   L98CC
        moveq   #err.ok,d0
L98E2   movem.l (a7)+,d4-d6/a5
        rts

L98E8   moveq   #0,d4
        move.w  2(a6,a4.l),d4
        addq.w  #4,a4
        jsr     LA56C(pc)
        addq.w  #2,a4
        moveq   #0,d5
        movea.l (a6),a5                 ;bv_bfbas
        movea.l a4,a0
L98FC   move.l  a5,-(a7)
        jsr     L5A82(pc)
        movea.l (a7)+,a5
        movea.l a0,a4
        bne.s   L9980
        addq.l  #1,d5
        move.w  0(a6,a1.l),0(a6,a5.l)
        blt.s   L997A
        addq.w  #4,a5
        addq.l  #2,bv_rip(a6)
        addq.w  #2,a0
        cmpi.w  #$8404,-2(a6,a0.l)      ;token symbol ',' ?
        beq.s   L98FC
        cmpi.w  #$8406,-2(a6,a0.l)      ;token symbol ')' ?
        bne.s   L997E
        lsl.l   #3,d4
        movea.l bv_ntbas(a6),a3
        adda.l  d4,a3
        cmpi.b  #3,0(a6,a3.l)
        bne.s   L9982
        move.b  1(a6,a3.l),d6
        cmpi.b  #1,d6
        bne.s   L9958
        move.w  -4(a6,a5.l),d1
        addq.w  #1,d1
        bclr    #0,d1
        move.w  d1,-4(a6,a5.l)
        ble.s   L997A
        moveq   #2,d1
        bra.s   L995A

L9958   moveq   #1,d1
L995A   move.l  d5,d0
        moveq   #1,d2
L995E   subq.w  #4,a5
        move.w  d2,2(a6,a5.l)
        subq.l  #1,d0
        beq.s   L9986
        move.l  d1,d3
        moveq   #1,d1
        add.w   0(a6,a5.l),d3
        mulu.w  d3,d2
        move.l  d2,d3
        swap    d3
        tst.w   d3
        beq.s   L995E
L997A   moveq   #err.or,d0
        rts

L997E   moveq   #err.xp,d0
L9980   rts

L9982   moveq   #err.bn,d0
        rts

L9986   movea.l a0,a4
        move.l  4(a6,a3.l),d4
        blt.s   L9990
        bsr.s   L99FE
L9990   move.l  d5,d1
        lsl.l   #2,d1
        addq.l  #6,d1
        move.l  a3,-(a7)
        jsr     L4DF6(pc)
        movea.l (a7)+,a3
        move.l  a0,d1
        sub.l   bv_vvbas(a6),d1
        move.l  d1,4(a6,a3.l)
        move.l  a0,-(a7)
        move.w  d5,4(a6,a0.l)
        addq.w  #6,a0
L99B0   move.l  0(a6,a5.l),0(a6,a0.l)
        addq.w  #4,a5
        addq.w  #4,a0
        subq.l  #1,d5
        bgt.s   L99B0
        movea.l (a6),a5                 ;bv_bfbas
        moveq   #1,d1
        add.w   0(a6,a5.l),d1
        mulu.w  2(a6,a5.l),d1
        subq.b  #2,d6
        blt.s   L99DC
        beq.s   L99D4
        add.l   d1,d1
        bra.s   L99DC

L99D4   add.l   d1,d1
        move.l  d1,d0
        add.l   d1,d1
        add.l   d0,d1
L99DC   move.l  a3,-(a7)
        jsr     L4DF6(pc)
        movea.l (a7)+,a3
        move.l  a0,d2
        sub.l   bv_vvbas(a6),d2
        movea.l (a7)+,a2
        move.l  d2,0(a6,a2.l)
L99F0   clr.w   0(a6,a0.l)
        addq.w  #2,a0
        subq.l  #2,d1
        bgt.s   L99F0
        moveq   #err.ok,d0
        rts

L99FE   movea.l bv_vvbas(a6),a2
        adda.l  d4,a2
        moveq   #1,d2
        add.w   6(a6,a2.l),d2
        mulu.w  8(a6,a2.l),d2
        move.b  d6,d1
        subq.b  #2,d1
        blt.s   L9A22
        beq.s   L9A1A
        add.l   d2,d2
        bra.s   L9A22

L9A1A   add.l   d2,d2
        move.l  d2,d1
        add.l   d2,d2
        add.l   d1,d2
L9A22   move.l  d2,d1
        movea.l bv_vvbas(a6),a0
        adda.l  0(a6,a2.l),a0
        move.l  a3,-(a7)
        move.l  a2,-(a7)
        jsr     L4FE8(pc)
        movea.l (a7)+,a2
        bsr.s   L9A3C
        movea.l (a7)+,a3
        rts

L9A3C   moveq   #0,d1
        move.w  4(a6,a2.l),d1
        lsl.l   #2,d1
        addq.l  #6,d1
        movea.l a2,a0
        jmp     L4FE8(pc)

L9A4C   moveq   #0,d4
L9A4E   jsr     L9B32(pc)
        bge.s   L9A5A
        tst.b   bv_inlin(a6)
        bne.s   L9AA4
L9A5A   jsr     LA60E(pc)
        bne.s   L9AA4
        jsr     LA56C(pc)
        cmpi.b  #key.b,d0
        bne.s   L9A4E
        addq.w  #2,a4
        cmpi.b  #$03,d1
        bne.s   L9A8C
        jsr     L9852(pc)
        blt.s   L9A88
        tst.b   bv_inlin(a6)
        bne.s   L9A88
L9A7E   jsr     L9B32(pc)
        blt.s   L9A5A
        addq.w  #2,a4
        bra.s   L9A7E

L9A88   addq.w  #1,d4
        bra.s   L9A4E

L9A8C   cmpi.b  #1,d1
        bne.s   L9A4E
        jsr     LA56C(pc)
        cmpi.w  #$8103,d1               ;token keyword 'IF' ?
        bne.s   L9A4E
        tst.b   d4
        beq.s   L9AA4
        subq.w  #1,d4
        bra.s   L9A4E

L9AA4   moveq   #err.ok,d0
        rts

L9AA8   jsr     LA56C(pc)
        cmpi.w  #$8107,d1               ;token keyword 'DEFine' ?
        beq     L96E6
        cmpi.w  #$8106,d1               ;token keyword 'WHEN' ?
        beq.s   L9AE6
        moveq   #6,d5
        cmpi.w  #$8104,d1               ;token keyword 'REPeat' ?
        beq.s   L9ACA
        moveq   #7,d5
        cmpi.w  #$8102,d1               ;token keyword 'FOR' ?
        bne.s   L9B28
L9ACA   addq.w  #2,a4
        jsr     LA09E(pc)
        bne.s   L9B2A
        cmp.b   d5,d1
        bne.s   L9B2E
        move.w  bv_linum(a6),8(a6,a2.l)
        move.b  bv_stmnt(a6),11(a6,a2.l)
        jmp     LA37A(pc)

L9AE6   tst.b   bv__C0(a6)
        bne     L7E50
L9AEE   move.w  bv_linum(a6),d4
        moveq   #-1,d3
        jsr     LACC2(pc)
        bne.s   L9B28
L9AFA   move.w  8(a6,a2.l),d4
        seq     bv_sing(a6)
        move.b  10(a6,a2.l),-(a7)
        move.b  11(a6,a2.l),bv_inlin(a6)
        move.w  12(a6,a2.l),bv_index(a6)
        st      8(a6,a2.l)
        jsr     L9FA2(pc)
        bne.s   L9B2C
        jsr     LA96A(pc)
        bne.s   L9B2C
        move.b  (a7)+,d4
        jsr     LA00A(pc)
L9B28   moveq   #err.ok,d0
L9B2A   rts

L9B2C   addq.w  #2,a7
L9B2E   moveq   #err.nf,d0
        rts

L9B32   moveq   #err.ok,d0
        move.w  0(a6,a4.l),d1
        cmpi.w  #$8402,d1               ;token symbol ':' ?
        beq.s   L9B5C
        cmpi.w  #$840A,d1               ;token symbol LF ?
        beq.s   L9B5A
        moveq   #1,d0
        cmpi.w  #$811C,d1               ;token keyword 'THEN' ?
        beq.s   L9B5C
        moveq   #2,d0
        cmpi.w  #$8114,d1               ;token keyword 'ELSE' ?
        beq.s   L9B5C
        jsr     L9072(pc)
        bra.s   L9B32

L9B5A   moveq   #err.nc,d0
L9B5C   rts

L9B5E   move.w  bv_linum(a6),bv_cnlno(a6)
        tst.b   bv_sing(a6)
        beq.s   L9B70
        move.w  #-1,bv_cnlno(a6)
L9B70   move.b  bv_stmnt(a6),bv_cnstm(a6)
        move.b  bv_inlin(a6),bv_cninl(a6)
        move.w  bv_index(a6),bv_cnind(a6)
        move.l  bv_rtp(a6),d1
        sub.l   bv_rtbas(a6),d1
        sne     bv_unrvl(a6)
        rts

L9B90   moveq   #err.nc,d0
L9B92   tst.b   bv__C0(a6)
        bne.s   L9BB6
        bsr.s   L9B5E
        bra.s   L9BD4

L9B9C   st      bv_sing(a6)
        cmpi.l  #err.bl,d0
        beq.s   L9B92
L9BA8   cmpi.l  #err.nc,d0
        beq.s   L9B92
        tst.b   bv__C0(a6)
        beq.s   L9BC0
L9BB6   bsr.s   L9BD4
        moveq   #err.dw,d0
        jsr     L3968(pc)               ;ut_err
        bra.s   L9C18

L9BC0   bsr.s   L9B5E
        move.w  bv_linum(a6),bv__C6(a6)
        move.l  d0,bv__C2(a6)
        bge.s   L9C18
        move.w  bv__BC(a6),d4
        bne.s   L9C1C
L9BD4   movea.l bv_chbas(a6),a0
        movea.l 0(a6,a0.l),a0
L9BDC   move.w  #-1,bv_nxlin(a6)
        tst.b   bv_sing(a6)
        bne.s   L9C14
        move.l  d0,-(a7)
        moveq   #err.at,d0
        jsr     L3968(pc)               ;ut_err
        movea.l a0,a5
        movea.w #bv_linum,a1
        movea.l (a6),a0                 ;bv_bfbas
        jsr     L3E54(pc)               ;cn_itod
        move.b  #' ',0(a6,a0.l)
        addq.w  #1,d1
        movea.l (a6),a1                 ;bv_bfbas
        move.w  d1,d2
        movea.l a5,a0
        moveq   #io.sstrg,d0
        moveq   #forever,d3
        trap    #4
        trap    #3
        move.l  (a7)+,d0
L9C14   jsr     L3968(pc)               ;ut_err
L9C18   moveq   #err.nc,d0
        rts

L9C1C   st      bv__C0(a6)
        move.l  d0,-(a7)
        tst.b   bv_sing(a6)
        beq.s   L9C2E
        jsr     L958E(pc)
        bne.s   L9C56
L9C2E   jsr     L9FA2(pc)
        jsr     LA966(pc)
        bne.s   L9C56
        jsr     LA56C(pc)
        cmpi.w  #$8106,d1               ;token keyword 'WHEN' ?
        bne.s   L9C56
        addq.w  #4,a7
        move.b  bv__BF(a6),bv_inlin(a6)
        move.b  bv__BE(a6),d4
        jsr     LA00A(pc)
        jmp     LA90C(pc)

L9C56   clr.w   bv__BC(a6)
        sf      bv__C0(a6)
        move.l  (a7)+,d0
        bra     L9BD4

L9C64   jsr     LA09E(pc)
        bne     L9CFC
        move.w  8(a6,a2.l),d4
        beq.s   L9C88
        jsr     L9FA2(pc)
        bne.s   L9C86
        jsr     LA966(pc)
        bne.s   L9C86
        move.b  11(a6,a2.l),d4
        jsr     LA00A(pc)
L9C86   bra.s   L9CFA

L9C88   move.w  -2(a6,a4.l),d4
        moveq   #7,d5
        sub.b   d1,d5
L9C90   jsr     L9B32(pc)
        blt.s   L9C9E
        addq.w  #2,a4
        addq.b  #1,bv_stmnt(a6)
        bra.s   L9CC0

L9C9E   tst.b   bv_inlin(a6)
        beq.s   L9CB2
        sf      bv_inlin(a6)
        move.w  bv_linum(a6),d0
        cmp.w   6(a6,a2.l),d0
        beq.s   L9CFA
L9CB2   tst.b   bv_sing(a6)
        bne.s   L9CFA
        addq.w  #2,a4
        jsr     LA966(pc)
        bne.s   L9CFA
L9CC0   jsr     LA56C(pc)
        cmpi.w  #$8101,d1               ;token keyword 'END' ?
        bne.s   L9C90
        addq.w  #2,a4
        jsr     LA56C(pc)
        tst.b   d5
        beq.s   L9CDC
        cmpi.w  #$8104,d1               ;token keyword 'REPeat' ?
        bne.s   L9C90
        bra.s   L9CE2

L9CDC   cmpi.w  #$8102,d1               ;token keyword 'FOR' ?
        bne.s   L9C90
L9CE2   addq.w  #2,a4
        jsr     LA56C(pc)
        cmp.w   2(a6,a4.l),d4
        bne.s   L9C90
        move.w  bv_linum(a6),8(a6,a2.l)
        move.b  bv_stmnt(a6),11(a6,a2.l)
L9CFA   moveq   #err.ok,d0
L9CFC   rts

L9CFE   moveq   #0,d5
L9D00   bsr.s   L9D46
        bne.s   L9D56
        jsr     LA56C(pc)
        cmpi.b  #key.b,d0
        bne.s   L9D00
        addq.w  #2,a4
        cmpi.b  #1,d1
        bne.s   L9D26
        jsr     LA56C(pc)
        cmpi.w  #$8105,d1               ;token keyword 'SELect' ?
        bne.s   L9D00
        subq.w  #1,d5
        blt.s   L9D56
        bra.s   L9D00

L9D26   cmpi.b  #5,d1
        bne.s   L9D00
        jsr     L9852(pc)
        blt.s   L9D38
        tst.b   bv_inlin(a6)
        beq.s   L9D3C
L9D38   addq.w  #1,d5
        bra.s   L9D00

L9D3C   jsr     L9B32(pc)
        blt.s   L9D00
        addq.w  #2,a4
        bra.s   L9D3C

L9D46   jsr     LA60E(pc)
        bne.s   L9D64
        tst.b   d0
        beq.s   L9D56
        tst.b   bv_inlin(a6)
        bne.s   L9D5A
L9D56   moveq   #err.ok,d0
        rts

L9D5A   subq.w  #8,a4
        move.w  2(a6,a4.l),d0
        sub.w   d0,bv_length(a6)
L9D64   tst.b   bv_inlin(a6)
        bgt.s   L9D6E
        sf      bv_inlin(a6)
L9D6E   moveq   #1,d0
        rts

L9D72   moveq   #0,d5
L9D74   jsr     L9D46(pc)
        bne.s   L9DEC
        jsr     LA56C(pc)
        cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   L9D8C
        addq.w  #2,a4
        tst.b   d5
        beq.s   L9DEE
        bra.s   L9D74

L9D8C   cmpi.b  #key.b,d0
        bne.s   L9D74
        cmpi.b  #$15,d1
        bne.s   L9DB6
        tst.b   d5
        bne.s   L9D74
        jsr     L9876(pc)
        blt.s   L9D74
        movea.l a4,a3
L9DA4   subq.w  #2,a3
        cmpi.w  #$8800,0(a6,a3.l)       ;token nametable entry
        bne.s   L9DA4
        cmp.w   2(a6,a3.l),d4
        bne.s   L9D74
        bra.s   L9DEE

L9DB6   cmpi.b  #5,d1
        bne.s   L9DD6
        jsr     L9852(pc)
        blt.s   L9DC8
        tst.b   bv_inlin(a6)
        beq.s   L9DCC
L9DC8   addq.w  #1,d5
        bra.s   L9D74

L9DCC   jsr     L9B32(pc)
        blt.s   L9D74
        addq.w  #2,a4
        bra.s   L9DCC

L9DD6   cmpi.b  #1,d1
        bne.s   L9D74
        addq.w  #2,a4
        jsr     LA56C(pc)
        cmpi.w  #$8105,d1               ;token keyword 'SELect' ?
        bne.s   L9D74
        subq.w  #1,d5
        bge.s   L9D74
L9DEC   rts

L9DEE   moveq   #err.ok,d0
        rts

L9DF2   movea.l a4,a5
        jsr     LA56C(pc)
        moveq   #0,d4
        move.w  2(a6,a4.l),d4
        addq.w  #4,a4
        jsr     L9852(pc)
        blt.s   L9E10
        move.b  #1,bv_inlin(a6)
        move.w  d4,bv_index(a6)
L9E10   jsr     L9F96(pc)
        move.b  1(a6,a2.l),d2
        subq.b  #2,d2
        bne.s   L9E30
        move.b  0(a6,a2.l),d2
        moveq   #6,d1
        subq.b  #2,d2
        beq.s   L9E34
        moveq   #12,d1
        subq.b  #4,d2
        beq.s   L9E34
        subq.b  #1,d2
        beq.s   L9E5C
L9E30   moveq   #err.bn,d0
        rts

L9E34   move.l  a2,-(a7)
        move.l  4(a6,a2.l),d2
        blt.s   L9E46
        movea.l bv_vvbas(a6),a0
        adda.l  d2,a0
        jsr     L4FE8(pc)
L9E46   moveq   #26,d1
        jsr     L4DF6(pc)
        suba.l  bv_vvbas(a6),a0
        movea.l (a7)+,a2
        move.b  #7,0(a6,a2.l)
        move.l  a0,4(a6,a2.l)
L9E5C   jsr     LA2FE(pc)
        jsr     LA56C(pc)
        move.l  a4,d0
        sub.l   a5,d0
        move.l  d7,12(a6,a2.l)
        move.l  d7,16(a6,a2.l)
        move.l  d7,20(a6,a2.l)
        move.w  d0,24(a6,a2.l)
        jsr     L9E88(pc)
        bgt.s   L9E80
        rts

L9E80   moveq   #0,d5
        jmp     L9C90(pc)

L9E86   dc.b    1
L9E87   dc.b    64

L9E88   move.l  a4,-(a7)
        move.w  0(a6,a4.l),d1
        jsr     L9686(pc)
        bne.s   L9E98
        moveq   #1,d0
        bra.s   L9EA6

L9E98   cmpi.w  #$8401,d1               ;token symbol '=' ?
        beq.s   L9EAA
        cmpi.w  #$8404,d1               ;token symbol ',' ?
        beq.s   L9EAA
        moveq   #err.or,d0
L9EA6   addq.w  #4,a7
        rts

L9EAA   addq.w  #2,a4
        movea.l a4,a0
        move.l  d4,d6
        jsr     L5A7E(pc)
        movea.l a0,a4
        bne.s   L9EA6
        bsr     L9F7E
        move.w  0(a6,a1.l),0(a6,a2.l)
        move.l  2(a6,a1.l),2(a6,a2.l)
        addq.l  #6,bv_rip(a6)
        cmpi.w  #$810B,0(a6,a4.l)       ;token keyword 'TO' ?
        beq.s   L9EF4
        move.w  0(a6,a2.l),12(a6,a2.l)
        move.l  2(a6,a2.l),14(a6,a2.l)
        move.w  d7,$12(a6,a2.l)
        move.l  d7,$14(a6,a2.l)
        move.l  a4,d0
        sub.l   (a7)+,d0
        add.w   d0,24(a6,a2.l)
        bra     L9F76

L9EF4   addq.w  #2,a4
        movea.l a4,a0
        jsr     L5A7E(pc)
        movea.l a0,a4
L9EFE   bne.s   L9EA6
        bsr.s   L9F7E
        move.w  0(a6,a1.l),12(a6,a2.l)
        move.l  2(a6,a1.l),14(a6,a2.l)
        addq.l  #6,bv_rip(a6)
        cmpi.w  #$811D,0(a6,a4.l)       ;token keyword 'STEP' ?
        beq.s   L9F32
        move.b  #8,$12(a6,a2.l)
        move.b  L9E86(pc),19(a6,a2.l)
        move.l  d7,$14(a6,a2.l)
        move.b  L9E87(pc),20(a6,a2.l)
        bra.s   L9F50

L9F32   addq.w  #2,a4
        movea.l a4,a0
        jsr     L5A7E(pc)
        movea.l a0,a4
        bne.s   L9EFE
        bsr.s   L9F7E
        move.w  0(a6,a1.l),18(a6,a2.l)
        move.l  2(a6,a1.l),20(a6,a2.l)
        addq.l  #6,bv_rip(a6)
L9F50   move.l  a4,d0
        sub.l   (a7)+,d0
        add.w   d0,24(a6,a2.l)
        jsr     L4E4C(pc)
        movea.l bv_rip(a6),a1
        subq.w  #6,a1
        move.l  2(a6,a2.l),2(a6,a1.l)
        move.w  0(a6,a2.l),0(a6,a1.l)
        jsr     LA404(pc)
        bra.s   L9F78
        bra.s   L9F7A
L9F76   moveq   #err.ok,d0
L9F78   rts

L9F7A   bra     L9E88

L9F7E   move.l  d6,d4
L9F80   bsr.s   L9F96
        move.b  0(a6,a2.l),d1
        move.b  1(a6,a2.l),d2
        move.l  4(a6,a2.l),d0
        movea.l bv_vvbas(a6),a2
        adda.l  d0,a2
        rts

L9F96   move.l  d4,d0
        movea.l bv_ntbas(a6),a2
        lsl.l   #3,d0
        adda.w  d0,a2
        rts

L9FA2   tst.b   bv_sing(a6)
        beq.s   L9FAE
        movea.l bv_tkbas(a6),a4
        bra.s   LA006

L9FAE   cmpi.w  #$840A,0(a6,a4.l)       ;token symbol LF ?
        beq.s   L9FBC
        jsr     L9072(pc)
        bra.s   L9FAE

L9FBC   addq.w  #2,a4
L9FBE   moveq   #0,d1
        moveq   #0,d2
        cmp.w   bv_linum(a6),d4
        ble.s   L9FEC
L9FC8   cmpa.l  bv_pfp(a6),a4
        blt.s   L9FD2
        jmp     LA994(pc)

L9FD2   cmp.w   4(a6,a4.l),d4
        ble.s   LA006
L9FD8   move.w  4(a6,a4.l),d2
        move.w  0(a6,a4.l),d1
        addq.w  #2,a4
        add.w   d1,bv_length(a6)
        adda.w  bv_length(a6),a4
        bra.s   L9FC8

L9FEC   suba.w  bv_length(a6),a4
        subq.w  #2,a4
        move.w  0(a6,a4.l),d1
        sub.w   d1,bv_length(a6)
        cmp.w   4(a6,a4.l),d4
        bgt.s   L9FD8
        cmpa.l  bv_pfbas(a6),a4
        bgt.s   L9FEC
LA006   moveq   #err.ok,d0
        rts

LA00A   cmp.b   bv_stmnt(a6),d4
        ble.s   LA01E
        jsr     L9B32(pc)
        blt.s   LA01E
        addq.w  #2,a4
        addq.b  #1,bv_stmnt(a6)
        bra.s   LA00A

LA01E   moveq   #err.ok,d0
        rts

LA022   moveq   #1,d4
        bra.s   LA038

LA026   jsr     L9876(pc)
        bge     L9CFE
        bsr.s   LA086
        move.w  0(a6,a1.l),d4
        ble.s   LA082
        addq.w  #2,a4
LA038   jsr     LA56C(pc)
        cmpi.b  #11,d1
        seq     d5
LA042   move.w  0(a6,a4.l),d1
        jsr     L9686(pc)
        beq.s   LA082
        addq.w  #2,a4
        bsr.s   LA086
        bgt.s   LA042
        subq.w  #1,d4
        bne.s   LA042
        move.w  0(a6,a1.l),d4
        tst.b   d5
        beq     L94CA
        sf      bv_inlin(a6)
        tst.b   bv_sing(a6)
        beq.s   LA078
        sf      bv_sing(a6)
        movea.l bv_pfbas(a6),a4
        jsr     LA966(pc)
        bne.s   LA07E
LA078   jsr     L9FA2(pc)
        subq.w  #2,a4
LA07E   moveq   #err.ok,d0
        rts

LA082   moveq   #err.or,d0
        rts

LA086   movea.l a4,a0
        jsr     L5A82(pc)
        movea.l a0,a4
        blt.s   LA09A
        bgt.s   LA098
        addq.l  #2,bv_rip(a6)
        moveq   #err.ok,d0
LA098   rts

LA09A   addq.w  #4,a7
        rts

LA09E   jsr     LA56C(pc)
        moveq   #0,d4
        move.w  2(a6,a4.l),d4
        addq.w  #4,a4
LA0AA   jsr     L9F80(pc)
        moveq   #err.ok,d0
        move.b  d1,d0
        subq.b  #6,d0
        beq.s   LA0BC
        subq.b  #1,d0
        beq.s   LA0BC
        moveq   #err.nf,d0
LA0BC   rts

LA0BE   movea.l a4,a0
        jsr     L5A7E(pc)
        movea.l a0,a4
        bne     LA150
        jsr     L9852(pc)
        blt.s   LA0DA
        tst.b   bv_inlin(a6)
        bne.s   LA0DA
        st      bv_inlin(a6)
LA0DA   addq.l  #6,bv_rip(a6)
        tst.w   0(a6,a1.l)
        bne.s   LA14E
        moveq   #0,d4
LA0E6   jsr     L9B32(pc)
        bge.s   LA0F2
        tst.b   bv_inlin(a6)
        bne.s   LA14E
LA0F2   jsr     LA60E(pc)
        bne.s   LA14E
        jsr     LA56C(pc)
        cmpi.b  #key.b,d0
        bne.s   LA0E6
        addq.w  #2,a4
        cmpi.b  #3,d1
        bne.s   LA124
        jsr     L9852(pc)
        blt.s   LA120
        tst.b   bv_inlin(a6)
        bne.s   LA120
LA116   jsr     L9B32(pc)
        blt.s   LA0F2
        addq.w  #2,a4
        bra.s   LA116

LA120   addq.w  #1,d4
        bra.s   LA0E6

LA124   cmpi.b  #1,d1
        bne.s   LA13C
        jsr     LA56C(pc)
        cmpi.w  #$8103,d1               ;token keyword 'IF' ?
        bne.s   LA0E6
        tst.b   d4
        beq.s   LA14E
        subq.w  #1,d4
        bra.s   LA0E6

LA13C   cmpi.b  #20,d1
        bne.s   LA0E6
        tst.b   d4
        beq.s   LA14C
        addq.b  #1,bv_stmnt(a6)
        bra.s   LA0E6

LA14C   subq.w  #2,a4
LA14E   moveq   #err.ok,d0
LA150   rts

        dc.w    L9AA8-LA190   ;END
        dc.w    L9DF2-LA190   ;FOR
        dc.w    LA0BE-LA190   ;IF
        dc.w    LA7B6-LA190   ;REPEAT
        dc.w    LA84A-LA190   ;SELECT
        dc.w    LAC00-LA190   ;WHEN
        dc.w    L98A6-LA190   ;DEFINE
        dc.w    0             ;PROCEDURE
        dc.w    0             ;FUNCTION
        dc.w    LA022-LA190   ;GO
        dc.w    0             ;TO
        dc.w    0             ;SUB
        dc.w    0             ;WHEN
        dc.w    0             ;ERROR
        dc.w    0             ;EOF
        dc.w    0             ;INPUT
        dc.w    LA820-LA190   ;RESTORE
        dc.w    LA372-LA190   ;NEXT
        dc.w    L9C64-LA190   ;EXIT
        dc.w    L9A4C-LA190   ;ELSE
        dc.w    LA026-LA190   ;ON
        dc.w    L96E6-LA190   ;RETURN
        dc.w    0             ;REMAINDER
        dc.w    LA7B2-LA190   ;DATA
        dc.w    L98C6-LA190   ;DIM
        dc.w    LA2FA-LA190   ;LOCAL
        dc.w    LA332-LA190   ;LET 
        dc.w    0             ;THEN
        dc.w    0             ;STEP
        dc.w    LA7B2-LA190   ;REMARK
        dc.w    LA2FA-LA190   ;MISTAKE

LA190   moveq   #0,d0
        move.b  1(a6,a4.l),d0
        addq.w  #2,a4
        add.b   d0,d0
        move.w  LA150(pc,d0.w),d0
        beq.s   LA1A4
        jmp     LA190(pc,d0.w)

LA1A4   moveq   #err.nf,d0
        rts

LA1A8   movea.l a2,a3
        andi.b  #$0F,1(a6,a3.l)
        move.l  a3,-(a7)
        move.b  1(a6,a3.l),-(a7)
        jsr     L7A8E(pc)
        bne.s   LA1E8
        cmpi.w  #$8405,d1               ;token symbol '(' ?
        bne.s   LA1F6
        cmpi.b  #1,1(a6,a3.l)
        bne.s   LA242
        move.l  4(a6,a3.l),d0
        blt.s   LA242
        movea.l bv_vvbas(a6),a2
        adda.l  d0,a2
        move.l  d0,-(a7)
        lea     2(a4),a0
        jsr     L6446(pc)
        movea.l a0,a4
        movea.l bv_vvbas(a6),a2
        adda.l  (a7)+,a2
LA1E8   bne.s   LA244
        jsr     LA56C(pc)
        cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   LA242
        sf      (a7)
LA1F6   cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   LA242
        addq.w  #2,a4
        move.b  (a7),d0
        movea.l a4,a0
        suba.l  bv_vvbas(a6),a2
        move.l  a2,-(a7)
        jsr     L5A84(pc)
        movea.l bv_vvbas(a6),a2
        adda.l  (a7)+,a2
        movea.l a0,a4
        bne.s   LA23C
        move.b  (a7)+,d0
        movea.l (a7)+,a3
        move.b  d0,1(a6,a3.l)
        jsr     L72C2(pc)               ;bp_let
        bne.s   LA23A
        tst.w   bv__C8(a6)
        beq.s   LA23A
        move.b  0(a6,a3.l),d1
        subq.b  #2,d1
        beq.s   LA248
        subq.b  #4,d1
        beq.s   LA248
        subq.b  #1,d1
        beq.s   LA248
LA23A   rts

LA23C   blt.s   LA244
        moveq   #err.xp,d0
        bra.s   LA244

LA242   moveq   #err.bn,d0
LA244   addq.w  #6,a7
        rts

LA248   move.l  a3,d4
        sub.l   bv_ntbas(a6),d4
        lsr.l   #3,d4
        moveq   #1,d3
        jsr     LACC2(pc)
LA256   bne     LA2DA
        tst.w   8(a6,a2.l)
        blt.s   LA268
LA260   moveq   #1,d3
        jsr     LACDA(pc)
        bra.s   LA256

LA268   move.w  bv_linum(a6),8(a6,a2.l)
        move.b  bv_stmnt(a6),10(a6,a2.l)
        move.b  bv_inlin(a6),11(a6,a2.l)
        move.w  bv_index(a6),12(a6,a2.l)
        move.w  d4,-(a7)
        move.w  d2,-(a7)
        move.l  d1,-(a7)
        move.w  2(a6,a2.l),d4
        move.b  4(a6,a2.l),d1
        suba.l  bv_vvbas(a6),a2
        move.l  a2,-(a7)
        move.b  d1,-(a7)
        tst.b   bv_sing(a6)
        beq.s   LA2A0
        jsr     L958E(pc)
LA2A0   jsr     L9FA2(pc)
        bne.s   LA2F2
        jsr     LA96A(pc)
        bne.s   LA2F2
        move.b  (a7)+,d4
        jsr     LA00A(pc)
        jsr     LA56C(pc)
        lea     2(a4),a0
        jsr     L5A7E(pc)
        movea.l a0,a4
        movea.l (a7)+,a2
        bne.s   LA2F6
        adda.l  bv_vvbas(a6),a2
        addq.l  #6,bv_rip(a6)
        tst.w   0(a6,a1.l)
        beq.s   LA2DE
        addq.w  #8,a7
        move.b  14(a6,a2.l),bv_inlin(a6)
LA2DA   moveq   #err.ok,d0
        rts

LA2DE   move.l  a2,-(a7)
        jsr     L9AFA(pc)
        movea.l (a7)+,a2
        bne.s   LA2F6
        move.l  (a7)+,d1
        move.w  (a7)+,d2
        move.w  (a7)+,d4
        bra     LA260

LA2F2   moveq   #err.nf,d0
        addq.w  #6,a7
LA2F6   addq.w  #8,a7
        rts

LA2FA   moveq   #err.bl,d0
        rts

LA2FE   movea.l 4(a6,a2.l),a2
        adda.l  bv_vvbas(a6),a2
        move.w  d7,0(a6,a2.l)
        move.l  d7,2(a6,a2.l)
        move.w  bv_linum(a6),d1
        move.b  bv_stmnt(a6),d0
        cmp.w   6(a6,a2.l),d1
        bne.s   LA322
        cmp.b   10(a6,a2.l),d0
        beq.s   LA32E
LA322   move.w  d1,6(a6,a2.l)
        move.l  d7,8(a6,a2.l)
        move.b  d0,10(a6,a2.l)
LA32E   moveq   #err.ok,d0
        rts

LA332   jsr     LA56C(pc)
        cmpi.b  #nam.b,d0
        bne.s   LA36E
LA33C   moveq   #0,d4
        move.w  2(a6,a4.l),d4
        addq.w  #4,a4
        jsr     LA56C(pc)
        jsr     L9F96(pc)
        move.b  0(a6,a2.l),d0
        cmpi.b  #3,d0
        beq     L93E6
        cmpi.b  #8,d0
        beq     LA6F6
        moveq   #1,d5
        cmpi.b  #4,d0
        beq     L94CA
        jmp     LA1A8(pc)

LA36E   moveq   #err.ni,d0
        rts

LA372   jsr     LA09E(pc)
        blt     LA402
LA37A   move.l  d4,d6
        cmpi.b  #6,d1
        beq.s   LA3D4
        move.l  a2,-(a7)
        jsr     L4E4C(pc)
        movea.l (a7),a2
        movea.l bv_rip(a6),a1
        suba.w  #12,a1
        move.l  20(a6,a2.l),2(a6,a1.l)
        beq.s   LA3DE
        move.w  18(a6,a2.l),0(a6,a1.l)
        move.w  0(a6,a2.l),6(a6,a1.l)
        move.l  2(a6,a2.l),8(a6,a1.l)
        jsr     L4838(pc)               ;ri.add
        blt.s   LA3D6
        move.l  a1,bv_rip(a6)
        jsr     L4A4A(pc)               ;ri.dup
        movea.l (a7),a2
        bsr.s   LA404
        bra.s   LA3D6
        bra.s   LA3DA
        movea.l (a7)+,a2
        move.w  0(a6,a0.l),0(a6,a2.l)
        move.l  2(a6,a0.l),2(a6,a2.l)
        addq.l  #6,bv_rip(a6)
LA3D4   bra.s   LA3E4

LA3D6   addq.w  #4,a7
        rts

LA3DA   addq.l  #6,bv_rip(a6)
LA3DE   movea.l (a7)+,a2
        moveq   #1,d3
        bra.s   LA3E6

LA3E4   moveq   #0,d3
LA3E6   bsr.s   LA44C
        bne.s   LA402
        tst.b   bv_inlin(a6)
        bgt.s   LA3F4
        sf      bv_inlin(a6)
LA3F4   cmpi.b  #127,bv_stmnt(a6)
        bne.s   LA400
        sf      bv_inlin(a6)
LA400   moveq   #err.ok,d0
LA402   rts

LA404   move.l  a2,-(a7)
        subq.w  #6,a1
        move.l  14(a6,a2.l),2(a6,a1.l)
        move.w  12(a6,a2.l),0(a6,a1.l)
        jsr     L482A(pc)               ;ri.sub
        movea.l (a7)+,a2
        blt.s   LA44A
        moveq   #13,d1
        add.w   0(a6,a1.l),d1
        cmp.w   18(a6,a2.l),d1
        bgt.s   LA42E
        lea     12(a2),a0
        bra.s   LA446

LA42E   movea.l bv_rip(a6),a0
        tst.b   2(a6,a1.l)
        blt.s   LA440
        tst.b   20(a6,a2.l)
        blt.s   LA446
        bra.s   LA448

LA440   tst.b   20(a6,a2.l)
        blt.s   LA448
LA446   addq.l  #2,(a7)
LA448   addq.l  #2,(a7)
LA44A   rts

LA44C   move.b  bv_stmnt(a6),-(a7)
        move.l  bv_linum(a6),-(a7)
        move.l  a4,-(a7)
        move.w  6(a6,a2.l),d4
        jsr     L9FA2(pc)
        bne.s   LA4AA
        jsr     LA96A(pc)
        bne.s   LA4AA
        move.b  10(a6,a2.l),d4
        jsr     LA00A(pc)
        jsr     LA56C(pc)
        cmpi.w  #$8102,d1               ;token keyword 'FOR' ?
        beq.s   LA47E
        cmpi.w  #$8104,d1               ;token keyword 'REPeat ?
        bne.s   LA4AA
LA47E   addq.w  #2,a4
        movea.l a4,a5
        jsr     LA56C(pc)
        cmp.w   2(a6,a4.l),d6
        bne.s   LA4AA
        tst.b   d3
        beq.s   LA4A4
        movea.l a5,a4
        adda.w  24(a6,a2.l),a4
        move.l  d6,d4
        jsr     L9E88(pc)
        blt.s   LA4AC
        beq.s   LA4A4
        moveq   #err.ok,d0
        bra.s   LA4AC

LA4A4   adda.w  #10,a7
        rts

LA4AA   moveq   #err.nf,d0
LA4AC   movea.l (a7)+,a4
        move.l  (a7)+,bv_linum(a6)
        move.b  (a7)+,bv_stmnt(a6)
        tst.l   d0
        rts

LA4BA   move.l  a4,-(a7)
        move.b  bv_sing(a6),d6
        beq.s   LA4CC
        movea.l bv_tkbas(a6),a4
        moveq   #0,d5
        bra.s   LA4EE

LA4CA   sf      d6
LA4CC   tst.b   bv_edit(a6)
        sf      bv_edit(a6)
        beq.s   LA4EA
        movea.l bv_pfbas(a6),a4
LA4DA   tst.b   d6
        bne.s   LA4CA
        move.w  4(a6,a4.l),d5
        addq.w  #6,a4
        cmpa.l  bv_pfp(a6),a4
        blt.s   LA4EE
LA4EA   movea.l (a7)+,a4
        rts

LA4EE   bsr.s   LA56C
        cmpi.b  #nam.b,d0
        beq.s   LA526
        cmpi.b  #key.b,d0
        bne.s   LA55C
        addq.w  #2,a4
        cmpi.b  #20,d1
        beq.s   LA4EE
        cmpi.b  #2,d1
        bne.s   LA50E
        bsr.s   LA56C
        bra.s   LA534

LA50E   cmpi.b  #4,d1
        bne.s   LA51E
        bsr.s   LA56C
        cmpi.b  #nam.b,d0
        bne.s   LA55C
        bra.s   LA534

LA51E   cmpi.b  #27,d1
        bne.s   LA53A
        bsr.s   LA56C
LA526   move.l  a4,d4
        addq.w  #4,a4
        bsr.s   LA56C
        cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   LA55C
        movea.l d4,a4
LA534   moveq   #2,d1
LA536   bsr.s   LA57E
        bra.s   LA55C

LA53A   cmpi.b  #7,d1
        bne.s   LA554
        bsr.s   LA56C
        move.w  d1,d4
        addq.w  #2,a4
        bsr.s   LA56C
        moveq   #4,d1
        cmpi.b  #8,d4
        beq.s   LA536
        moveq   #5,d1
        bra.s   LA536

LA554   cmpi.b  #25,d1
        bne.s   LA55C
        bsr.s   LA5CE
LA55C   jsr     L9B32(pc)
        bge.s   LA568
        addq.w  #2,a4
        bra     LA4DA

LA568   addq.w  #2,a4
        bra.s   LA4EE

LA56C   move.w  0(a6,a4.l),d1
        move.b  0(a6,a4.l),d0
        cmpi.b  #spc.b,d0
        bne.s   LA5CC
        jmp     L9072(pc)

LA57E   moveq   #0,d0
        move.w  2(a6,a4.l),d0
        lsl.l   #3,d0
        movea.l bv_ntbas(a6),a0
        adda.l  d0,a0
        move.b  0(a6,a0.l),d0
        cmp.b   d0,d1
        beq.s   LA5BC
        subq.b  #2,d0
        blt.s   LA5BC
        beq.s   LA5AC
        subq.b  #1,d0
        beq.s   LA5CC
        subq.b  #2,d0
        ble.s   LA5A6
        subq.b  #3,d0
        blt.s   LA5AC
LA5A6   cmpi.b  #$04,d1
        bge.s   LA5BC
LA5AC   movea.l a0,a2
        movem.l d1/a0,-(a7)
        jsr     L5784(pc)
        movem.l (a7)+,d1/a0
        bne.s   LA5CC
LA5BC   move.b  d1,0(a6,a0.l)
        subq.b  #4,d1
        beq.s   LA5C8
        subq.b  #1,d1
        bne.s   LA5CC
LA5C8   move.w  d5,4(a6,a0.l)
LA5CC   rts

LA5CE   moveq   #0,d3
        subq.w  #2,a4
LA5D2   move.w  #$8800,d4               ;token nametable entry
        bsr.s   LA5E0
        bne.s   LA60C
        moveq   #3,d1
        bsr.s   LA57E
        bra.s   LA5D2

LA5E0   moveq   #0,d3
LA5E2   jsr     L9072(pc)
        cmp.w   d4,d1
        bne.s   LA5F0
        tst.b   d3
        beq.s   LA60C
        bra.s   LA5E2

LA5F0   cmpi.w  #$8405,d1               ;token symbol '(' ?
        bne.s   LA5FA
        addq.b  #1,d3
        bra.s   LA5E2

LA5FA   cmpi.w  #$8406,d1               ;token symbol ')' ?
        bne.s   LA604
        subq.b  #1,d3
        bra.s   LA5E2

LA604   jsr     L9686(pc)
        bne.s   LA5E2
        moveq   #1,d1
LA60C   rts

LA60E   jsr     L9B32(pc)
        bge.s   LA640
        tst.b   bv_sing(a6)
        bne.s   LA63E
        addq.w  #2,a4
        cmpa.l  bv_pfp(a6),a4
        blt.s   LA626
        jmp     LA994(pc)

LA626   move.w  0(a6,a4.l),d0
        add.w   d0,bv_length(a6)
        move.w  4(a6,a4.l),bv_linum(a6)
        moveq   #1,d0
        move.b  d0,bv_stmnt(a6)
        addq.w  #6,a4
        moveq   #0,d1
LA63E   rts

LA640   addq.w  #2,a4
        addq.b  #1,bv_stmnt(a6)
        moveq   #err.ok,d0
        rts

LA64A   jsr     LA56C(pc)
        cmpi.w  #$8117,d1               ;token keyword 'REMAINDER' ?
        beq.s   LA6CE
        bsr.s   LA68C
        cmpi.w  #$810B,0(a6,a4.l)       ;token keyword 'TO' ?
        beq.s   LA672
        bsr.s   LA6D6
        move.w  0(a6,a1.l),d0
        beq.s   LA6CA
        addi.w  #24,d0
        sub.w   -6(a6,a1.l),d0
        ble.s   LA6CA
        bra.s   LA6A6

LA672   bsr.s   LA6D6
        tst.b   2(a6,a1.l)
        bgt.s   LA6A6
        addq.w  #2,a4
        addq.l  #6,bv_rip(a6)
        bsr.s   LA68C
        bsr.s   LA6D6
        tst.b   2(a6,a1.l)
        blt.s   LA6A6
        bra.s   LA6CA

LA68C   movea.l a4,a0
        suba.l  bv_vvbas(a6),a2
        move.l  a2,-(a7)
        jsr     L5A7E(pc)
        movea.l bv_vvbas(a6),a2
        adda.l  (a7)+,a2
        movea.l a0,a4
        beq.s   LA6D4
        addq.w  #4,a7
        rts

LA6A6   addq.l  #6,bv_rip(a6)
LA6AA   move.w  0(a6,a4.l),d0
        cmpi.w  #$8404,d0               ;token symbol ',' ?
        bne.s   LA6B8
        addq.w  #2,a4
        bra.s   LA64A

LA6B8   cmpi.w  #$840A,d0               ;token symbol LF ?
        beq.s   LA6D2
        cmpi.w  #$8402,d0               ;token symbol ':' ?
        beq.s   LA6D2
        jsr     L9072(pc)
        bra.s   LA6AA

LA6CA   addq.l  #6,bv_rip(a6)
LA6CE   moveq   #err.ok,d0
        rts

LA6D2   moveq   #err.nc,d0
LA6D4   rts

LA6D6   subq.w  #6,a1
        move.l  2(a6,a2.l),2(a6,a1.l)
        move.w  0(a6,a2.l),0(a6,a1.l)
        jsr     L482A(pc)               ;ri.sub
        beq.s   LA6EC
        addq.w  #4,a7
LA6EC   rts

LA6EE   movea.l bv_tkpos(a6),a4
        movea.l bv_ptemp(a6),a2
LA6F6   sf      bv_undo(a6)
        move.l  a4,bv_tkpos(a6)
        move.l  a2,bv_ptemp(a6)
        move.l  4(a6,a2.l),d4
        movea.l a4,a0
        move.l  bv_ntp(a6),d0
        sub.l   bv_ntbas(a6),d0
        move.l  d0,-(a7)
        jsr     L614A(pc)
        bne.s   LA724
        move.w  0(a6,a0.l),d1
        jsr     L9686(pc)
        beq.s   LA732
        moveq   #err.bp,d0
LA724   move.l  bv_ntbas(a6),d1
        add.l   (a7)+,d1
        move.l  d1,bv_ntp(a6)
        tst.l   d0
        rts

LA732   move.l  a5,d0
        sub.l   bv_ntbas(a6),d0
        move.l  d0,-(a7)
        movea.l a0,a4
        movea.l bv_ntbas(a6),a3
        adda.l  4(a7),a3
        movea.l d4,a2
        movea.l bv_rip(a6),a1
        suba.l  bv_ribas(a6),a1
        suba.l  bv_tkbas(a6),a4
        movem.l d5-d7/a1/a4,-(a7)
        move.l  bv_linum(a6),-(a7)
        move.b  bv_stmnt(a6),-(a7)
        move.l  bv_inlin(a6),-(a7)
        jsr     (a2)
        move.l  d0,d2
        move.l  (a7)+,bv_inlin(a6)
        move.b  (a7)+,bv_stmnt(a6)
        move.l  (a7)+,bv_linum(a6)
        movem.l (a7)+,d5-d7/a1/a4
        adda.l  bv_tkbas(a6),a4
        adda.l  bv_ribas(a6),a1
        move.l  a1,bv_rip(a6)
        movea.l bv_ntbas(a6),a5
        movea.l a5,a3
        adda.l  (a7)+,a5
        adda.l  (a7)+,a3
        jsr     L5702(pc)
        move.l  d2,d0
        tst.b   bv_undo(a6)
        bne     L97E0
        rts

LA79C   move.b  #$7F,bv_stmnt(a6)
        moveq   #0,d4
        move.w  bv_index(a6),d4
        jsr     LA0AA(pc)
        bge     LA37A
        rts

LA7B2   moveq   #err.ok,d0
        rts

LA7B6   jsr     LA56C(pc)
        cmpi.b  #nam.b,d0
        bne.s   LA7F0
        moveq   #0,d4
        move.w  2(a6,a4.l),d4
        addq.w  #4,a4
        jsr     L9852(pc)
        blt.s   LA7D8
        move.b  #1,bv_inlin(a6)
        move.w  d4,bv_index(a6)
LA7D8   jsr     L9F96(pc)
        move.b  0(a6,a2.l),d2
        moveq   #6,d1
        subq.b  #2,d2
        beq.s   LA7F4
        subq.b  #4,d2
        beq.s   LA81C
        moveq   #26,d1
        subq.b  #1,d2
        beq.s   LA7F4
LA7F0   moveq   #err.bn,d0
        rts

LA7F4   move.l  a2,-(a7)
        move.l  4(a6,a2.l),d2
        blt.s   LA806
        movea.l bv_vvbas(a6),a0
        adda.l  d2,a0
        jsr     L4FE8(pc)
LA806   moveq   #12,d1
        jsr     L4DF6(pc)
        suba.l  bv_vvbas(a6),a0
        movea.l (a7)+,a2
        move.l  a0,4(a6,a2.l)
        move.b  #6,0(a6,a2.l)
LA81C   jmp     LA2FE(pc)

LA820   clr.w   bv_dalno(a6)
        movea.l a4,a0
        jsr     L5A82(pc)
        movea.l a0,a4
        blt.s   LA848
        bgt.s   LA83A
        addq.l  #2,bv_rip(a6)
        move.w  0(a6,a1.l),bv_dalno(a6)
LA83A   move.b  #1,bv_dastm(a6)
        move.b  #1,bv_daitm(a6)
        moveq   #err.ok,d0
LA848   rts

LA84A   jsr     LA56C(pc)
        cmpi.w  #$8115,d1               ;token keyword 'ON' ?
        bne.s   LA85A
        addq.w  #2,a4
        jsr     LA56C(pc)
LA85A   moveq   #0,d4
        move.w  2(a6,a4.l),d4
        addq.w  #4,a4
        jsr     L9F80(pc)
        subq.b  #2,d2
        bne.s   LA8A0
        tst.l   d0
        blt.s   LA89C
        jsr     L9852(pc)
        blt.s   LA88C
        tst.b   bv_inlin(a6)
        bne.s   LA87E
        st      bv_inlin(a6)
LA87E   jsr     LA56C(pc)
        cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   LA88C
        addq.w  #2,a4
        bra.s   LA892

LA88C   jsr     L9D72(pc)
        bne.s   LA898
LA892   jsr     LA64A(pc)
        blt.s   LA88C
LA898   moveq   #err.ok,d0
        rts

LA89C   moveq   #err.xp,d0
        rts

LA8A0   moveq   #err.bn,d0
        rts

LA8A4   movea.l bv_pfbas(a6),a4
LA8A8   sf      bv_inlin(a6)
        tst.b   bv_sing(a6)
        bne.s   LA8B8
LA8B2   bsr     LA966
        bne.s   LA8F8
LA8B8   moveq   #1,d0
        jsr     L4E32(pc)
        moveq   #0,d0
LA8C0   jsr     LA56C(pc)
        cmpi.b  #key.b,d0
        bne.s   LA8D0
        jsr     LA190(pc)
        bra.s   LA8EA

LA8D0   cmpi.b  #nam.b,d0
        bne.s   LA8DC
        jsr     LA33C(pc)
        bra.s   LA8EA

LA8DC   cmpi.b  #sym.b,d0
        bne.s   LA8E8
        jsr     LABF0(pc)
        bra.s   LA8EA

LA8E8   moveq   #err.ni,d0
LA8EA   tst.l   d0
        bne     L9BA8
        tas     bv_brk(a6)
        beq     L9B90
LA8F8   tas     bv_cont(a6)
        beq     LA9BA
        tst.b   bv_sing(a6)
        bne.s   LA90C
        cmpa.l  bv_pfbas(a6),a4
        ble.s   LA8A4
LA90C   jsr     L9B32(pc)
        bge.s   LA95C
        tst.b   bv_inlin(a6)
        beq.s   LA94E
        blt.s   LA924
        jsr     LA79C(pc)
        tst.b   bv_inlin(a6)
        bne.s   LA8EA
LA924   tst.b   bv__C0(a6)
        beq.s   LA93C
        tst.b   bv__BF(a6)
        beq.s   LA94E
        jsr     L7E50(pc)
        st      bv_cont(a6)
        jmp     LA9BA(pc)

LA93C   move.w  bv_linum(a6),d4
        moveq   #-1,d3
        jsr     LACC2(pc)
        bne.s   LA94E
        jsr     L9AEE(pc)
        bra.s   LA90C

LA94E   tst.b   bv_sing(a6)
        addq.w  #2,a4
        beq     LA8B2
        bsr.s   LA998
        bra.s   LA990

LA95C   addq.w  #2,a4
        addq.b  #1,bv_stmnt(a6)
        bra     LA8B8

LA966   sf      bv_inlin(a6)
LA96A   clr.w   bv_linum(a6)
        tst.b   bv_sing(a6)
        bne.s   LA98A
        cmpa.l  bv_pfp(a6),a4
        bge.s   LA994
        move.w  0(a6,a4.l),d0
        add.w   d0,bv_length(a6)
        move.w  4(a6,a4.l),bv_linum(a6)
        addq.w  #6,a4
LA98A   move.b  #1,bv_stmnt(a6)
LA990   moveq   #err.ok,d0
        rts

LA994   sf      bv_cont(a6)
LA998   move.w  #-1,bv_nxlin(a6)
        move.w  #4,bv_stopn(a6)
        rts

LA9A6   dc.w    LA9F2-LA9BA
        dc.w    LAADC-LA9BA
        dc.w    LABCA-LA9BA
        dc.w    LAA5E-LA9BA
        dc.w    LAADC-LA9BA
        dc.w    LAADC-LA9BA
        dc.w    LABDC-LA9BA
        dc.w    LABDC-LA9BA
        dc.w    LABA6-LA9BA
        dc.w    LA9C6-LA9BA

LA9BA   move.w  bv_stopn(a6),d1
        move.w  LA9A6(pc,d1.w),d1
        jmp     LA9BA(pc,d1.w)

LA9C6   jsr     L9BA8(pc)
        movea.l bv_rtp(a6),a0
        sf      bv_auto(a6)
LA9D2   cmpa.l  bv_rtbas(a6),a0
        ble.s   LA9FC
        move.b  -8(a6,a0.l),d0
        beq.s   LA9EE
        move.l  -24(a6,a0.l),d0
        add.l   bv_ntbas(a6),d0
        move.l  d0,bv_ntp(a6)
        suba.w  #16,a0
LA9EE   subq.w  #8,a0
        bra.s   LA9D2

LA9F2   tst.b   bv_unrvl(a6)
        beq.s   LA9FC
        jsr     L97E0(pc)
LA9FC   bsr     LAA88
        jsr     L56C6(pc)
        movea.l bv_ntbas(a6),a0
LAA08   move.b  0(a6,a0.l),d0
        subq.b  #1,d0
        beq.s   LAA4A
        subq.b  #3,d0
        blt.s   LAA1C
        subq.b  #2,d0
        blt.s   LAA52
        subq.b  #2,d0
        bge.s   LAA52
LAA1C   move.w  2(a6,a0.l),d0
        blt.s   LAA4A
        bsr     LAACC
        move.b  -1(a6,a1.l),d0
        moveq   #1,d1
        subi.b  #37,d0
        blt.s   LAA38
        bgt.s   LAA36
        addq.w  #1,d1
LAA36   addq.w  #1,d1
LAA38   move.b  d1,1(a6,a0.l)
        move.b  d7,0(a6,a0.l)
        move.l  #-1,4(a6,a0.l)
        bra.s   LAA52

LAA4A   move.l  d7,0(a6,a0.l)
        move.l  d7,4(a6,a0.l)
LAA52   addq.w  #8,a0
        cmpa.l  bv_ntp(a6),a0
        blt.s   LAA08
        st      bv_edit(a6)
LAA5E   tst.b   bv_unrvl(a6)
        beq.s   LAA80
        move.w  bv_nxlin(a6),bv_ptemp(a6)
        move.w  bv_stopn(a6),bv__B6(a6)
        jsr     L97E0(pc)
        move.w  bv_ptemp(a6),bv_nxlin(a6)
        move.w  bv__B6(a6),bv_stopn(a6)
LAA80   jsr     LA4BA(pc)
        bra     LABE6

LAA88   moveq   #0,d7
        move.l  d7,bv_dalno(a6)
        move.b  #1,bv_daitm(a6)
        move.l  bv_vvbas(a6),bv_vvp(a6)
        move.l  #-1,bv__CA(a6)
        move.w  d7,bv__C8(a6)
        move.l  bv_rtbas(a6),bv_rtp(a6)
        moveq   #bv_rip,d0
LAAAE   move.l  bv_ribas(a6),0(a6,d0.w)
        subq.w  #4,d0
        cmpi.b  #bv_btp,d0
        bge.s   LAAAE
        sf      bv_unrvl(a6)
        move.l  d7,bv_vvfree(a6)
        jsr     L4F9E(pc)
        jmp     L4F96(pc)

LAACC   movea.l bv_nlbas(a6),a1
        adda.w  d0,a1
        moveq   #1,d0
        add.b   0(a6,a1.l),d0
        adda.w  d0,a1
        rts

LAADC   movea.l bv_chp(a6),a3
        movea.l bv_chbas(a6),a2
        adda.w  #120,a2                 ;skip over #0, #1, #2
        move.l  a2,bv_chp(a6)
LAAEC   cmpa.l  a3,a2
        bge.s   LAB02
        move.l  0(a6,a2.l),d0
        blt.s   LAAFC
        movea.l d0,a0
        moveq   #io.close,d0
        trap    #2
LAAFC   adda.w  #ch.lench,a2
        bra.s   LAAEC

LAB02   moveq   #mt.dmode,d0
        moveq   #-1,d1
        moveq   #-1,d2
        trap    #1
        moveq   #mt.dmode,d0
        trap    #1
        move.l  bv_lnbas(a6),bv_lnp(a6)
        clr.w   bv_lsbas(a6)
        bsr     LAA88
        move.l  bv_pfbas(a6),bv_pfp(a6)
        moveq   #0,d0
        moveq   #0,d5
        movea.l bv_ntbas(a6),a0
        moveq   #8,d1
LAB2C   cmpa.l  bv_ntp(a6),a0
        beq.s   LAB84
        cmp.b   0(a6,a0.l),d1
        bgt.s   LAB42
        tst.b   d5
        bne.s   LAB50
        move.w  2(a6,a0.l),d0
        bra.s   LAB80

LAB42   tst.b   d5
        bne.s   LAB80
        movea.l a0,a2
        bsr.s   LAACC
        movea.l a1,a3
        st      d5
        bra.s   LAB80

LAB50   move.l  0(a6,a0.l),0(a6,a2.l)
        move.l  4(a6,a0.l),4(a6,a2.l)
        move.w  2(a6,a2.l),d0
        bsr     LAACC
        suba.w  d0,a1
        move.l  a3,d2
        sub.l   bv_nlbas(a6),d2
        move.w  d2,2(a6,a2.l)
        addq.w  #8,a2
LAB72   move.b  0(a6,a1.l),0(a6,a3.l)
        addq.w  #1,a1
        addq.w  #1,a3
        subq.w  #1,d0
        bgt.s   LAB72
LAB80   addq.w  #8,a0
        bra.s   LAB2C

LAB84   tst.b   d5
        beq.s   LAB90
        move.l  a2,bv_ntp(a6)
        move.l  a3,bv_nlp(a6)
LAB90   jsr     L4F9A(pc)
        jsr     L4F8E(pc)
        jsr     L4F92(pc)
        jsr     L4F8A(pc)
        jsr     L56C6(pc)
        bra.s   LABC4

LABA6   move.w  bv_cnlno(a6),bv_nxlin(a6)
        move.b  bv_cnstm(a6),bv_nxstm(a6)
        move.w  #-1,bv_cnlno(a6)
        move.b  bv_cninl(a6),bv_inlin(a6)
        move.w  bv_cnind(a6),bv_index(a6)
LABC4   sf      bv_comln(a6)
        bra.s   LABEC

LABCA   tst.b   bv__C0(a6)
        sf      bv__C0(a6)
        bne.s   LABE6
        moveq   #0,d0
        jsr     L9B5E(pc)
        bra.s   LABEC

LABDC   tst.b   bv_unrvl(a6)
        beq.s   LABE6
        jsr     L97E0(pc)
LABE6   move.b  bv_sing(a6),bv_comln(a6)
LABEC   moveq   #err.ok,d0
        rts

LABF0   cmpi.w  #$8401,d1               ;token symbol '=' ?
        bne.s   LABFC
        addq.w  #2,a4
        jmp     L9CFE(pc)

LABFC   moveq   #err.ok,d0
        rts

LAC00   jsr     LA56C(pc)
        cmpi.w  #$810E,d1               ;token keyword 'ERRor' ?
        bne.s   LAC32
        move.w  bv_linum(a6),bv__BC(a6)
        move.b  bv_stmnt(a6),bv__BE(a6)
        jsr     L9852(pc)
        seq     bv__BF(a6)
        beq.s   LAC28
        moveq   #6,d2
        jmp     L98A8(pc)

LAC26   addq.w  #2,a4
LAC28   jsr     L9B32(pc)
        bge.s   LAC26
LAC2E   moveq   #err.ok,d0
        rts

LAC32   move.w  2(a6,a4.l),d4
        addq.w  #4,a4
        jsr     LA56C(pc)
        cmpi.b  #ops.b,d0
        beq.s   LAC54
LAC42   moveq   #1,d3
        jsr     LACC2(pc)
        bne.s   LAC2E
        st      0(a6,a2.l)
        subq.w  #1,bv__C8(a6)
        bra.s   LAC42

LAC54   moveq   #1,d3
        jsr     LACC2(pc)
LAC5A   bne.s   LAC6C
        move.w  bv_linum(a6),d0
        cmp.w   2(a6,a2.l),d0
        beq.s   LAC76
        jsr     LACDA(pc)
        bra.s   LAC5A

LAC6C   moveq   #0,d3
        jsr     LACC2(pc)
        addq.w  #1,bv__C8(a6)
LAC76   move.w  d4,0(a6,a2.l)
        move.w  bv_linum(a6),2(a6,a2.l)
        move.b  bv_stmnt(a6),4(a6,a2.l)
        move.w  bv_linum(a6),6(a6,a2.l)
        move.b  #127,5(a6,a2.l)
        clr.l   10(a6,a2.l)
        st      8(a6,a2.l)
        move.l  a2,-(a7)
        jsr     L9852(pc)
        movea.l (a7)+,a2
        seq     14(a6,a2.l)
        beq.s   LAC28
        moveq   #6,d2
        move.l  a2,-(a7)
        jsr     L98A8(pc)
        move.l  (a7)+,d2
        move.w  bv_linum(a6),6(a6,a2.l)
        move.b  bv_stmnt(a6),5(a6,a2.l)
        bra     LAC2E

LACC2   move.w  bv__C8(a6),d2
        beq.s   LAD06
        bsr.s   LAD10
LACCA   move.w  0(a6,a2.l),d0
        blt.s   LACEC
        tst.b   d3
        blt.s   LACF2
        beq.s   LACDE
        cmp.w   d0,d4
        beq.s   LACFE
LACDA   subq.w  #1,d2
        beq.s   LAD02
LACDE   adda.w  #16,a2
        subi.l  #16,d1
        bge.s   LACCA
        bra.s   LAD26

LACEC   tst.b   d3
        bne.s   LACDE
        rts

LACF2   cmp.w   6(a6,a2.l),d4
        bne.s   LACDA
        tst.w   8(a6,a2.l)
        blt.s   LACDA
LACFE   moveq   #err.ok,d0
        rts

LAD02   moveq   #err.nc,d0
        rts

LAD06   tst.b   d3
        bne.s   LAD02
        tst.l   bv__CA(a6)
        blt.s   LAD26
LAD10   movea.l bv_vvbas(a6),a2
        adda.l  bv__CA(a6),a2
        move.l  0(a6,a2.l),d1
        subi.l  #20,d1
        addq.w  #4,a2
        rts

LAD26   move.l  bv__CA(a6),d1
        bge.s   LAD32
        moveq   #0,d1
        moveq   #0,d0
        bra.s   LAD3C

LAD32   movea.l bv_vvbas(a6),a2
        adda.l  d1,a2
        move.l  0(a6,a2.l),d0
LAD3C   addi.l  #320,d1
        move.l  a2,-(a7)
        move.l  d0,-(a7)
        jsr     L4DF6(pc)
        move.l  (a7)+,d0
        movea.l (a7)+,a2
        move.l  d1,0(a6,a0.l)
        move.l  a0,-(a7)
        sub.l   d0,d1
LAD56   addq.w  #4,a2
        addq.w  #4,a0
        subq.w  #4,d0
        ble.s   LAD66
        move.l  0(a6,a2.l),0(a6,a0.l)
        bra.s   LAD56

LAD66   move.l  a0,-(a7)
LAD68   st      0(a6,a0.l)
        adda.w  #16,a0
        subi.l  #16,d1
        bgt.s   LAD68
        move.l  bv__CA(a6),d1
        blt.s   LAD8C
        movea.l bv_vvbas(a6),a0
        adda.l  d1,a0
        move.l  0(a6,a0.l),d1
        jsr     L4FE8(pc)
LAD8C   movea.l (a7)+,a2
        move.l  (a7)+,d1
        sub.l   bv_vvbas(a6),d1
        move.l  d1,bv__CA(a6)
        rts

;--------------------------------------
LAD9A   include char_font1
LB106   include char_font2
;--------------------------------------

;Keyboard decoding. Use IPC data and multiple return outcome mechanism to
;translate keyboard matrix key pressed to corressponding QL key code action.

LB352   andi.w  #$0007,d2                       ;mask out SHIFT/CTRL/ALT bits
        andi.w  #$003F,d1                       ;limit matrix key code to 0-63
        cmpi.b  #54,d1                          ;SPACE key ?
        bne.s   LB368                           ;no
        btst    #1,d2                           ;with CTRL key ?
        beq.s   LB368                           ;no
        rts                                     ;else return to first outcome

LB368   movem.l d3-d5/a3,-(a7)
        lea     MB458,a3                        ;key translation tables
        move.w  8(a3),d4                        ;get fourth offset which is
        lea     0(a3,d4.w),a3                   ;pointer to cursor/capslock key table
        moveq   #4,d4                           ;of which there are 5 entries
LB37C   cmp.b   (a3)+,d1                        ;match matrix key code ?
        beq.s   LB388                           ;yes, deal with immediately
        addq.l  #1,a3                           ;skip over corressponding QL key code
        dbf     d4,LB37C                        ;check with next entry
        bra.s   LB392                           ;else continue with search

LB388   move.b  (a3),d1                         ;get corressponding QL key code
        or.b    d2,d1                           ;offset by SHIFT/CTRL/ALT key
        dc.w    $4EF9,0,LB450    *** jmp LB450  ;return to third outcome

LB392   clr.w   d5                              ;status of
        bclr    #0,d2                           ;ALT pressed ?
        beq.s   LB39E                           ;no
        move.b  #$FF,d5                         ;else treat as ALT combination key
LB39E   lea     MB458,a3                        ;key translation tables
        move.w  0(a3,d2.w),d3                   ;get offset to SHIFT/CTRL tables
        lea     0(a3,d3.w),a3                   ;pointer to appropriate table
        clr.l   d3                              ;(byte used as word offset)
        move.b  (a3)+,d3                        ;start range of table
        cmp.b   d3,d1                           ;key below table ?
        bge.s   LB3BA                           ;no
        dc.w    $4EF9,0,LB446    *** jmp LB446  ;else return to second outcome

LB3BA   cmp.b   (a3)+,d1                        ;key above end of table ?
        ble.s   LB3C4                           ;no
        dc.w    $4EF9,0,LB446    *** jmp LB446  ;else return to second outcome

LB3C4   suba.w  d3,a3                           ;align table pointer and
        move.b  0(a3,d1.w),d3                   ;get corressponding QL key code
        cmpi.b  #$FF,d3                         ;is it a special accent type key ?
        bne.s   LB3D8                           ;no
        add.b   d2,d1                           ;add SHIFT/CTRL value
        move.b  d1,sv_ichar(a6)                 ;update character input buffer
        bra.s   LB446                           ;return to second outcome

LB3D8   move.b  d3,d1                           ;QL key code for matrix key
        move.b  sv_ichar(a6),d2                 ;previous special key ?
        beq.s   LB414                           ;no, key is true representation
        clr.b   sv_ichar(a6)                    ;else acknowledge it
        lea     MB458,a3                        ;key translation tables
        move.w  10(a3),d4                       ;get fifth offset which is
        lea     0(a3,d4.w),a3                   ;pointer to special key table
LB3F2   tst.b   (a3)                            ;table defined ?
        beq.s   LB446                           ;no, return to second outcome
        cmp.b   (a3)+,d2                        ;is this the accentable key ?
        beq.s   LB404                           ;yes, get the required accent
        move.b  (a3)+,d4                        ;else get offset to next
        asl.b   #1,d4                           ;double it
        ext.w   d4                              ;and use as offset to
        adda.w  d4,a3                           ;skip to next special key
        bra.s   LB3F2                           ;and check for match

LB404   move.b  (a3)+,d4                        ;number of accents in list
LB406   beq.s   LB446                           ;end of list
        cmp.b   (a3)+,d3                        ;match required accent ?
        beq.s   LB412                           ;yes, new key code follows
        addq.l  #1,a3                           ;else skip it
        subq.b  #1,d4                           ;one less to check
        bra.s   LB406                           ;try next

LB412   move.b  (a3),d1                         ;get substitution key code
LB414   tst.b   sv_caps(a6)                     ;capslock setting ?
        beq.s   LB43C                           ;not set
        cmpi.b  #'a',d1
        bcs.s   LB43C                           ;not alpha lower case or foreign
        cmpi.b  #'z',d1
        bls.s   LB438                           ;alpha lower case
        cmpi.b  #$80,d1
        bcs.s   LB43C                           ;not foreign character
        cmpi.b  #$8B,d1
        bhi.s   LB43C                           ;not foreign lower case
        bset    #5,d1                           ;make upper case
        bra.s   LB43C

LB438   bclr    #5,d1                           ;make upper case
LB43C   tst.b   d5                              ;ALT combination ?
        beq.s   LB450                           ;no, return to third outcome
        lsl.w   #8,d1                           ;combination key to upper byte
        move.b  d5,d1                           ;lower byte 255 signifies ALT key
        bra.s   LB450                           ;return to third outcome

LB446   clr.w   d1                              ;key has no meaning/not found
        movem.l (a7)+,d3-d5/a3
        addq.l  #2,(a7)                         ;return to second outcome
        rts

LB450   movem.l (a7)+,d3-d5/a3
        addq.l  #4,(a7)                         ;return to third outcome
        rts

;--------------------------------------
LB458   include key_codes
;--------------------------------------

;Translate d1.b and if required put translation into serial transmit queue at a2

LB56C   movem.l d1-d3/a1,-(a7)
        moveq   #0,d0                           ;(irrelevant instruction)
        movea.l sv_trtab(a6),a1                 ;current translation table
        move.w  2(a1),d2                        ;offset to first table
        lea     0(a1,d2.w),a1                   ;pointer to first table
        tst.b   d1                              ;byte to send
        beq.s   LB5D6                           ;is zero, put it in queue and return
        move.b  0(a1,d1.w),d0                   ;get translate byte
        tst.b   d0                              ;if zero
        beq.s   LB58E                           ;use second table
        move.b  d0,d1                           ;use translation byte
        bra.s   LB5D6                           ;put it in queue and return

LB58E   move.l  d1,d3                           ;preserve original byte
        dc.w    $4EB9,0,L380A    *** jsr L380A  ;io.qtest, test queue status
        move.l  d3,d1
        cmpi.w  #err.ef,d0                      ;was queue at EOF ?
        beq.s   LB5DC                           ;yes, return error to caller
        moveq   #err.ok,d0                      ;no error if no translation required
        cmpi.w  #3,d2                           ;free space in queue big enough?
        bge.s   LB5AA                           ;yes, find translation
        moveq   #err.nc,d0                      ;operation incomplete
        bra.s   LB5DC                           ;return to caller

LB5AA   movea.l sv_trtab(a6),a1                 ;current translation table
        move.w  4(a1),d2                        ;offset to second table
        lea     0(a1,d2.w),a1                   ;pointer to second table
        move.b  (a1)+,d3                        ;total translations in table
LB5B8   beq.s   LB5DC                           ;none, return to caller
        cmp.b   (a1)+,d1                        ;this byte translation ?
        beq.s   LB5C4                           ;yes, perform translation
        addq.l  #3,a1                           ;skip translation bytes
        subq.b  #1,d3                           ;decrement counter
        bra.s   LB5B8                           ;look for next match

LB5C4   move.b  (a1)+,d1                        ;first translation byte
        dc.w    $4EB9,0,L3838    *** jsr L3838  ;io.qin, place byte in queue
        move.b  (a1)+,d1                        ;second translation byte
        dc.w    $4EB9,0,L3838    *** jsr L3838  ;io.qin, place byte in queue
        move.b  (a1),d1                         ;third translation byte
LB5D6   dc.w    $4EB9,0,L3838    *** jsr L3838  ;io.qin, place byte in queue
LB5DC   movem.l (a7)+,d1-d3/a1
        rts

;Translate d1.b fetched from serial receive queue by returning translation entry.

LB5E2   movem.l d0-d2/a1,-(a7)
        movea.l sv_trtab(a6),a1         ;current translate table
        move.w  2(a1),d2                ;offset to first table
        lea     0(a1,d2.w),a1           ;pointer to first table
        move.b  0(a1,d1.w),d0           ;get translation entry
        tst.b   d0                      ;if zero
        beq.s   LB60E                   ;no translation required
        move.w  d1,d2                   ;from this table entry
LB5FC   move.b  0(a1,d2.w),d0           ;get translation byte
        cmp.b   d0,d1                   ;match original ?
        beq.s   LB60C                   ;yes, return table entry number
        addq.b  #1,d2                   ;else next table entry
        cmp.b   d2,d1                   ;match original byte ?
        bne.s   LB5FC                   ;no, get next
        bra.s   LB60E                   ;else return 
LB60C   move.b  d2,d1
LB60E   movem.l (a7)+,d0-d2/a1          ;(this undoes any translation) [!]
        rts

;Translation table (no translation)

LB614   dc.w    $4AFB                   ;identifier
        dc.w    LB61A-LB614             ;first table (256 entries)
        dc.w    LB71A-LB614             ;second table (variable)

LB61A   dc.b    $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        dc.b    $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F
        dc.b    $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F
        dc.b    $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F
        dc.b    $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
        dc.b    $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F
        dc.b    $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F
        dc.b    $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F
        dc.b    $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F
        dc.b    $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F
        dc.b    $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF
        dc.b    $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF
        dc.b    $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF
        dc.b    $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF
        dc.b    $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF
        dc.b    $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF

LB71A   dc.w    0                       ;empty second table

;Message table

LB71C   dc.w    $4AFB                   ;identifier

        dc.w    LB758-LB71C     ;-$01 err.nc
        dc.w    LB768-LB71C     ;-$02 err.nj
        dc.w    LB776-LB71C     ;-$03 err.om
        dc.w    LB786-LB71C     ;-$04 err.or
        dc.w    LB796-LB71C     ;-$05 err.bo
        dc.w    LB7A4-LB71C     ;-$06 err.no
        dc.w    LB7B8-LB71C     ;-$07 err.nf
        dc.w    LB7C4-LB71C     ;-$08 err.ex
        dc.w    LB7D6-LB71C     ;-$09 err.iu
        dc.w    LB7E0-LB71C     ;-$0A err.ef
        dc.w    LB7EE-LB71C     ;-$0B err.df
        dc.w    LB7FC-LB71C     ;-$0C err.bn
        dc.w    LB808-LB71C     ;-$0D err.te
        dc.w    LB816-LB71C     ;-$0E err.ff
        dc.w    LB826-LB71C     ;-$0F err.bp
        dc.w    LB836-LB71C     ;-$10 err.fe
        dc.w    LB84E-LB71C     ;-$11 err.xp
        dc.w    LB864-LB71C     ;-$12 err.ov
        dc.w    LB870-LB71C     ;-$13 err.ni
        dc.w    LB882-LB71C     ;-$14 err.ro
        dc.w    LB88E-LB71C     ;-$15 err.bl
        dc.w    LB89A-LB71C     ;-$16 err.at
        dc.w    LB8A4-LB71C     ;-$17 err.sc
        dc.w    LB8B0-LB71C     ;-$18 err.bt
        dc.w    LB8C8-LB71C     ;-$19 err.cp
        dc.w    LB8E8-LB71C     ;-$1A err.dw
        dc.w    LB902-LB71C     ;-$1B err.pf
        dc.w    LB914-LB71C     ;-$1C daytab
        dc.w    LB92A-LB71C     ;-$1D montab

LB758   dc.w    13
        dc.b    'not complete',10,0
LB768   dc.w    12
        dc.b    'invalid Job',10
LB776   dc.w    14
        dc.b    'out of memory',10
LB786   dc.w    13
        dc.b    'out of range',10,0
LB796   dc.w    12
        dc.b    'buffer full',10
LB7A4   dc.w    17
        dc.b    'channel not open',10,0
LB7B8   dc.w    10
        dc.b    'not found',10
LB7C4   dc.w    15
        dc.b    'already exists',10,0
LB7D6   dc.w    7
        dc.b    'in use',10,0
LB7E0   dc.w    12
        dc.b    'end of file',10
LB7EE   dc.w    11
        dc.b    'drive full',10,0
LB7FC   dc.w    9
        dc.b    'bad name',10,0
LB808   dc.w    11
        dc.b    'Xmit error',10,0
LB816   dc.w    14
        dc.b    'format failed',10
LB826   dc.w    14
        dc.b    'bad parameter',10
LB836   dc.w    22
        dc.b    'bad or changed medium',10
LB84E   dc.w    20
        dc.b    'error in expression',10
LB864   dc.w    9
        dc.b    'overflow',10,0
LB870   dc.w    16
        dc.b    'not implemented',10
LB882   dc.w    10
        dc.b    'read only',10
LB88E   dc.w    9
        dc.b    'bad line',10,0
LB89A   dc.w    8
        dc.b    'At line '
LB8A4   dc.w    9
        dc.b    ' sectors',10,0
LB8B0   dc.w    21
        dc.b    'F1...monitor',10,'F2...TV',10,0
LB8C8   dc.w    30
        dc.b    '  1983 Sinclair Research Ltd '
LB8E8   dc.w    23
        dc.b    'during WHEN processing',10,0
LB902   dc.w    16
        dc.b    'PROC/FN cleared',10
LB914   dc.b    'SunMonTueWedThuFriSat',0
LB92A   dc.b    'JanFebMarAprMayJunJulAugSepOctNovDec'

        lib     spare_bin               ;spare ROM 1684 bytes (all zero)

LBFE2   dc.l    LB614                   ;Pointer to translation table
LBFE6   dc.l    LB71C                   ;Pointer to message table
LBFEA   dc.l    LB5E2                   ;Translate routine for serial fetch
LBFEE   dc.l    LB56C                   ;Translate routine for serial send
LBFF2   dc.l    LB352                   ;Key matrix to QL key code translation
LBFF6   dc.b    '1.10'                  ;version string
LBFFA   dc.w    $0002
        dc.b    'JS  '

        end
