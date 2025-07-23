;
;	Small Monitor EKMonitor for ReadBoard-2 6809
;
;	Version:	2.0
;	Created:	2012 September 04	
;	Updated:	2025 june 02
;	Author:		Laurent Favard
;	Contact:	laurent68k@gmail.com
;
;	CCR:	7 6 5 4 3 2 1 0
;			E F H I N Z V C
;
;	This source code is free and can be used, copied, updated and modified for any hardware you want
;	wihtout restrictions.
;
;	Change log:
;	----------------------------------------------------------------------------------------------------
;	----------------------------------------------------------------------------------------------------
;	2015/02/27	-	Fix in SkipSpace about the bug found in SkipSpace by "Roland Leurs". Thanks you!
;	2013/05/24	-   Added a tribute to the pioniers
;   2013/04/12	-	SECOND VERSION BURNED ! Monitor works fine
;				-	LOAD command ok
;				-	MEMSIZE with decimal memory display ok
;	2013/04/05	-   Fix in GetHexDigit
;	2013/03/29	-	Fix in WriteDecimal2D
;	2013/03/26	-	Added WriteDecimal2D to print a number with the digit
;	2013/03/25	-	Fix in WriteDecimalBytes, print at least one Zero.
;	2013/03/22	-	Added automatic execute of Application Cart. init code
;				-	Display the Application count in decimal 
;				-	Display the Date and Time in decimal
;				-	Display the memory size in decimal 
;				-	Display the monitor version in decimal as: '1.2'
;				-	display decimal routine ignore the firsts '0' on the left
;				-	CA_Init is now checked and executed while monitor booting 
;	2013/03/21	-	Added routine to display decimal values on the terminal
;				-	Added CA_Init field in APP header for extra init program. Not used.
;	2013/03/20	-	Added new Disgnostic cartridge, then Automatic moved before Application cart. checking
;				-	Added Date and Time of applications cartridge			
;	2013/03/18	-	Added GetHexDigit and LoadCodeSerial
;   2013/03/15	-	Removed CTRX on SETPIA
;				-	Added Dump command display now the letter on the terminal
;				-	Load command added without action
;	----------------------------------------------------------------------------------------------------
;   2013/02/24	-	FIRST VERSION BURNED ! The Board Works and tinyBasic too
;	----------------------------------------------------------------------------------------------------
;               -   Updated to peform a Master Reset on the 6850
;   2013/01/16	-	Updated to divide by 16 the clock of 6850
;	2013/01/12	-	Updated, uppercase all received characters from UART when between [a,z]
;	2013/01/05	-	Updated disable and reenable inerrupts
;               -   Bug fixed, PIA was set as out instead of In mode.
;	2012/12/19	-	Added init of UART, 8 bits 1 stop Parity None
;	2012/12/10	-	Added MEMSIZE
;				-	Added SETPIA CTRX: to finish
;				-	Using of DispatchCommands for the CLI
;	----------------------------------------------------------------------------------------------------
;	2012/09/04	-	Creation
;	----------------------------------------------------------------------------------------------------
RamStart			equ	$0000
RamEnd				equ	$7FFF
Cartridge           equ $8000
RomStart			equ	$C000
RomEnd				equ	$FFFF
;	----------------------------------------------------------------------------------------------------
Uart				equ	$A000
UartTDR				equ Uart+1
UartRDR				equ Uart+1
;	----------------------------------------------------------------------------------------------------
Pia					equ	$A002
PiaDDRA				equ	Pia
PiaORA				equ	Pia
PiaCRA				equ	Pia+1
PiaDDRB				equ	Pia+2
PiaORB				equ	Pia+2
PiaCRB				equ	Pia+3
PIA_INPUTS			equ	$00
PIA_OUTUTS			equ	$FF
;	----------------------------------------------------------------------------------------------------
Major				equ	RomStart+$06
Minor				equ	RomStart+$07
;	----------------------------------------------------------------------------------------------------
MagicCartHigh		equ	Cartridge
MagicCartLow		equ	MagicCartHigh+1
AutoCartEntry		equ	MagicCartLow+1
;	----------------------------------------------------------------------------------------------------
CA_Next				equ	0
CA_Run				equ	CA_Next+2
CA_Init				equ	CA_Run+2
CA_Date				equ	CA_Init+2
CA_Time				equ	CA_Date+2
CA_Name				equ	CA_Time+2
;	CA_Time is as follows: (Like in Atari GEMDOS)
;unsigned hour		:5		MSb
;unsigned minute	:6
;unsigned second	:5		LSb		0-31 seconds
;	CA_Date is as follows: (Like in Atari GEMDOS)
;unsigned year		:7		MSb		offset from 1980
;unsigned month		:4
;unsigned day		:5		LSb
;	----------------------------------------------------------------------------------------------------
ROMVectors			equ	$FFF0
;	----------------------------------------------------------------------------------------------------
STACK_SIZE_S		equ	64
STACK_SIZE_U		equ	STACK_SIZE_S
PRTLINE_SIZE		equ	16
NLCHAR				equ	$0A
CRCHAR				equ	$0D
SPACE				EQU	$20
BACKSPACECHAR		equ	$08
DELETECHAR			equ	$7F
RDSTRBUFSTART		equ	2
RDSTRBUFEND			equ	0
STRDateSep			equ	'/'
STRTimeSep			equ ':'
;	----------------------------------------------------------------------------------------------------
;	Stack pointer while boot sequence before RAM size evaluation
BootSSP				equ	$100									;	size = 64 bytes
BootUSP				equ	BootSSP-STACK_SIZE_S					;	size = 64 bytes

;	----------------------------------------------------------------------------------------------------
;	BOTTOM RAM

                    ORG $0000
                    
RamSize				rmb	2										;	Size of the RAM 
RamTop				rmb	2										;	Top RAM address
Swi3Vector			rmb	2										;	Vector address to SW3
Swi2Vector			rmb	2										;	Vector address to SW2
FirqVector			rmb	2										;	Vector address to FIRQ
IrqVector			rmb	2										;	Vector address to IRQ
SwiVector			rmb	2										;	Vector address to SWI
NmiVector			rmb	2										;	Vector address to NMI
Pia_CR				rmb 2										;	Scratch variable
Pia_DDR				rmb 2										;	Scratch variable
save1_reg8bits		rmb	1										;	Scratch variable
save2_reg8bits		rmb	1										;	Scratch variable
save3_reg8bits		rmb	1										;	Scratch variable
save_count			rmb	1										;	Scratch variable
decimalbuffer		rmb	4										;	Scratch variable for BCD operation
bcdbuf				rmb 5										;	Scratch variable for BCD operation
promptLine			rmb PRTLINE_SIZE							;	CLI line buffer

;	----------------------------------------------------------------------------------------------------
;	Start of System ROM

					org		RomStart					
					
					bra		OSStart
					fcc		"6809"
					fcb		2,0
					fcb		20,25,06,02
					fdb		FunctionsTable
					
OSStart:			;	Strategic init temporaly Stack pointer
					lds		#BootSSP
					ldu		#BootUSP
					andcc	#$f0
					orcc	#$50                        ;   Disable all interrupts IRQ and FIRQ (E F H I N Z V C)
				
					;	Set the interrupts vectors in RAM
					ldx		#Vector_swi3
					stx		Swi3Vector
					
					ldx		#Vector_swi2
					stx		Swi2Vector
					
					ldx		#Vector_firq
					stx		FirqVector
					
					ldx		#Vector_irq
					stx		IrqVector
					
					ldx		#Vector_swi
					stx		SwiVector
					
					ldx		#Vector_nmi
					stx		NmiVector
					
					;	Init Uart for format 8N1 and dib by 16
                    ;	CMOS 4060 has a crystal of 2457600 Hz => Q4: 2457600/16=153600 so need 153600/16=9600
					lda		#$03						;	ACIA master reset
					sta		Uart
					
					lda		#%00010101                  ;   %0001 0110 => 8N1, div by 16					
                    sta		Uart
					
                    ;   Diagnostic cartdridge inserted ? Code = DG
                    lda		MagicCartHigh
					cmpa	#'D'
					bne		RetAutoCart1
					
					lda		MagicCartLow
					cmpa	#'G'
					bne		RetAutoCart1
					
					ldx		#STRDiagCartOk
					lbsr	WriteString
					
					ldx		#RetAutoCart1				;	Load in X the return address in case of...				
					jmp		AutoCartEntry				;	Jump to the first instruction
										
                    ;   Display system started
RetAutoCart1:		andcc	#$A0                        ;   Enable all interrupts IRQ and FIRQ (E F H I N Z V C)

                    ldx		#STRSystemStart1
					lbsr	WriteString
                    ldx		#STRSystemStart2
					lbsr	WriteString

					;	Automatic memory checking and size compute by step of 512 bytes
					ldx		#$0000
MemCheck:    		lda  	511,x
					coma                			
					sta  	511,x
					cmpa 	511,x            			;	IS IT RAM?
					bne  	MemCheckEnd        			;	BRANCH IF NOT (ROM, BAD RAM OR NO RAM)
					
					leax 	512,x            			;	MOVE POINTER UP ONE
					com  	512,x            			;	RE-COMPLEMENT TO RESTORE BYTE
					bra  	MemCheck          			;	KEEP LOOKING FOR END OF RAM 
		  
MemCheckEnd:   		
					stx  	RamSize   
                    leax    -1,x						;	for 32Kb: $8000
                    stx  	RamTop         				;	for 32Kb: $7FFF
                    				
					tfr		x,d
					
					ldx		#STRMemCalculated
					lbsr	WriteString
					
					lbsr	WriteHexByte				;	print A
					tfr		b,a
					lbsr	WriteHexByte				;	print B
					
					ldx		#STRNewLine
					lbsr	WriteString
					
					;	use the calculated RAM size to set the stacks to the good location
					lds		RamTop						;	set System stack
					leax	-STACK_SIZE_S,s				;	x = x - STACK_SIZE_S
					tfr		x,u							;	store x in User stack
					
                    ;	add here init of any additionnal hardware	
					;	Set port as input
					lda		#%11111011                  ;	clear bit DDR to access to DDR register
					anda	PiaCRA
					sta		PiaCRA
					
					lda		#PIA_INPUTS					;	pins are input
					sta		PiaDDRA						;	set Direction register, b contains the good value
										
                    lda		#%00000100                  ;	Set the DDR bit to enable next access to Data Register
					ora		PiaCRA
					sta		PiaCRA			
								
					;	Set port as input
					lda		#%11111011					;	clear bit DDR to access to DDR register
					anda	PiaCRB
					sta		PiaCRB
					
					lda		#PIA_INPUTS					;	pins are input
					sta		PiaDDRB						;	set Direction register, b contains the good value
										
                    lda		#%00000100                  ;	Set the DDR bit to enable next access to Data Register
					ora		PiaCRB
					sta		PiaCRB			

                    ;   Automatic cartdridge inserted ? Code = AT
                    lda		MagicCartHigh
					cmpa	#'A'
					bne		RetAutoCart2
					
					lda		MagicCartLow
					cmpa	#'T'
					bne		RetAutoCart2
					
					ldx		#STRAutCartOk
					lbsr	WriteString
					
					ldx		#RetAutoCart2				;	Load in X the return address in case of...				
					jmp		AutoCartEntry				;	Jump to the first instruction

					;	check for an application cartridge: Code = AP
					;	Display the count of available Application and execute their init code if any
RetAutoCart2:		lda		MagicCartHigh
					cmpa	#'A'
					bne		NoCartApp
					
					lda		MagicCartLow
					cmpa	#'P'
					bne		NoCartApp
					
					ldx		#STRAppCartOk
					lbsr	WriteString
					
					
					ldy		#AutoCartEntry				;	Y = $8002		
					ldb		#1							;	count of applications
					
_mnAppLoop:         pshs    b

					ldx		CA_Init,y					;	Load the address of Init code if any
					cmpx	#$0000
					beq		_mnAppLoop2
					
                    pshs    a,b,x,y						;	Be secure: save our registers before !
					jsr		0,x							;	Execute init code
					puls    a,b,x,y
                    
_mnAppLoop2:       	ldd     CA_Next,y					
                    
					cmpd	#$0000
					beq		_mnAppLoopEnd
					
                    tfr     d,y
                    puls    b
                    addb	#1
					bra		_mnAppLoop
					
_mnAppLoopEnd:		ldx		#STRCountApp
					lbsr	WriteString

					puls	b
					clra								;	clear upper bytes
					std		decimalbuffer+2				;	store D to display
					clr		decimalbuffer
					clr		decimalbuffer+1
					ldx		#decimalbuffer
					lbsr	WriteDecimalBytes			;	print the 4 bytes in decimal, decimalbuffer is cleared
					
					ldx		#STRNewLine
					lbsr	WriteString

                    ;puls    a					
                    ;lbsr	WriteHexByte
					;ldx		#STRNewLine
					;lbsr	WriteString

					;	Ready
NoCartApp:			ldx		#STRSystemReady
					lbsr	WriteString
													
					
;	----------------------------------------------------------------------------------------------------
;   Monitor Main loop to handle interaction with user
;	Command Line Interpreter

Mainloop:			ldx		#STRPromptCLI
					lbsr	WriteString
					
					ldx		#promptLine		
					ldb		#PRTLINE_SIZE
					lbsr	ReadString
					
					ldx		#STRNewLine
					lbsr	WriteString

					;	experimentation
					ldy		#DispatchCommands				;	Y = jumps table
FindCmd:			ldx		#promptLine						;	X = String entered

					pshs	y								;	save y before change
					ldy		,y								;	load content of Y with command string to check
					lbsr	CompareString					;	compare
					beq		FoundCmd				
					puls	y								;	restore
					
					leay	4,y								;	not found, inc to next command to check
					ldd		,y								;	load D with the content at Y
					cmpd	#$0000							;	is the end of table (No address)?
					bne		FindCmd							;	No, loop again
					
					ldx		#STRSorry						;	end of table, unknow command
					lbsr	WriteString
					
					bra		Mainloop                        ;	go main loop 
					
FoundCmd:			puls	y								;	reload X with last table address
                    leay    2,y
                    ldy     ,y
					jmp     ,y								

;	----------------------------------------------------------------------------------------------------
;	Command execution
;	----------------------------------------------------------------------------------------------------

;	----------------------------------------------------------------------------------------------------
;	HELP Command
mnHelp:				ldx		#STRHelp
					lbsr	WriteString
                	lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	HELP Command
mnTribute:			ldx		#STRTribute
					lbsr	WriteString
                	lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	VER Command
mnVersion:			ldx		#STRVersion
					lbsr	WriteString
					
					clra	
					ldb		Major
					std		decimalbuffer+2				;	store D to display
					clr		decimalbuffer
					clr		decimalbuffer+1
					ldx		#decimalbuffer
					lbsr	WriteDecimalBytes			;	print the 4 bytes in decimal, decimalbuffer is cleared
					
                	lda		#'.'						;	symbol for data to display
					lbsr	PutChar						;	display
										
					clra	
					ldb		Minor
					std		decimalbuffer+2				;	store D to display
					lbsr	WriteDecimalBytes			;	print the 4 bytes in decimal, decimalbuffer is cleared
					
					ldx		#STRNewLine
                	lbsr	WriteString					
					lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
mnMemSize:			ldx		#STRMemSize1
                	lbsr	WriteString

					
                    ;ldd		RamSize									
					;lbsr	WriteHexByte

					;tfr		b,a
					;lbsr	WriteHexByte
					
					ldd		RamSize						;	Get the total RAM available
					std		decimalbuffer+2				;	store D to display
                                                    
					clr		decimalbuffer
					clr		decimalbuffer+1
					ldx		#decimalbuffer
					lbsr	WriteDecimalBytes			;	print the 4 bytes in decimal, decimalbuffer is cleared
					
					ldx		#STRMemSize2
                	lbsr	WriteString
					
					;ldx		#STRNewLine
                	;lbsr	WriteString					
					lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	LIST Command
mnList:				lda		MagicCartHigh				;	Check if a cartidge is inserted		
					cmpa	#'A'
					lbne	_mnListFailed				;	no
					
					lda		MagicCartLow
					cmpa	#'P'
					lbne	_mnListFailed				;	no
					
					ldy		#AutoCartEntry				;	Y = $8002				
_mnListLoop:		leax	CA_Name,y					;	display the name
					lbsr	WriteString	
					
					lda		#$09						;	tabulation
					lbsr	PutChar
					lda		#'$'						;	Hexa symbol
					lbsr	PutChar
										
					ldd		CA_Run,y					;	print the start address
					lbsr	WriteHexByte
					
					tfr		b,a
					lbsr	WriteHexByte
					
                    lda		#$09						;	Space separator
					lbsr	PutChar

                    ;	DATE: DDDDDDDM.MMMDDDDD
					ldd		CA_Date,y					;	Write year as YYYY
					anda	#%11111110
					lsra
					tfr		a,b
                    clra
					addd	#1980

					std		decimalbuffer+2				;	store D to display
					clr		decimalbuffer
					clr		decimalbuffer+1
					ldx		#decimalbuffer
					lbsr	WriteDecimalBytes			;	print the 4 bytes in decimal, decimalbuffer is cleared
				
            
					lda		#STRDateSep
					lbsr	PutChar	
					
					ldd		CA_Date,y					;	Write month as MM
					lsrb	
					lsrb	
					lsrb	
					lsrb	
					lsrb	
					stb		save1_reg8bits
					anda	#%00000001
					lsla	
					lsla	
					lsla							
					ora		save1_reg8bits
					sta		decimalbuffer+3					
					ldx		#decimalbuffer
					lbsr	WriteDecimal2D				;	print the 4 bytes in decimal, decimalbuffer is cleared

					lda		#STRDateSep
					lbsr	PutChar	
					
					ldd		CA_Date,y					;	Write day as DD
					andb	#%00011111
					stb		decimalbuffer+3					
					ldx		#decimalbuffer
					lbsr	WriteDecimal2D				;	print the 4 bytes in decimal, decimalbuffer is cleared
                    
					lda		#$09						;	Space separator
					lbsr	PutChar
					
					;	TIME:	HHHHHMMM.MMMSSSSS
					ldd		CA_Time,y					;	Write Hour as HH
					lsra	
					lsra	
					lsra	
					sta		decimalbuffer+3
					clra
					ldx		#decimalbuffer
					lbsr	WriteDecimal2D				;	print the 4 bytes in decimal, decimalbuffer is cleared
                    
					lda		#STRTimeSep
					lbsr	PutChar	                    
					
					ldd		CA_Time,y					;	Write Minute as MM
					lsrb
					lsrb
					lsrb
					lsrb
					lsrb
					stb		save1_reg8bits
					anda	#%00000111
					lsla	
					lsla	
					lsla	
					ora		save1_reg8bits
					sta		decimalbuffer+3
					clra					
					ldx		#decimalbuffer
					lbsr	WriteDecimal2D				;	print the 4 bytes in decimal, decimalbuffer is cleared
					
					
_mnList2:           ldx		#STRNewLine
					lbsr	WriteString	
					
					ldd		CA_Next,y
					tfr		d,y
					
					cmpy	#$0000
					beq		_mnListEnd
					lbra	_mnListLoop
					
_mnListEnd:			lbra	Mainloop
					
_mnListFailed:		ldx		#STRListFailed
					lbsr	WriteString	
                	lbra	Mainloop
					
;	----------------------------------------------------------------------------------------------------
;	RUN Command
mnRun:				lbsr	SkipSpace					;	skip space from current X									
					lbsr	ReadHexFromString			;	read hexa address pointed by X and store it to D
					tfr		d,y							;	store D in Y

                    jsr     0,y                    		;	Jump to this address given to run the program
                    lbra	Mainloop
					
;	----------------------------------------------------------------------------------------------------
;	LOAD Command
mnLoad:				lbsr	SkipSpace					;	skip space from current X									
					lbsr	ReadHexFromString			;	read hexa address pointed by X and store it to D
					tfr		d,y							;	store D in X, the start address where upload the program

					ldx		#STRLoading
					lbsr	WriteString	
												
					lbsr	WriteHexByte				;	write the byte of A

					tfr		b,a							;	write the byte of B
					lbsr	WriteHexByte
                                 					
					ldx		#STRNewLine
					lbsr	WriteString	
									
														;	Y = start address
					lbsr	LoadCodeSerial				;	Code loading form Serial port RS-232					
					
                    lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	PEEK Command
mnRead:				lbsr	SkipSpace					;	skip space from current X									
					lbsr	ReadHexFromString			;	read hexa value pointed by X and store it to D
					tfr		d,x							;	store D in X
					
					lda		#'$'						;	symbol for data to display
					lbsr	PutChar						;	display
					lda		,x							;	load A with the content at the X address
					lbsr	WriteHexByte				;	display A
					
                                            
					lda		#' '						;	separator
					lbsr	PutChar						;	display					
                        
					lda		#'b'						;	symbol for data to display
					lbsr	PutChar						;	display
					lda		,x							;	load A with the content at the X address
					lbsr	WriteBinByte				;	display A
							
					ldx		#STRNewLine
                	lbsr	WriteString	
					
					lbra	Mainloop

;	----------------------------------------------------------------------------------------------------
;	POKE Command
mnWrite:			lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read hexa value to poke pointed by X and store it to D
					;stb		save1_reg8bits					;	only LSB in B will be used
					pshs	b
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnWrite2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnWrite2:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					
					tfr		d,x							;	store D in X
					puls	b
					stb		0,x
					
					ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop
					
;	----------------------------------------------------------------------------------------------------
;	DUMP Command
mnDump:				lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read hexa value = count and store it to D
					stb		save_count					;	only LSB in B will be used
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnDump2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnDump2:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					
					tfr		d,y							;	store D (adress start) in Y										
					
_mnDump3:			ldb		save_count					;	reload the count
					cmpb	#0
					beq		_mnDumpDone
					
					decb								;	decrement count in B
					stb		save_count					;	save count
						
					lda		#'$'						;	symbol for data to display
					lbsr	PutChar						;	display
					
					tfr		y,d
					lbsr	WriteHexByte				;	display upper D = A
					
					tfr		b,a
					lbsr	WriteHexByte				;	display lower D = B

					lda		#':'						;	symbol separator
					lbsr	PutChar						;	display
					
					lda		#'$'						;	symbol for data to display
					lbsr	PutChar						;	display
					lda		,y							;	load A with the content at the X address
					lbsr	WriteHexByte				;	display A
					
                                            
					lda		#' '						;	separator
					lbsr	PutChar						;	display					
                        
					lda		#'b'						;	symbol for data to display
					lbsr	PutChar						;	display
					lda		,y							;	load A with the content at the X address
					lbsr	WriteBinByte				;	display A
							
							
					lda		#' '						;	separator
					lbsr	PutChar						;	display	
					lda		,y							;	load A with the content at the X address
										
					cmpa    #$20                    	;   is the char <= SPACE ?
                    ble     _mnDump4		          	;   yes		
					
					cmpa	#$7F						;	is the char >= DEL ?
					bge		_mnDump4					;   yes		
					
                    lbsr	PutChar						;	display
					bra		_mnDump5
					
_mnDump4:			lda		#'.'
					lbsr	PutChar						;	display
					
_mnDump5:			ldx		#STRNewLine
                	lbsr	WriteString					
					
					leay	1,y							;	next value
					bra		_mnDump3
					
_mnDumpDone:		ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop					
					
;	----------------------------------------------------------------------------------------------------
;	COPY Command
mnCopy:				lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read hexa value = count and store it to D
					stb		save_count					;	only LSB in B will be used
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnCopy2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnCopy2:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					
					tfr		d,y							;	store D (adress source) in Y										
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnCopy3
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnCopy3:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					
					tfr		d,x							;	store D (adress destination) in X	
					
                    ldb		save_count					;	reload the count
_mnCopy4:			cmpb	#0
					beq		_mnCopyDone
					
					decb								;	decrement count in B
						
                    lda     ,y
                    sta     ,x
                    
					leay	1,y							;	next value Y = Y +1
					leax	1,x
					bra		_mnCopy4
					
_mnCopyDone:		ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop					

;	----------------------------------------------------------------------------------------------------
;	ORI Command
mnOri:				lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read mask to set pointed by X and store it to D
					;stb		save1_reg8bits					;	only LSB in B will be used										
					pshs	b
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnOri2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop

_mnOri2:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					
					tfr		d,x							;	store D in X
					puls	b
					orb		,x
					stb		,x
					
                	ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop

;	----------------------------------------------------------------------------------------------------
;	ANDI Command
mnAndi:				lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read mask to set pointed by X and store it to D
					;stb		save1_reg8bits					;	only LSB in B will be used
					pshs	b
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnAndi2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop

_mnAndi2:			lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					
					tfr		d,x							;	store D in X
					puls	b
					andb	,x
					stb		,x
					
                	ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop
					
;	----------------------------------------------------------------------------------------------------
;	SW1 Command
mnSw1:              swi
					lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	SW2 Command
mnSw2:              swi2
					lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	SW3 Command
mnSw3:              swi3
					lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	BSET Command
mnBset:				lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read hexa value as bit number to set pointed by X and store it to D
					
					cmpd	#8
					bpl     _mnBset1
					stb		save1_reg8bits					;	only LSB in B will be used as bit number
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnBset2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnBset1:			ldx		#STRHow						;	bit number incorrect
					lbsr	WriteString					
                	lbra	Mainloop			

_mnBset2:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					tfr		d,x							;	store D in X
					
					lda		save1_reg8bits					;	A = bit number
					ldb		#$01						;	B = mask
_mnBset3:			cmpa	#0
					beq		_mnBset4
					lslb	
					deca
					bra		_mnBset3
					
_mnBset4:          	orb		,x							;	B= B OR (x) 
					stb		,x							;	(x) = B

					ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop

					lbra	Mainloop

;	----------------------------------------------------------------------------------------------------
;	BCLR Command
mnBclr:				lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read hexa value as bit number to set pointed by X and store it to D
					
					cmpd	#8
					bpl		_mnBclr1
					
					stb		save1_reg8bits					;	only LSB in B will be used as bit number
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnBclr2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnBclr1:			ldx		#STRHow						;	bit number incorrect
					lbsr	WriteString					
                	lbra	Mainloop	
					
_mnBclr2:			lbsr	SkipSpace					;	skip space from current X	
					lbsr	ReadHexFromString			;	read hexa adr value pointed by X and store it to D
					tfr		d,x							;	store D in X
					
					lda		save1_reg8bits					;	A = bit number
					ldb		#$FE						;	B = mask
_mnBclr3:			cmpa	#0
					beq		_mnBclr4
					rolb	
					deca
					bra		_mnBclr3
					
_mnBclr4:          	andb	,x							;	B = B AND (x) 
					stb		,x							;	(x) = B

					ldx		#STRDone
					lbsr	WriteString
					lbra	Mainloop

					lbra	Mainloop

;	----------------------------------------------------------------------------------------------------
;	PIA DATA REGISTER Command
mnPiadr:			lbsr	SkipSpace					;	skip space from current X	
					lda		,x+							;	check the port
					cmpa	#'A'						;	is port A requested ?
					beq		mnPiadr1					;	Yes port A, else port B
							
					cmpa	#'B'
					bne		mnPiadr6
						
					ldy		#PiaCRB
					sty		Pia_CR						;	contains the correct CR according to port A or B
					ldy		#PiaDDRB
					sty		Pia_DDR						;	contains the correct DDR according to port A or B
					
					bra		mnPiadr2
					
mnPiadr1:			ldy		#PiaCRA
					sty		Pia_CR						;	contains the correct CR according to port A or B
					ldy		#PiaDDRA
					sty		Pia_DDR						;	contains the correct DDR according to port A or B
					
mnPiadr2:			lda		,x+							;	check the separator
					cmpa	#','
					bra     mnPiadr3
                    
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
mnPiadr3:			lda		,x+							;	check the separator
					cmpa	#'I'
					beq		mnPiadr4

					ldb		#PIA_OUTUTS					;	PINs will be in Output
					bra		mnPiadr5
					
mnPiadr4:			ldb		#PIA_INPUTS					;	PINs will be in Input

					
mnPiadr5:			lda		#$FB						;	clear bit DDR to access to DDR register
					anda	[Pia_CR]
					sta		[Pia_CR]
					
					stb		[Pia_DDR]					;	set Direction register, b contains the good value
					
					
                    lda		#$04						;	Set the DDR bit to enable next access to Data Register
					ora		[Pia_CR]
					sta		[Pia_CR]
					
					ldx		#STRDone
					lbsr	WriteString
                    lbra	Mainloop
					
mnPiadr6:			ldx		#STRWhat					;	Misunderstood: isn't port A or B....
					lbsr	WriteString
                    lbra	Mainloop
	
;	----------------------------------------------------------------------------------------------------
;	PIA RD Command
mnPiard:			lbsr	SkipSpace					;	skip space from current X	
					lda		,x+							;	check the port
					cmpa	#'A'						;	is port A requested ?
					beq		mnPiard1				

					cmpa	#'B'
					bne		mnPiard3
					
					ldy		#PiaORB
					bra		mnPiard2
					
mnPiard1:			ldy		#PiaORA

mnPiard2:			lda		#'$'						;	symbol for data to display
					lbsr	PutChar						;	display
					lda		0,y							;	load A with the content of PIA DDRx
					lbsr	WriteHexByte				;	display A
					
                                            
					lda		#' '						;	separator
					lbsr	PutChar						;	display					
                        
					lda		#'b'						;	symbol for data to display
					lbsr	PutChar						;	display
					lda		0,y							;	reload A with the content of PIA DDRx
					lbsr	WriteBinByte				;	display A
							
					ldx		#STRNewLine
                	lbsr	WriteString
					
					lbra	Mainloop
					
mnPiard3:			ldx		#STRWhat
					lbsr	WriteString
                    lbra	Mainloop					
;	----------------------------------------------------------------------------------------------------
;	PIA WR Command
mnPiawr:			lbsr	SkipSpace					;	skip space from current X				
					lbsr	ReadHexFromString			;	read hexa value to write pointed by X and store it to D
					;stb	save1_reg8bits					;	only LSB in B will be used
					
					lda		,x+							;	check the separator
					cmpa	#','
					beq		_mnPiawr2
					
					ldx		#STRSyntErr					;	failed
					lbsr	WriteString					
                	lbra	Mainloop
					
_mnPiawr2:			lbsr	SkipSpace					;	skip space from current X	
					lda		,x+							;	get the correct PIA port A or B to write
					cmpa	#'A'
					beq		_mnPiawr3
				
					cmpa	#'B'
					bne		_mnPiawr5
					
					ldy		#PiaORB
					bra		_mnPiawr4
					
_mnPiawr3:			ldy		#PiaORA

_mnPiawr4:			stb		0,y							;	B already contains the value to write
					
					ldx		#STRDone
					lbsr	WriteString
                    lbra	Mainloop
					
_mnPiawr5:			ldx		#STRWhat
					lbsr	WriteString
                    lbra	Mainloop					
;	----------------------------------------------------------------------------------------------------
;	MEMTEST Command
mnMemTest:			ldx		#STRMemTest
					lbsr	WriteString
					lbsr	MemTest
					lbra	Mainloop
;	----------------------------------------------------------------------------------------------------
;	MEMTEST Command
mnClear:			ldx		#STRClear
					lbsr	WriteString
					lbra	Mainloop								

;	----------------------------------------------------------------------------------------------------
;	----------------------------------------------------------------------------------------------------
;	Write a string on the ACIA/Terminal Console
;	X = address of the string to write

WriteString			pshs	a,x,cc
WSloop:				lda		,x+
					beq		WS_done
					lbsr	PutChar
					bra		WSloop
					
WS_done:			puls	a,x,cc
					rts

;	----------------------------------------------------------------------------------------------------
; Read a string from the ACIA/Terminal Console
; arguments:	X = pointer to string buffer
;				B = maximum character count in B
; returns:		string copied to buffer
; destroys:		A,B

ReadString:			pshs	x					;	save buffer origin

					decb						;	leave room for null char.
					abx							;	save buffer end
					pshs	x
					ldx		RDSTRBUFSTART,s		;	restore buffer origin
					
rl_getchar:			lbsr	GetCharUntil
					cmpa	#NLCHAR				;	return could be CR or LF
					beq		rl_linedone
					
					cmpa	#CRCHAR
					beq		rl_linedone
		
					cmpa	#BACKSPACECHAR		;	handle delete or backspace
					beq		rl_deletechar
		
					cmpa	#DELETECHAR
					beq		rl_deletechar
					
					; lbsr	VALIDATE_ALL		;	validate character
					; bvs		rl_getchar
					
rl_storechar:		cmpx	RDSTRBUFEND,s		;	max amount of characters typed?
					bge		rl_getchar			;	yes, don't store character
					
					lbsr	PutChar				;	echo character
					sta		,x+					;	store char in buffer
					bra		rl_getchar
					
rl_deletechar:		cmpx	RDSTRBUFSTART,s		;	don't delete if at first char
					beq		rl_getchar
		
					lda		#$08
					lbsr	PutChar				;	send delete sequence (\b space \b)
		
					lda		#$20
					lbsr	PutChar
		
					lda		#$08
					lbsr	PutChar
		
					lda		#0					;	overwrite last char with 0
					sta		,-x
					bra		rl_getchar
					
rl_linedone:		lda		#0					;	null-terminate the string
					sta		,x+
					
					leas	2,s					;	throw away end address
					puls	x					;	restore X
					
					rts

;	----------------------------------------------------------------------------------------------------
;	register: X = string where to skip space
;	output: X = next character not equal to SPACE

SkipSpace:    		lda    ,x
					cmpa   #SPACE
					bne    SkipSpaceEnd
					leax   1,x   
					bra    SkipSpace
SkipSpaceEnd:  		rts

					
;	----------------------------------------------------------------------------------------------------
;	compare the strings
;	register: X = prompt line string
;	register: Y = reference string
;	CCR.Z = 1 if equal else 0
;	output:	x point to the next character to compare after the recognized sub-string reference
;
;	ex:	X = "READ FFFE"		Y = "READ"

CompareString:		pshs	a,y

_CompareString:		lda		,y+						;	get a character from reference string
					cmpa	#0						;	compare to EOS
					beq		_CMPStringdone			;	YES, end of string
					
					cmpa	,x+						;	compare this ref character with the entered
					bne		_CMPStringfailed		;	different
										
					bra		_CompareString			;	continue
					
_CMPStringfailed:	puls	a,y
					andcc	#$FB					;	clear bit Z					
					rts
					
_CMPStringdone:		puls	a,y
					orcc	#$04					;	set bit Z			
					rts			

;	----------------------------------------------------------------------------------------------------
;	compare the strings
;	register: X = string where to find inside the second
;	register: Y = second string
;	CCR.Z = 1 if equal else 0

StartWithString:	pshs	a,y

_StartWithString:	lda		,x+
					cmpa	,y+
					bne		_StartWithfailed
					
					cmpa	#0                      ;   end if null termintated
					beq		_StartWithdone
                    
                    cmpa	#' '                    ;   end if space
					beq		_StartWithdone
                    
					bra		_StartWithString
				
_StartWithfailed:	puls	a,y
					andcc	#$FB					;	clear bit Z					
					rts
					
_StartWithdone:		puls	a,y
					orcc	#$04					;	set bit Z			
					rts			
					
;	----------------------------------------------------------------------------------------------------
;	Write a byte in hexadecimal on the ACIA/Terminal Console
;	register: A = byte to display
			
WriteHexByte		pshs	cc
					rora
					rora
					rora
					rora
					bsr		PutHexChar
					rora
					rora
					rora
					rora
					rora
					bsr		PutHexChar
					puls	cc
					rts

;	----------------------------------------------------------------------------------------------------
;	Write a byte in hexadecimal on the ACIA/Terminal Console
;	register: A = byte to display

WriteBinByte:		pshs	b,cc
					ldb		#$80
					
_WriteBinByte1:		stb		save1_reg8bits
                    pshs    a
					anda	save1_reg8bits
					beq		_WriteBinByte2			;	bit = 0
					
					lda		#'1'
					lbsr	PutChar
					puls	a
					bra		_WriteBinByte3
					
_WriteBinByte2:     lda		#'0'
					lbsr	PutChar
					puls	a

_WriteBinByte3:		lsrb							;	A >> 1		    
					beq		_WriteBinByte			;	A = 0 ?
					bra		_WriteBinByte1
					
_WriteBinByte:		puls	b,cc
					rts
					
;	----------------------------------------------------------------------------------------------------
;	Convert the LSB content of register A to ASCI representation and call PutChar to display it
PutHexChar			pshs	a,cc
					anda	#$0F
					adda	#'0'
					cmpa	#'9'
					ble		_PutHexChar1
					
					adda	#7
_PutHexChar1		lbsr	PutChar
					puls	a,cc
					rts

;	----------------------------------------------------------------------------------------------------
;	Write on the terminal, the content of the last byte of the buffer (4-bytes) pointed by X
;	Print only 2 digit on this end byte
;	Number at that location is destroyed by the process.

WriteDecimal2D:		pshs	a,b,x,y,u
							
					lbsr 	bin2bcd             ;	Convert to bcd

        			ldx 	#bcdbuf             ;	Traverse 5-byte buffer.
					leax	4,x					;	Point to the last byte
					
					lda 	,x
                    
					lbsr	WriteHexByte		;	FIX: More quick to reuse this routine
					
					;pshs    a
					;lsra
					;lsra
					;lsra
					;lsra                        ;	Extract higher digit from bcd byte.
					;adda 	#'0'								
					;lbsr	PutChar				;	A contents a character > '0'
					
					;puls    a
					;anda 	#$0F				;	Extract lower digit.
					;adda 	#'0'	                    
					;lbsr 	PutChar						
						
					lda 	#4				
_WriteDecimal2D1:	clr 	,x+					;	Clear the 4-byte buffer.
					deca
					bne 	_WriteDecimal2D1
										
					puls	a,b,x,y,u
										
					rts
					
;	----------------------------------------------------------------------------------------------------
;	Write on the terminal, the content of the buffer (4-bytes) pointed by X
;	Print double number (including leading zeros)  pointed to by X.
;	Number at that location is destroyed by the process 
;   Works ONLY WITH X POINTING RAM ZONE !!

WriteDecimalBytes:	pshs	a,b,x,y,u
							
					bsr 	bin2bcd             ;	Convert to bcd

        			ldx 	#bcdbuf             ;	Traverse 5-byte buffer.
					ldb		#5
					stb		save1_reg8bits
					clr		save3_reg8bits		;	count of digit printed
				
					ldb		#'0'				;	set '0' to say previous BCD was '0'
pdloop:				lda 	,x+
                    pshs    a

					lsra
					lsra
					lsra
					lsra                        ;	Extract higher digit from bcd byte.

					adda 	#'0'				
					cmpa	#'0'
					beq		_WriteDecimalByte1	;	A contents the '0' to display
					
					lbsr	PutChar				;	A contents a character > '0'
					inc		save3_reg8bits
					tfr		a,b					;	Memorize the new previous is not '0'
					bra		_WriteDecimalByte2
					
_WriteDecimalByte1:	cmpb	#'0'				;	If B contents '0', previous char was '0' too. Not display A
					beq		_WriteDecimalByte2				
					lbsr     PutChar			;	Display the '0' in A, because previous char was not '0'
					inc		save3_reg8bits
						
_WriteDecimalByte2: puls    a

					anda 	#$0F				;	Extract lower digit.
					adda 	#'0'	
                    cmpa    #'0'
                    beq     _WriteDecimalByte3
					
					lbsr 	PutChar
					tfr		a,b					;	FIX 
					inc		save3_reg8bits
					
                    bra     _WriteDecimalByte4

_WriteDecimalByte3: cmpb	#'0'				;	If B contents '0', previous char was '0' too. Not display A
					beq		_WriteDecimalByte4				
					lbsr     PutChar			;	Display the '0' in A, because previous char was not '0'
					inc		save3_reg8bits

_WriteDecimalByte4:	dec	 	save1_reg8bits				
					bne 	pdloop
															
													
					;clra	
					;sta		0,x
					;sta		1,x
					;sta		2,x
					;sta		3,x
							
					lda		save3_reg8bits		;	how many digit printed
					cmpa	#0					;	Zero ?
					bne		_WriteDecimalByte7	;	no, >= 1
					
					lda		#'0'
					lbsr     PutChar			;	Print at least one Zero 
						
_WriteDecimalByte7:	lda 	#4	
                    leax    -4,x
                    
_WriteDecimalByte8:	clr 	,x+					;	Clear the 4-byte buffer.
					deca
					bne 	_WriteDecimalByte8
										
					puls	a,b,x,y,u
										
					rts
					
;	----------------------------------------------------------------------------------------------------
;	Convert 4-byte number pointed to by X to 5-byte (10 digit) bcd.

bin2bcd:			ldu 	#bcdbuf

					ldb 	#5				
bbclr:				clr 	,u+				;Clear the 5-byte bcd buffer.
					decb
					bne 	bbclr
					
					ldb 	#4				;traverse 4 bytes of bin number 
					stb 	save1_reg8bits
					
bbloop:				ldb 	#8				;and 8 bits of each byte. (msb to lsb)
					stb 	save2_reg8bits
bbl1:				rol 	,x				;Extract next bit from binary number.

					ldb 	#5
					ldu 	#bcdbuf+5
bbl2:				lda 	,-u				;multiply bcd number by 2 and add extracted bit
					adca 	,u				;into it. 
					daa
					sta 	,u
					
					decb
					bne 	bbl2
					
					dec 	save2_reg8bits
					bne 	bbl1
					
					leax 	1,x
					dec 	save1_reg8bits
					bne 	bbloop
					
					rts
	
;	----------------------------------------------------------------------------------------------------
; send to the UART the content of register A
PutChar				pshs	a
_PutChar1			lda		Uart
					bita	#$02
					beq		_PutChar1
					puls	a
					sta		UartTDR
					rts

;	----------------------------------------------------------------------------------------------------
; Wait and get from the Uart a new char in register A
GetCharUntil:		lda		Uart
					bita	#$01
					beq		GetCharUntil
					
                    lda		UartRDR                 ;   read the char
                    
                    cmpa    #'a'                    ;   is the char >= 'a' ?
                    bge     _GetCharUntil1          ;   yes
                    bra     GetCharUntilEnd
                        
_GetCharUntil1:     cmpa    #'z'                    ;   is the char <= 'z'
                    ble     _GetCharUntil2          ;   yes
                    bra     GetCharUntilEnd
                    
_GetCharUntil2:     suba    #32                     ;   usbstract 32 to have uppercase
                    
GetCharUntilEnd:	rts

;	----------------------------------------------------------------------------------------------------
; Try to get from the Uart a new char in register A 
;	CCR.Z = 1 if new char

GetChar:			lda		Uart				
					bita	#$01
					beq		GetCharFailed
										
					lda		Uart+1
					orcc	#$04					;	set bit Z	
					rts
					
GetCharFailed:		andcc	#$FB					;	clear bit Z	
					rts

;	----------------------------------------------------------------------------------------------------
;;; read hex digits from the string in X into a 16-bit integer
;;; stops after the first invalid character
;;; arguments:	string pointer in X
;;; returns:	value in D
;;; destroys:	X advanced
ReadHexFromString:	ldd		#$0000
					pshs	d			;	temporary result is on stack
readhexdigit:		ldb		,x+			;	get a character
					cmpb	#'0'		;	is it a decimal digit?
					blo		nothex
					cmpb	#'9'
					bhi		testaf
					subb	#'0'		;	it's a decimal digit
					bra		addhexdigit	;	we're good
testaf:				cmpb	#'A'		;	is it between A and F?
					blo		nothex
					cmpb	#'F'
					bhi		testaflower
					subb	#55
					bra		addhexdigit
testaflower:		cmpb	#'a'
					blo		nothex
					cmpb	#'f'
					bhi		nothex
					subb	#87
addhexdigit:		lsl		1,s		;	multiply temporary by 16
					rol		,s
					lsl		1,s
					rol		,s
					lsl		1,s
					rol		,s
					lsl		1,s
					rol		,s
					orb		1,s			;	or digit into lower nibble
					stb		1,s
					bra		readhexdigit
nothex:				leax	-1,x		;	back up x
					puls	d			;	pop result into D
					andcc	#$FD		;	clear V
					rts

;	----------------------------------------------------------------------------------------------------
;	Memory test

MemTest:			pshs	a,x
					ldx		RamTop
					
MMloop1:			cmpx	#$0000
					beq		MMSuccess
					
					lda  	0,x
					coma                			
					sta  	0,x
					cmpa 	0,x            				
					bne  	MMError        				
										       			
					com  	0,x            				
					leax 	-1,x     
					bra  	MMloop1

MMSuccess:  		ldx		#STRDone
					lbsr	WriteString
					puls	a,x
					rts
					
MMError:  			ldx		#STRFailed
					lbsr	WriteString					
					puls	a,x
					rts

;	----------------------------------------------------------------------------------------------------
;	
GetHexDigit:		lbsr	GetCharUntil					
					cmpa	#'A'

					blt		_GetHexDigit1
                    
					suba	#55
					bra		_GetHexDigitEnd
					
_GetHexDigit1:		suba	#48
					
										
_GetHexDigitEnd:	anda	#$0F
					rts
					
;	----------------------------------------------------------------------------------------------------
;	Load code from text hex. file on serial port
;	Y = start address
LoadCodeSerial:		bsr		GetHexDigit							;	first byte MSB for length 
					lsla
					lsla
					lsla
					lsla
					sta		save1_reg8bits						;	save 
					
					bsr		GetHexDigit							;	first byte LSB for length
					ora		save1_reg8bits
					sta		save1_reg8bits						;	save 
					
                    lbsr    WriteHexByte

					bsr		GetHexDigit							;	second byte MSB for length 
					lsla
					lsla
					lsla
					lsla
					sta		save2_reg8bits						;	save 
					
                    
					bsr		GetHexDigit							;	second byte LSB for length
					ora		save2_reg8bits
					
                    lbsr    WriteHexByte
                    
					tfr		a,b
					lda		save1_reg8bits						;	read back the MSB
					
					tfr		d,x									;	X contents now the length
					
_LoadCodeSerial1:	cmpx	#$0000
					beq		_LoadCodeSerial2
					
					bsr		GetHexDigit							;	first byte MSB for byte data 
					lsla
					lsla
					lsla
					lsla
					sta		save1_reg8bits						;	save 
					
					bsr		GetHexDigit							;	first byte LSB for byte data 
					ora		save1_reg8bits						;	Now A content the complete byte
					
                    lbsr    WriteHexByte

					sta		0,y+								;	store the byte to the location
					
					leax	-1,x								;	count to upload - 1
					bra		_LoadCodeSerial1
					
_LoadCodeSerial2:	ldx		#STRNewLine
					lbsr	WriteString
                    
                    ldx		#STRDone
					lbsr	WriteString
					
					rts

;	----------------------------------------------------------------------------------------------------
;	SWI Software Interrupt: Display registers on Console

Vector_swi:			ldx		#system_sw1
					lbsr	WriteString

					rti

;	----------------------------------------------------------------------------------------------------
;	Interrupt handler
Vector_irq:			
					ldx		#system_irq
					lbsr	WriteString
					
					rti
;	----------------------------------------------------------------------------------------------------
;	Interrupt handler
Vector_firq:		
					ldx		#system_firq
					lbsr	WriteString
					
					rti
;	----------------------------------------------------------------------------------------------------
;	Interrupt handler
Vector_nmi:
					ldx		#system_nmi
					lbsr	WriteString
					
					rti
;	----------------------------------------------------------------------------------------------------
;	Interrupt handler
Vector_swi2:		
					ldx		#system_sw2
					lbsr	WriteString
					
					rti
;	----------------------------------------------------------------------------------------------------
;	Interrupt handler
Vector_swi3:		
					ldx		#system_sw3
					lbsr	WriteString
					
					rti
;	----------------------------------------------------------------------------------------------------
;	Interrupt handler reserved for Motorola
Vector_reserved:	
					rti

;	----------------------------------------------------------------------------------------------------
;   remember:
;   \015 = $0D  return
;   \012 = $0A  newline

STRSystemStart1		fcc	"\033ERedBoard-2 6809 16kB\015\012\0"
STRSystemStart2		fcc	"Favard Laurent 2003-2025\015\012\0"

STRPromptCLI		fcc	"CLI>\0"
STRNewLine			fcc	"\015\012\0"
STRClear            fcc "\033E\0"
STRMemCalculated	fcc	"End of memory: $\0"
;STRMemSize			fcc "Size bytes: $\0"
STRMemSize1			fcc "Available: \0"
STRMemSize2			fcc " bytes\015\012\0"
STRSystemReady		fcc	"Ready\015\012\0"
STRVersion			fcc	"Version: \0"
STRDiagCartOk		fcc	"DiagCartridge inserted\015\012\0"
STRAutCartOk		fcc	"AutoCartridge inserted\015\012\0"
STRAppCartOk		fcc	"ApplCartridge inserted\015\012\0"
STRMemTest			fcc "Test running...\015\012\0"
STRList				fcc	"List of applications:\015\012\0"
STRListFailed		fcc	"No applications cartridge\015\012\0"
STRBasic			fcc	"BASIC running\015\012\0"
STRCountApp			fcc	"Count: \0"
STRLoading			fcc	"Loading: at $\0"
;	----------------------------------------------------------------------------------------------------
STRTribute          fcc "In tribute to:\015\012"
                    fcc "Jack Tramiel, Commodore and Atari, 1928-2012\015\012"
                    fcc "Steve Jobs, Apple Computers 1955-2011\015\012"
					fcc	"Dennis Ritchie, Langage C 1941-2011\015\012"
                    fcc "Gary Kildall, Digital Research, 1942-1994\015\012"
                    fcc "Nolan Bushnell, Atari founder\015\012"
					fcc "Steve Wozniak, Apple Computers\015\012"
					fcc "And to Motorola, MOS Technology, Rockwell,...\015\012\0"
                    
;	----------------------------------------------------------------------------------------------------
STRSorry            fcc	"Sorry ?\015\012\0"
STRSyntErr       	fcc	"Syntax error\015\012\0"
STRDone				fcc	"Ok\015\012\0"
STRHow				fcc	"How ?\015\012\0"
STRWhat				fcc	"What ?\015\012\0"
STRFailed			fcc	"Failed\015\012\0"
;	----------------------------------------------------------------------------------------------------
system_irq			fcc	"IRQ !015\012\0"
system_firq			fcc	"FIRQ !\015\012\0"
system_nmi			fcc	"NMI !\015\012\0"
system_sw1			fcc	"SW1 !\015\012\0"
system_sw2			fcc	"SW2 !\015\012\0"
system_sw3			fcc	"SW3 !\015\012\0"
;	----------------------------------------------------------------------------------------------------
STRHelp				fcc "\033E"
                    fcc "[HELP/?] : Commands list\015\012"
                    fcc "[CLS]    : CLear Screen\015\012"
					fcc	"[VER]    : VERsion\015\012"
					fcc	"[MEMSIZE]: Memory size\015\012"
					fcc	"[LIST]   : LIST contents of App Cartridge\015\012"
					fcc "[RUN]    : RUN <AE> Run a program\015\012"
					fcc "[LOAD]   : LOAD <AE> Load a program\015\012"
					fcc	"[PEEK]   : READ <AE>\015\012"
					fcc	"[POKE]   : WRITE <byte>,<AE>\015\012"
					fcc	"[DUMP]   : DUMP <byte>,<AE>\015\012"
					fcc	"[COPY]   : COPY <byte>,<SAE>,<DAE>\015\012"
					fcc	"[ORI]    : ORI <mask>,<AE>\015\012"
					fcc	"[ANDI]   : ANDI <mask>,<AE>\015\012"
					fcc	"[BSET]   : BSET [0-7],<AE>\015\012"
					fcc	"[BCLR]   : BCLR [0-7],<AE>\015\012"
					fcc	"[SETPIA] : PIA set A/B,[I/O]\015\012"
					fcc	"[RDPIA]  : PIA Read A/B\015\012"
					fcc	"[WRPIA]  : PIA Write <byte>,A/B\015\012"
					fcc	"[SW1/2/3]: SW interrupt\015\012"
					fcc	"[MEMTEST]: MEMory TEST\015\012"
;	----------------------------------------------------------------------------------------------------
CMDHelp				fcc "?\0"
CMDHelp2			fcc "HELP\0"
CMDVersion			fcc "VER\0"
CMDMemSize			fcc	"MEMSIZE\0"
CMDClear			fcc "CLS\0"
CMDList				fcc "LIST\0"
CMDRun				fcc "RUN\0"
CMDRead				fcc "PEEK\0"
CMDWrite			fcc "POKE\0"
CMDSw1				fcc "SW1\0"
CMDSw2				fcc "SW2\0"
CMDSw3				fcc "SW3\0"
CMDOri				fcc "ORI\0"
CMDAndi				fcc "ANDI\0"
CMDBset				fcc "BSET\0"
CMDBclr				fcc "BCLR\0"
CMDDump				fcc "DUMP\0"
CMDCopy				fcc "COPY\0"
CMDPIADR			fcc	"SETPIA\0"
CMDPIARD			fcc	"RDPIA\0"
CMDPIAWR			fcc	"WRPIA\0"
CMDMemTest			fcc "MEMTEST\0"
CMDLoad				fcc "LOAD\0"
CMDTribute          fcc "THANKS\0"
;	----------------------------------------------------------------------------------------------------
;	Jmp table for CLI commands
DispatchCommands	fdb 	CMDHelp
					fdb		mnHelp
					fdb 	CMDHelp2
					fdb		mnHelp
					fdb 	CMDVersion
					fdb		mnVersion
					fdb		CMDMemSize
					fdb		mnMemSize
					fdb 	CMDClear
					fdb		mnClear
					fdb		CMDList
					fdb		mnList
					fdb		CMDRun
					fdb		mnRun
					fdb 	CMDRead
					fdb		mnRead
					fdb 	CMDWrite
					fdb		mnWrite
					fdb 	CMDSw1
					fdb		mnSw1
					fdb 	CMDSw2
					fdb		mnSw2
					fdb 	CMDSw3
					fdb		mnSw3
					fdb 	CMDOri
					fdb		mnOri
					fdb 	CMDAndi
					fdb		mnAndi
					fdb 	CMDBset
					fdb		mnBset
					fdb 	CMDBclr
					fdb		mnBclr
					fdb 	CMDDump
					fdb		mnDump
					fdb 	CMDCopy
					fdb		mnCopy
					fdb		CMDPIADR
					fdb		mnPiadr
					fdb		CMDPIARD
					fdb		mnPiard
					fdb		CMDPIAWR
					fdb		mnPiawr
					fdb 	CMDMemTest
					fdb		mnMemTest
					fdb 	CMDLoad
					fdb		mnLoad
                    fdb 	CMDTribute
					fdb		mnTribute
					fdb		$0000						;	end of table
                    
;	----------------------------------------------------------------------------------------------------
;	Monitor functions table exported
FunctionsTable		fdb		PutChar
					fdb		PutHexChar
					fdb		GetChar
					fdb		GetCharUntil
					fdb		WriteHexByte
					fdb		WriteBinByte
					fdb		WriteString
					fdb		ReadString
					fdb		ReadHexFromString
					fdb		$0000						;	end of table
                    
;	----------------------------------------------------------------------------------------------------
;   Jmp table to vectors stored in RAM, excepted RESET

JmpSwi3:            jmp    [Swi3Vector]
JmpSwi2:            jmp    [Swi2Vector]
JmpFirq:            jmp    [FirqVector]
JmpIrq:             jmp    [IrqVector]
JmpSwi:             jmp    [SwiVector]
JmpNmi:             jmp    [NmiVector]

;	----------------------------------------------------------------------------------------------------
;	System vector specification
					
					spaceto ROMVectors				; special LFD directive: fill from last PC = * to here 
					org	ROMVectors
Vectors:			
					fdb		Vector_reserved		
					fdb		JmpSwi3
					fdb		JmpSwi2
					fdb		JmpIrq
					fdb		JmpIrq
					fdb		JmpSwi
					fdb		JmpNmi
					fdb		RomStart            

					end

;	----------------------------------------------------------------------------------------------------
;
;	Global Memory Map (RedBoard-2 6809)
;
;	+---------------+
;	|               | $FFFF 
;	| Boot/Monitor  |		 		|
;	| ROM 16 HB     |  				|
;	|               | 				| 
;	| EEPROM        | 				|	16 KB
;	| C28C256       |  				|
;	|               | 				|		
;	|               | $C000
;	+---------------+ 
;	| NOT USED      | $BFFF			|	4KB
;	|               | $B000			|
;	+---------------+  
;	|  I/O Area (1) | $AFFF			|	4KB
;	|  4 KB         | $A000			|
;	+---------------+  
;	|               | $9FFF 		|	8KB
;	| NOT USED      | $9000	 		|
;	|               | $8FFF 		|
;	|               | $8000 		|
;	+---------------+
;	| Ram Monitor   | $7FFF
;	|...............|
;	|               |
;	|               |
;	|               |
;	|               |
;	|               |
;	|               | 				|
;	|               | 				|
;	|               | 				|	32 KB
;	|               | 				|
;	|               | 				|
;	|               |
;	|               |
;	|               |
;	| User          |		
;	| RAM           |
;	|               |
;	|...............|
;	| Ram Monitor   |
;	+---------------+ $0000 
;
;
;
;
;	I/O area: total 4B reserved
;
;	+---------------+ 
;	|               | $AFFF
;	|  FREE			|		
;	|...............|
;	|  PIA 6821     | 
;	|               | 
;	|               | 
;	|               | $A002 - $A005 (4 registers) No specific function in this monitor
;	|...............|
;	|  ACIA 6850    | 
;	|      			| $A000 - $A001 (2 registers) Terminal connection 115200 Baud serial interface
;	+---------------+ 
