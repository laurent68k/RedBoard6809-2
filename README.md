# Alix-32

This project is the continuation of RedBoard-1, a 6809-based SBC. The difference between the both projects:

- Using GAL 22V10 instead of TTL circuits for address decoding
- 16 KB ROM vs 8KB to handle CocoBasic Microsoft (Comming from Grant's SBC project), my Monitor or any other.
- Fixed UART 115200 bauds communication via the CPU 7.3728 MHz crystal (removed 4060 frequency divider)
- MAX 232 removed for a modern FTDI232 UART-USB adapter.
- Optional second UART 68B50
- Optional PTM 68B40 asociated with a HCT4060
- Optional second GAL 16V8 to generate a cartdridge /CSCARD
- 4 expansions slots

# A name

"Alix-32" could be the name of this small project, drawn from the French language for "Appareil Léger d'Informatique eXpérimentale", 
a nod to the French clone of the TRS-80 MC-1, the Alice.

## Status, Work in progress

- EKEmulator works (updated to the lastest Xcode and macOS)
- i6809asm works (updated to the lastest Xcode and macOS)
- CocoBasic can be assembled correctly. Tested and run on EKEmulator.
- EKMonitor16kB can be assembled correctly. Tested and run on EKEmulator.

The schematic is almost complete, and the printed circuit board is nearly finalized.

## Specification

- 16KB ROM. On the PCB I use a 28C256 and a jumper to have a 2x16KB bank select.
- 32KB RAM
- 68B09P Processor with a 7.3728MHz crystal (1.8432MHz clock inside the CPU)
- 68B50 UART used at 115200 Baud to communicate with a computer through a FTDI232. Fixed by the CPU crystal.
- 68B21 PIA
- 4 free Chip select
- 4 Expansion slots
- Optional second 68B50 UART. In my case, I would like to use it with a Commodore C64 keyboard/Arduino Micro pro.
- Optional 68B40 PTM and HCT4060 to generate a system time counter on /NMI
- Optional second GAL 16V8 to add a cardridge port chip select /CSCARD whithin a space of 4KB


## Memory map

Addr.	        Size	     Signal CS	Périphérique
$0000 - $7FFF	32 KB	    /CSRAM	    RAM
$8000 - $BBFF	15 KB     Not decoded
$BC00 - $BC0F	16 bytes	/CSUSB	USB
$BC10 - $BC1F	16 bytes	/CSC64	C64/UART
$BC20 - $BC2F	16 bytes	/CSPIA	PIA
$BC30 - $BC3F	16 bytes	/CSPTM	PTM
$BC40 - $BF3F	768 bytes	Not decoded
$BF00 - $BF3F	64 bytes	/CSFREE0	Libre 0
$BF40 - $BF7F	64 bytes	/CSFREE1	Libre 1
$BF80 - $BFBF	64 bytes	/CSFREE2	Libre 2
$BFC0 - $BFFF	64 bytes	/CSFREE3	Libre 3
$C000 - $FFFF	16 KB	    /CSROM	  ROM


FFFF +----------------------+
     |        ROM           |
C000 |      /CSROM          |
     +----------------------+

BFFF +----------------------+
     | FREE3  /CSFREE3      |
BFC0 +----------------------+
     | FREE2  /CSFREE2      |
BF80 +----------------------+
     | FREE1  /CSFREE1      |
BF40 +----------------------+
     | FREE0  /CSFREE0      |
BF00 +----------------------+
BF3F |                      |
     |                      |
     |   Free area          |
BC40 |                      |
BC3F +----------------------+
     | PTM    /CSPTM        |  PTM 6840
BC30 +----------------------+
     | PIA    /CSPIA        |  PIA 6821
BC20 +----------------------+
     | C64    /CSC64        |  UART 6850 2 Optionnal.
BC10 +----------------------+
     | USB    /CSUSB        |  UART 6850 1
BC00 +----------------------+
BBFF |                      |
     |   Not decoded        |
8000 |                      |
7FFF +----------------------+
     |                      |
0000 | RAM      /CSRAM      |
     +----------------------+

## Glue logic

- A PLD 22V10 Logic is used to decode the address bus with a range of 12 bits A15...A4 and provide all necessary /CS.
- The second GAL 16V8 can be used to decode the cartdridge space optionnally. I you don't want that, do not install it and ignore in your monitor anything about this space.

## Extend memory map with cardridge

- The second optional GAL/PAL 16V8 allows to drive a new /CS for the cardridge port. In this case, I choose to add 4Kb.

     ...
     |                      |
BC10 +----------------------+
     | USB    /CSUSB        |  UART 6850 1
BC00 +----------------------+
BBFF |                      |
9000 |   Not decoded        |
     +----------------------+
8FFF |                      |
     |     CARDRIDGE        |  CARD 4Kb added
8000 |                      |
7FFF +----------------------+
     |                      |
0000 | RAM      /CSRAM      |
     +----------------------+
     
## CocoBasic ROM

Make CocoBasic.bin :

- Open a BASH terminal 
- Enter to the folder CocoBasic
- Type: $ ./i6809asm CocoBasic.asm -bin
- This will produce a CocoBasic.bin file

CocoBasic.bin file produced is ready for your EEPROM burner with the correct size of 16384 bytes for a 16kB device.

## EKMonitor16kB ROM

EKMonitor16kB is my own monitor in 6809 assembly language. Currently, comming from my first project RedBoard-1.

Make EKMonitor.bin :

- Open a BASH terminal 
- Enter to the folder EKMonitor16kB
- Type: $ ./i6809asm EK6809Monitor.asm -bin
- This will produce a EK6809Monitor.bin file

EK6809Monitor.bin file produced is ready for your EEPROM burner with the correct size of 16384 bytes for a 16kB chip.

## i6809asm

This program is the 6809 assembler running on macOS used to assemble the source code of CocoBasic and EKMonitor. It is derivated from the AS9 of the Grant's page. 

I put a copy of this i6809asm executable in each folder CocoBasic and EKMonitor. The i6809 assembler source code is available in my Github. Currently it is adapted to Xcode 16.2.

## EKEmulator

EKEmulator is my own personnal emulator developed to run CocoBasic or EKMonitor ROM. This program run only on macOS and has been code in Objective-C.

This emulator has been updated to Xcode 16.2 and macOS Sequoia (15.5).

## Terminal with Screen and Putty

### macOS

Open a BASH terminal and type : 

$ screen /dev/my_usb_dev 115200

For example :

screen /dev/tty.PL2203 115200

Prolific adapter/RedBoard-1:    screen /dev/cu.usbserial-1440 9600
FTDI UART-USB:                  screen /dev/cu.usbserial-A5069RR4 115200

### Windows

I currently don't use Windows to communicate.


## Usefull links

Grant's 6-chip 6809 computer: Where I found CocoBasic and AS9. Very good page.
http://searle.x10host.com/6809/Simple6809.html


## Licence

This project is free and can be reused as you want without any restrictions.

2002/2026 FAVARD Laurent 6809 projects.
