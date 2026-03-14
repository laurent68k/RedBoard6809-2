# RedBoard6809-2

This project is the continuation of RedBoard-1, a 6809-based SBC. The difference between the both projects:

- Using GAL 22V10 instead of TTL circuits for address decoding
- 16 kB ROM vs 8kB to handle CocoBasic Microsoft (Comming from Grant's SBC project), my Monitor ot zny other.
- Fixed UART 115200 bauds communication via the CPU 7.3728 MHz crystal (removed 4060 frequency divider)
- MAX 232 removed for a modern FTDI232 UART-USB adapter.
- Optional second UART 68B50 to hzndle serial communication with a native C64 keyboard 
- Optional PTM 68B40 asociated with a 4060
- Optional second GAL 22V10 to generate a cartdridge /CS
- 

At the moment, no hardware has been started. The first step is to get all necessary software and take time to think about what's I want.
The second, currently in progress, is to design a schematic.

# A name

Alix-9 could be the name of this simple board comming from the french language : "Appareil Léger d'Informatique eXpérimentale"

## Status

- EKEmulator works (updated to the lastest Xcode and macOS)
- i6809asm works (updated to the lastest Xcode and macOS)
- CocoBasic can be assembled correctly. Tested and run on EKEmulator.
- EKMonitor16kB can be assembled correctly. Tested and run on EKEmulator.


## Specification

- 16K ROM
- 32K RAM
- 68B09P Processor with a 7.3728MHz crystal (1.8432MHz clock inside the CPU)
- 68B50 UART used at 115200 Baud to communicate with a computer through a FTDI232
- 68B21 PIA
- 4 free Chip select
- 4 Expansion slot
- Optional 68B50 UART used to communicate with a Commodore C64 keyboard/Arduino Micro pro.
- Optional 68B40 PTM+4060 to generate a system time counter
- Optional GAL 22V10 to add a cartdridge port chip select


## Memory map

  AREA            SIZE                            USAGE             SIGNAL            Address used and ghost

- C000-FFFF       16K                             ROM               /CSROM            -

- BFC0...BFFF     64 bytes                        FREE CS 3         /CSFREE0          -                        
- BF80...BFBF     64 bytes                        FREE CS 2         /CSFREE0          -
- BF40...BF7F     64 bytes                        FREE CS 1         /CSFREE0          -
- BF00...BF3F     64 bytes                        FREE CS 0         /CSFREE0          -

- BC30...BC3F     8 bytes used in 16 possible     PTM6840           /CSPTM            $BC30...$BC37
- BC20...BC2F     4 bytes used in 16 possible     PIA6821           /CSPIA            $BC20...$BC23
- BC10...BC1F     2 bytes used in 16 possible     UART6850_C64      /CSC64            $BC20...$BC21
- BC00...BC0F     2 bytes used in 16 possible     UART6850_USB      /CSUSB            $BC00...$BC01

- 8000...BBFF     15K                             FREE SPACE        -                 -
- 0000...7FFF     32K                             RAM 62256         /CSROM            -

## Glue logic

An IC PLD 22V10 Logic is used to decode the address bus with a range of 12 bits A15...A4 and provide all necessary /CS.

## CocoBasic ROM

CocoBasic is...

Make CocoBasic.bin :

- Open a BASH terminal 
- Enter to the folder CocoBasic
- Type: $ ./i6809asm CocoBasic.asm -bin
- This will produce a CocoBasic.bin file

CocoBasic.bin file produced is ready for your EEPROM burner with the correct size of 16384 bytes for a 16kB device.

## EKMonitor16kB ROM

EKMonitor16kB is my own monitor wrote in 6809 assembly language. Currently, comming from my first project RedBoard-1.

Make EKMonitor.bin :

- Open a BASH terminal 
- Enter to the folder EKMonitor16kB
- Type: $ ./i6809asm EK6809Monitor.asm -bin
- This will produce a EK6809Monitor.bin file

EK6809Monitor.bin file produced is ready for your EEPROM burner with the correcte size of 16384 bytes for a 16kB device.

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



## Contribute

## Usefull links

Grant's 6-chip 6809 computer: Where I found CocoBasic and AS9. Very good page.

http://searle.x10host.com/6809/Simple6809.html


## Licence

This project is free and can be reused as you want without any restrictions.

2002/2025 FAVARD Laurent 6809 projects.
