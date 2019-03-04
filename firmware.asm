;===============================================================================
;  _     _   _        ___
; | |   | | | |      / _ \ _ __   ___
; | |   | |_| |_____| | | | '_ \ / _ \
; | |___|  _  |_____| |_| | | | |  __/
; |_____|_| |_|      \___/|_| |_|\___|
;
; Model Lighthouse Controller
;-------------------------------------------------------------------------------
; Copyright (C)2015-2019 HandCoded Software Ltd.
; All rights reserved.
;
; This work is made available under the terms of the Creative Commons
; Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
; following URL to see the details.
;
; http://creativecommons.org/licenses/by-nc-sa/4.0/
;===============================================================================
;
; Notes:
;
;
; Pin out:
;
;                                    +--------+
;                          VCC   -> [|   \/   |] <- GND
;                          RX    -> [|        |] -> RED
;                          TX    <- [|        |] -> GREEN
;                          MODE  -> [|        |] -> BLUE
;                                    +--------+
;
;-------------------------------------------------------------------------------

                errorlevel -302
                errorlevel -303

#define M(X)    (1<<(X))

;===============================================================================
; Device Configuration
;-------------------------------------------------------------------------------

                ifdef __12F1840
                include "P12F1840.inc"
                endif

STD_CONFIG1     =       _FOSC_INTOSC & _BOREN_OFF & _CLKOUTEN_OFF
STD_CONFIG1     &=      _IESO_OFF & _FCMEN_OFF

STD_CONFIG2     =       _WRT_OFF & _PLLEN_ON & _STVREN_ON & _LVP_OFF

BLD_CONFIG1     =       _WDTE_OFF & _PWRTE_ON & _MCLRE_OFF
BLD_CONFIG1     &=      _CP_OFF & _CPD_OFF

BLD_CONFIG2     =       h'ffff'

                __config _CONFIG1, STD_CONFIG1 & BLD_CONFIG1
                __config _CONFIG2, STD_CONFIG2 & BLD_CONFIG2

                __idlocs h'1902'

;===============================================================================
; Hardware Configuration
;-------------------------------------------------------------------------------

OSC             equ     .8000000
PLL             equ     .4

FOSC            equ     OSC * PLL

;-------------------------------------------------------------------------------

PWM_STEPS       equ     .100
REFRESH_HZ      equ     .100

LED_R_PIN       equ     .0
LED_G_PIN       equ     .1
LED_B_PIN       equ     .2

LED_MASK        equ     M(LED_R_PIN)|M(LED_G_PIN)|M(LED_B_PIN)

MODE_PIN        equ     .3

RX_PIN          equ     .5
TX_PIN          equ     .4

;-------------------------------------------------------------------------------

BAUD_RATE       equ     .57600

UART_BRG16      equ     FOSC / (.16 * BAUD_RATE) - .1

RXD_SIZE        equ     .32
TXD_SIZE        equ     .32

;-------------------------------------------------------------------------------

TMR0_HZ         equ     .200
TMR0_PRE        equ     .256

TMR0_PERIOD     equ     FOSC / (.4 * TMR0_PRE * TMR0_HZ)

                if      TMR0_PERIOD & h'ffffff00'
                error   "Timer0 period does not fit in 8-bits"
                endif

TMR2_HZ         equ     PWM_STEPS * REFRESH_HZ
TMR2_PRE        equ     .4                      ; 1, 4, 16 or 64
TMR2_POST       equ     .4                      ; 1 to 16

TMR2_PERIOD     equ     FOSC / (.4 * TMR2_PRE * TMR2_HZ * TMR2_POST)

                if      TMR2_PERIOD & h'ffffff00'
                error   "Timer2 period does not fit in 8-bits"
                endif

;-------------------------------------------------------------------------------

OP_BLACK        equ     .0
OP_WHITE        equ     .1
OP_RED          equ     .2
OP_GREEN        equ     .3
OP_BLUE         equ     .4

OP_RGB          equ     .5

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

                udata_shr

_STKPTR         res     .1                      ; Stack pointer for idle task

TICKS           res     .1                      ; Interrupt tick count
BUTTON          res     .1                      ; Combined GPIO state

SHADOW          res     .1                      ; PWM pin shadow register
STEPS           res     .1                      ; PWM step counter

DUTY_R          res     .1                      ; Colour duty cycle values
DUTY_G          res     .1
DUTY_B          res     .1

TIMEOUT         res     .1                      ; Button press timeout tick
RESTART         res     .1                      ; Pattern restart control flag

RXD_HEAD        res     .1                      ; UART buffer indexes
RXD_TAIL        res     .1
TXD_HEAD        res     .1
TXD_TAIL        res     .1

TX_CHAR         res     .1                      ; Temporary storage for TX

;-------------------------------------------------------------------------------

.Pattern        udata

FLAGS           res     .1                      ; Eeprom/Flash flag
START           res     .2                      ; Data address

COUNT           res     .1                      ; Pattern step count
DELAY           res     .1                      ; Delay period in 1/10 secs

LAST_R          res     .1                      ; RGB value to fade from
LAST_G          res     .1
LAST_B          res     .1

NEXT_R          res     .1                      ; RGB value to fade to
NEXT_G          res     .1
NEXT_B          res     .1

DELTA_R         res     .2                      ; Delta step for each colour
DELTA_G         res     .2
DELTA_B         res     .2

VALUE_R         res     .2                      ; Current step values
VALUE_G         res     .2
VALUE_B         res     .2

;-------------------------------------------------------------------------------

.Debounce       udata

SAMPLES         res     .2                      ; GPIO samples

;-------------------------------------------------------------------------------

.UartBuffers    udata

RXD_BUFF        res     RXD_SIZE                ; The UART receive buffer
TXD_BUFF        res     TXD_SIZE                ; The UART transmit buffer

;===============================================================================
; Interrupt Handler
;-------------------------------------------------------------------------------

.Interrupt      code    h'004'

                btfss   INTCON,T0IF             ; Is Timer0 the source?
                bra     Timer0Handled           ; No.
                bcf     INTCON,T0IF             ; Clear the interrupt

                movlw   -TMR0_PERIOD            ; Adjust timer for the
                banksel TMR0                    ; .. next period
                addwf   TMR0,F
                incf    TICKS,F

                banksel SAMPLES                 ; Shift down samples
                movf    SAMPLES+.0,W            ; .. and combine
                movwf   BUTTON
                movf    SAMPLES+.1,W
                movwf   SAMPLES+.0
                andwf   BUTTON,F
                banksel PORTA
                movf    PORTA,W                 ; Add a new sample
                banksel SAMPLES
                movwf   SAMPLES+.1              ; .. and combine
                andwf   BUTTON,F
Timer0Handled:

;-------------------------------------------------------------------------------

                banksel PIR1                    ; Timer 2 interrupt?
                btfss   PIR1,TMR2IF
                bra     Timer2Handled           ; No.
                bcf     PIR1,TMR2IF             ; Yes, clear the flag

                banksel LATA                    ; Update PWM output pins
                movf    LATA,W
                xorwf   SHADOW,W
                andlw   LED_MASK
                xorwf   LATA,F

                clrw
                decf    DUTY_R,F                ; Work out next red state
                btfss   DUTY_R,.7
                iorlw   M(LED_R_PIN)
                decf    DUTY_G,F                ; Work out next green state
                btfss   DUTY_G,.7
                iorlw   M(LED_G_PIN)
                decf    DUTY_B,F                ; Work out next blue state
                btfss   DUTY_B,.7
                iorlw   M(LED_B_PIN)
                movwf   SHADOW

                decfsz  STEPS,F                 ; End of a complete cycle?
                bra     Timer2Handled           ; No.

                movlw   PWM_STEPS               ; Reset step count
                movwf   STEPS
                addwf   DUTY_R,F                ; And duty cycle values
                addwf   DUTY_G,F
                addwf   DUTY_B,F
Timer2Handled:

;-------------------------------------------------------------------------------

                banksel PIR1
                btfss   PIR1,RCIF               ; Has the UART recieved data?
                bra     UartRxHandled           ; No.

                banksel RXD_TAIL
                clrf    FSR1H
                movlw   low RXD_BUFF
                addwf   RXD_TAIL,W
                movwf   FSR1L

                banksel RCREG
                movf    RCREG,W                 ; Save the received charater
                movwf   INDF1

                banksel RXD_TAIL                ; Is the buffer full?
                incf    RXD_TAIL,W
                xorwf   RXD_HEAD,W
                btfsc   STATUS,Z
                bra     UartRxHandled           ; Yes, drop the character

                incf    RXD_TAIL,W              ; No, bump the tail offset
                andlw   RXD_SIZE-.1
                movwf   RXD_TAIL
UartRxHandled:

;-------------------------------------------------------------------------------

                banksel PIR1
                btfss   PIR1,TXIF               ; Is the UART ready to transmit?
                bra     UartTxHandled

                banksel TXD_HEAD
                movf    TXD_HEAD,W              ; Any data to left to transmit?
                xorwf   TXD_TAIL,W
                btfsc   STATUS,Z
                bra     UartTxDisable           ; No, disable TX interrupt

                clrf    FSR1H                   ; Point FSR1 at the head of
                movlw   low TXD_BUFF            ; .. the transmit buffer
                addwf   TXD_HEAD,W
                movwf   FSR1L

                movf    INDF1,W                 ; Fetch the next character
                banksel TXREG
                movwf   TXREG                   ; And start transmission

                banksel TXD_HEAD                ; Bump and wrap head offset
                incf    TXD_HEAD,W
                andlw   TXD_SIZE-.1
                movwf   TXD_HEAD
                bra     UartTxHandled
UartTxDisable:
                banksel PIE1                    ; Disable the TX module
                bcf     PIE1,TXIE
UartTxHandled:

;-------------------------------------------------------------------------------

                retfie                          ; Done

;===============================================================================
; Uart API
;-------------------------------------------------------------------------------

; Place the character in W at the tail of the transmit buffer and ensure
; transmit buffer empty interrupts are enabled so it will be sent.

UartTx:
                movwf   TX_CHAR                 ; Save character

                clrf    FSR1H                   ; Point FSR1 at the tail of
                movlw   low TXD_BUFF            ; .. the transmit buffer
                addwf   TXD_TAIL,W
                movwf   FSR1L

                movf    TX_CHAR,W               ; Copy character to tail
                movwf   INDF1
UartTxLoop:
                banksel TXD_TAIL                ; Bump and wrap tail offset
                incf    TXD_TAIL,W
                andlw   TXD_SIZE-.1
                movwf   TX_CHAR                 ; Save copy
                xorwf   TXD_HEAD,W              ; Buffer completely full?
                btfsc   STATUS,Z
                bra     UartTxLoop              ; Yes, wait for a space

                movf    TX_CHAR,W               ; No, update the tail
                movwf   TXD_TAIL
                banksel PIE1
                bsf     PIE1,TXIE               ; Ensure TX interrupt on
                return                          ; Done

;-------------------------------------------------------------------------------

UartRx:
                return

;===============================================================================
; Power On Reset
;-------------------------------------------------------------------------------

.ResetHandler   code    h'000'

                goto    PowerOnReset

;-------------------------------------------------------------------------------

                code
PowerOnReset:
                movlw   b'00000111'
                banksel OPTION_REG
                movwf   OPTION_REG

                movlw   M(MODE_PIN)|M(RX_PIN)   ; Make mode and RX pins inputs
                banksel TRISA
                movwf   TRISA
                movlw   M(MODE_PIN)
                banksel WPUA
                movwf   WPUA

                banksel ANSELA                  ; Turn analog off
                clrf    ANSELA

                banksel PORTA                   ; Clear ports
                clrf    PORTA
                banksel LATA                    ; And make all outputs low
                clrf    LATA

                movlw   b'10000100'             ; Set RX/TX pins
                banksel APFCON
                movwf   APFCON

;-------------------------------------------------------------------------------

                movlw   b'01110000'             ; Switch to 32MHz
                banksel OSCCON
                movwf   OSCCON

                clrwdt
                ifndef  __DEBUG
WaitTillStable:
                btfss   OSCSTAT,HFIOFR          ; And wait until stable
                bra     WaitTillStable
                endif

;-------------------------------------------------------------------------------

                movlw   M(BRG16)                ; Configure baud generator
                banksel BAUDCON
                movwf   BAUDCON
                movlw   low UART_BRG16
                movwf   SPBRGL
                movlw   high UART_BRG16
                movwf   SPBRGH

                movlw   M(TXEN)
                movwf   TXSTA
                movlw   M(SPEN)|M(CREN)         ; Configure UART for operation
                movwf   RCSTA
                movf    RCREG,W                 ; And clear the input buffer

                banksel PIE1
                bsf     PIE1,RCIE               ; Allow RX interrupts

                clrf    TXD_HEAD                ; Reset UART TXD and RXD
                clrf    TXD_TAIL                ; .. buffer offsets
                clrf    RXD_HEAD
                clrf    RXD_TAIL

;-------------------------------------------------------------------------------

                banksel TMR0
                clrf    TMR0

                bsf     INTCON,T0IE
                bsf     INTCON,T0IF

;-------------------------------------------------------------------------------

                movlw   TMR2_PERIOD-.1          ; Set Timer2 period
                banksel PR2
                movwf   PR2
                clrf    TMR2

                if      TMR2_PRE == .1
                movlw   ((TMR2_POST-.1)<<3)|M(TMR2ON)|h'00'
                else
                if      TMR2_PRE == .4
                movlw   ((TMR2_POST-.1)<<3)|M(TMR2ON)|h'01'
                else
                if      TMR2_PRE == .16
                movlw   ((TMR2_POST-.1)<<3)|M(TMR2ON)|h'02'
                else
                if      TMR2_PRE == .64
                movlw   ((TMR2_POST-.1)<<3)|M(TMR2ON)|h'03'
                else
                error   "Timer2 prescaler value is invalid"
                endif
                endif
                endif
                endif
                movwf   T2CON                   ; Configure and start

                banksel PIE1                    ; Enable timer interrupts
                bsf     PIE1,TMR2IE

;-------------------------------------------------------------------------------

                movlw   -.1                     ; Initialise button samples
                movwf   BUTTON
                banksel SAMPLES
                movwf   SAMPLES+.0
                movwf   SAMPLES+.1

                bsf     INTCON,PEIE             ; Enable peripheral interrupts

                call    TaskInit                ; Start pattern display task
                bra     CommandTask             ; Followed by command processor

;===============================================================================
; Multi-Tasking Routines
;-------------------------------------------------------------------------------

; Split the stack ready for use by two cooperative threads and then start the
; display task.

TaskInit:
                banksel STKPTR
                movf    STKPTR,W                ; Use current stack for
                movwf   _STKPTR                 ; .. alternate thread
                addlw   .7                      ; Then use upper area for
                movwf   STKPTR                  ; .. the current thread
                bra     DisplayTask             ; And start its task.

; Swap the hardware stack pointer with the alternate pointer to switch between
; threads.

TaskSwap:
                clrwdt
                banksel STKPTR
                movf    _STKPTR,W               ; Exchange stack pointers
                xorwf   STKPTR,W                ; .. using XOR
                xorwf   STKPTR,F
                xorwf   STKPTR,W
                movwf   _STKPTR
                return

; Delay the execution of a thread until W clock ticks have elapsed. If the
; restart flag is set then abandon the delay and return immediately.

TaskDelay:
                banksel STKPTR
                incf    STKPTR,F                ; Use stack to save end tick
                addwf   TICKS,W
                movwf   TOSL
TaskLoop:
                call    TaskSwap                ; Run other task
                btfsc   RESTART,.7              ; Signaled to restart?
                bra     DelayDone               ; Yes, end early
                movf    TOSL,W                  ; No, wait till end tick reached
                xorwf   TICKS,W
                btfss   STATUS,Z
                bra     TaskLoop
DelayDone:
                decf    STKPTR,F                ; Restore stack
                return

;===============================================================================
; Display Task
;-------------------------------------------------------------------------------

; The display task recovers the details of the default pattern from EEPROM at
; power on and the carries out the repeated display of the selected lighthouse
; characteristic.

DisplayTask:
                movlw   PWM_STEPS               ; Initialise display variables
                movwf   STEPS
                clrf    DUTY_R
                clrf    DUTY_G
                clrf    DUTY_B
                clrf    SHADOW

                banksel LAST_R                  ; Clear the last colour
                clrf    LAST_R
                clrf    LAST_G
                clrf    LAST_B

                bsf     INTCON,GIE              ; Allow interrupts from here

                call    SetEeprom               ; Read defaults from EEPROM
                clrf    EEADRL

                movlw   low FLAGS               ; Point FSR at key variables
                movwf   FSR0L
                movlw   high FLAGS
                movwf   FSR0H

                call    FlashRead               ; Save source
                movwi   FSR0++
                call    FlashRead               ; Save pattern address
                andlw   h'3f'
                movwi   FSR0++
                call    FlashRead
                movwi   FSR0++

;-------------------------------------------------------------------------------

FetchPattern:
                call    ReadPattern             ; Fetch the current pattern
                clrf    RESTART                 ; Clear reset flag
DisplayLoop:
                call    ReadColour              ; Set next RGB values

                movf    LAST_R,W                ; Work out delta step values
                subwf   NEXT_R,W
                movwf   DELTA_R+.1
                clrf    DELTA_R+.0

                movf    LAST_G,W
                subwf   NEXT_G,W
                movwf   DELTA_G+.1
                clrf    DELTA_G+.0

                movf    LAST_B,W
                subwf   NEXT_B,W
                movwf   DELTA_B+.1
                clrf    DELTA_B+.0

                asrf    DELTA_R+.1,F            ; Divide by 8
                rrf     DELTA_R+.0,F
                asrf    DELTA_R+.1,F
                rrf     DELTA_R+.0,F
                asrf    DELTA_R+.1,F
                rrf     DELTA_R+.0,F

                asrf    DELTA_G+.1,F
                rrf     DELTA_G+.0,F
                asrf    DELTA_G+.1,F
                rrf     DELTA_G+.0,F
                asrf    DELTA_G+.1,F
                rrf     DELTA_G+.0,F

                asrf    DELTA_B+.1,F
                rrf     DELTA_B+.0,F
                asrf    DELTA_B+.1,F
                rrf     DELTA_B+.0,F
                asrf    DELTA_B+.1,F
                rrf     DELTA_B+.0,F

                movf    LAST_R,W                ; Set initial value
                movwf   VALUE_R+.1
                clrf    VALUE_R+.0
                movf    LAST_G,W
                movwf   VALUE_G+.1
                clrf    VALUE_G+.0
                movf    LAST_B,W
                movwf   VALUE_B+.1
                clrf    VALUE_B+.0

                call    FadeStep                ; Step fade seven times
                call    FadeStep
                call    FadeStep
                call    FadeStep
                call    FadeStep
                call    FadeStep
                call    FadeStep

                banksel NEXT_R                  ; Then set the final value
                bcf     INTCON,GIE
                movlw   PWM_STEPS
                movwf   STEPS
                movf    NEXT_R,W
                movwf   LAST_R
                movwf   DUTY_R
                movf    NEXT_G,W
                movwf   LAST_G
                movwf   DUTY_G
                movf    NEXT_B,W
                movwf   LAST_B
                movwf   DUTY_B
                bsf     INTCON,GIE

                call    FlashRead               ; Fetch time period
                banksel DELAY
                movwf   DELAY
DelayLoop:
                movlw   TMR0_HZ/.10             ; Delay for 0.1S
                call    TaskDelay
                btfsc   RESTART,.7              ; Has the pattern changed?
                goto    FetchPattern            ; Yes, restart fetch
                banksel DELAY
                decfsz  DELAY,F                 ; End of delay period?
                bra     DelayLoop               ; No, repeat

                banksel COUNT
                decfsz  COUNT,F                 ; End of pattern?
                bra     DisplayLoop             ; No, fetch next colour
                bra     FetchPattern            ; Yes, restart pattern

;-------------------------------------------------------------------------------

; Modify the PWM values using the delta step value to make them closer to the
; final target state.

FadeStep:
                banksel DELTA_R
                movf    DELTA_R+.0,W            ; Work out next step value
                addwf   VALUE_R+.0,F
                movf    DELTA_R+.1,W
                addwfc  VALUE_R+.1,F

                movf    DELTA_G+.0,W
                addwf   VALUE_G+.0,F
                movf    DELTA_G+.1,W
                addwfc  VALUE_G+.1,F

                movf    DELTA_B+.0,W
                addwf   VALUE_B+.0,F
                movf    DELTA_B+.1,W
                addwfc  VALUE_B+.1,F

                bcf     INTCON,GIE              ; Disable interrupts
                movlw   PWM_STEPS               ; Copy to PWM generation
                movwf   STEPS                   ; .. counters
                movf    VALUE_R+.1,W
                movwf   DUTY_R
                movf    VALUE_G+.1,W
                movwf   DUTY_G
                movf    VALUE_B+.1,W
                movwf   DUTY_B
                bsf     INTCON,GIE              ; Enable interrupts

                movlw   .5                      ; Delay to allow display
                goto    TaskDelay

;-------------------------------------------------------------------------------

; Reset the flash access registers to read from the current pattern source
; starting at the address of the last selected pattern and read its length.

ReadPattern:
                call    SetEeprom               ; Pick pattern source
                banksel FLAGS
                btfss   FLAGS,.7
                call    SetFlash

                banksel START                   ; Set memory pointer
                movf    START+.0,W
                banksel EEADRH
                movwf   EEADRH
                banksel START
                movf    START+.1,W
                banksel EEADRL
                movwf   EEADRL

                call    FlashRead               ; And read count
                banksel COUNT
                movwf   COUNT
                return

;-------------------------------------------------------------------------------

; Read the next colour opcode and execute the corresponding function to set
; the target RGB values.

ReadColour:
                call    FlashRead               ; Fetch next colour opcode
                banksel NEXT_R
                brw                             ; And process it

                goto    SetBlack
                goto    SetWhite
                goto    SetRed
                goto    SetGreen
                goto    SetBlue
                goto    SetRGB

; These functions map a single byte opcode to common RGB colour settings.

SetBlack:
                clrf    NEXT_R                  ; R = 0%
                clrf    NEXT_G                  ; G = 0%
                clrf    NEXT_B                  ; B = 0%
                return

SetWhite:
                movlw   .100
                movwf   NEXT_R                  ; R = 100%
                movwf   NEXT_G                  ; G = 100%
                movwf   NEXT_B                  ; B = 100%
                return

SetRed:
                movlw   .100
                movwf   NEXT_R                  ; R = 100%
                clrf    NEXT_G                  ; G = 0%
                clrf    NEXT_B                  ; B = 0%
                return

SetGreen:
                movlw   .100
                clrf    NEXT_R                  ; R = 0%
                movwf   NEXT_G                  ; G = 100%
                clrf    NEXT_B                  ; B = 0%
                return

SetBlue:
                movlw   .100
                clrf    NEXT_R                  ; R = 0%
                clrf    NEXT_G                  ; G = 0%
                movwf   NEXT_B                  ; B = 100%
                return

; This opcode allows custom colour settings to be included after the opcode.

SetRGB:
                call    FlashRead               ; Read and store values for
                banksel NEXT_R
                movwf   NEXT_R                  ; .. Red %
                call    FlashRead
                banksel NEXT_G
                movwf   NEXT_G                  ; .. Green %
                call    FlashRead
                banksel NEXT_B
                movwf   NEXT_B                  ; .. Blue %
                return

;===============================================================================
; Command Task
;-------------------------------------------------------------------------------

CommandTask:
                call    TaskSwap
                btfss   BUTTON,MODE_PIN         ; Is the button pressed?
                goto    TimeButton              ; Yes

                ; UART HERE

                bra     CommandTask             ; Repeat

;-------------------------------------------------------------------------------

TimeButton:
                movf    TICKS,W                 ; Work out long press tick
                addlw   TMR0_HZ                 ; .. count
                movwf   TIMEOUT

ShortPressLoop:
                call    TaskSwap                ; Wait until the button is
                btfsc   BUTTON,MODE_PIN         ; .. released
                bra     ShortPress
                movf    TICKS,W                 ; Or the long press timeout
                xorwf   TIMEOUT,W               ; .. is reached
                btfss   STATUS,Z
                bra     ShortPressLoop          ; Not there yet

LongPressLoop:
                call    TaskSwap                ; Wait until the button is
                btfss   BUTTON,MODE_PIN         ; .. released
                bra     LongPressLoop

LongPress:
        movlw   'L'
        call    UartTx
                goto    CommandTask

ShortPress:
        movlw   'S'
        call    UartTx
                call    ReadPattern             ; Reread pattern to get length
SkipPattern:
                call    ReadColour              ; Skip over a colour entry
                call    FlashRead               ; .. and delay
                banksel COUNT
                decfsz  COUNT,F                 ; Until the end is reached
                bra     SkipPattern

                banksel EEADRH                  ; And save start address for
                movf    EEADRH,W                ; .. the next pattern
                banksel START
                movwf   START+.0
                banksel EEADRL
                movf    EEADRL,W
                banksel START
                movwf   START+.1

                decf    RESTART,F               ; Signal display task to restart

                call    ReadPattern             ; Attempt to read the pattern
                xorlw   -.1                     ; Read end of table?
                btfss   STATUS,Z
                goto    CommandTask             ; No

                movlw   low Patterns            ; Assume ROM source
                movwf   START+.0
                movlw   high Patterns
                movwf   START+.1
                btfss   FLAGS,.7                ; ROM Source?
                bra     WrapPointer
                movlw   low UserPatterns        ;
                movwf   START+.0
                movlw   high UserPatterns
                movwf   START+.1
WrapPointer:
                call    ReadPattern             ; Ensure valid pattern set
                goto    CommandTask             ; And continue

;===============================================================================
; EEPROM Access Routines
;-------------------------------------------------------------------------------

SetFlash:
                banksel EECON1
                bsf     EECON1,EEPGD
                return

SetEeprom:
                banksel EECON1
                bcf     EECON1,EEPGD
                return

FlashRead:
                banksel EECON1
                bcf     EECON1,CFGS             ; Read a byte from flash or
                bsf     EECON1,RD               ; .. EEPROM
                nop
                nop
                incf    EEADRL,F                ; Bump the address
                btfsc   STATUS,Z
                incf    EEADRH,F
                movf    EEDATL,W
                return                          ; Done.

;===============================================================================
; ROM Pattern Table
;-------------------------------------------------------------------------------

Patterns:

                de      .3
                de      OP_RED,         .10
                de      OP_GREEN,       .10
                de      OP_BLUE,        .10

                de      .2
                de      OP_WHITE,       .2
                de      OP_BLACK,       .2

; Alderney - Fl(4) W 15s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .27
                de      OP_WHITE,       .10
                de      OP_BLACK,       .28

; Anvil Point - Fl W 10s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .90

; Bamburgh - Occ(2) RGW 8s [W 14m, R/G 11m]

                de      .2
                de      OP_BLACK,       .10
                de      OP_WHITE,       .30

                de      .2
                de      OP_BLACK,       .10
                de      OP_RED,         .30

                de      .2
                de      OP_BLACK,       .10
                de      OP_GREEN,       .30

; Bardsey - Fl(5) W 15s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .20

; Beachy Head - Fl(2) W 20s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .90

; Berry Head - Gr Fl(2) W 15s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .120

; Bishop Rock - Gr Fl(2) W 15s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .120

; Bull Point - Fl(3) W 10s

                de      .6
                de      OP_WHITE,       .10
                de      OP_BLACK,       .23
                de      OP_WHITE,       .10
                de      OP_BLACK,       .23
                de      OP_WHITE,       .10
                de      OP_BLACK,       .24

; Caldey Island = Gr Fl(3) W 20s [W 13, R 9m]

                de      .6
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .100

                de      .6
                de      OP_RED,         .10
                de      OP_BLACK,       .10
                de      OP_RED,         .10
                de      OP_BLACK,       .10
                de      OP_RED,         .10
                de      OP_BLACK,       .100

; Casquets - Gr Fl(5) W 30s

                de      .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .210

; Coquet - Fl(3) RW 20s [W 19m, R 15m]

                de      .6
                de      OP_WHITE,       .10
                de      OP_BLACK,       .56
                de      OP_WHITE,       .10
                de      OP_BLACK,       .57
                de      OP_WHITE,       .10
                de      OP_BLACK,       .57

                de      .6
                de      OP_RED,         .10
                de      OP_BLACK,       .56
                de      OP_RED,         .10
                de      OP_BLACK,       .57
                de      OP_RED,         .10
                de      OP_BLACK,       .57

; Cromer - Fl W 5s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .40

; Crow Point - Fl RW 2.5s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .15

                de      .2
                de      OP_RED,         .10
                de      OP_BLACK,       .15

; Dungeness - Fl W 10s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .90

; Eddystone - Gr Fl(2) W 10s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .70

; Europa Point - Fl(2) RW 15s [W 19m, R 15m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .65

                de      .2
                de      OP_RED,         .10
                de      OP_BLACK,       .65

; Farne - Fl(2) RW 15s [W 19m, R 15m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .65

                de      .2
                de      OP_RED,         .10
                de      OP_BLACK,       .65

; Flamborough Head - Fl(4) W 15s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .32
                de      OP_WHITE,       .10
                de      OP_BLACK,       .33

 ; Flatholm - Gr Fl(3) RW 10s [W 15m, R 12m]

                de      .6
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .50

; Godrevy - Fl RW 10s [8m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .90

                de      .2
                de      OP_RED,         .10
                de      OP_BLACK,       .90

; Guile Point East - Occ RWG 10s

                de      .2
                de      OP_BLACK,       .10
                de      OP_WHITE,       .90

                de      .2
                de      OP_BLACK,       .10
                de      OP_RED,         .90

                de      .2
                de      OP_BLACK,       .10
                de      OP_GREEN,       .90

; Hartland Point Fl(6) 15s [8m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .15

; Heugh Hill OCC WRG 6s [5m]

                de      .2
                de      OP_BLACK,       .10
                de      OP_WHITE,       .50

                de      .2
                de      OP_BLACK,       .10
                de      OP_RED,         .50

                de      .2
                de      OP_BLACK,       .10
                de      OP_GREEN,       .50

; Hilbre Island Fl R 3s [5m]

                de      .2
                de      OP_RED,         .10
                de      OP_BLACK,       .20

; Hurst Point Fl(4) RW 15s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .27
                de      OP_WHITE,       .10
                de      OP_BLACK,       .28

                de      .4
                de      OP_RED,         .10
                de      OP_BLACK,       .27
                de      OP_RED,         .10
                de      OP_BLACK,       .28

; Les Hanois Gr Fl(2) W 15s

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .10
                de      OP_WHITE,       .10
                de      OP_BLACK,       .120

; Lizard Fl W 3s

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .20

; Longships Iso WR 10s

                de      .2
                de      OP_WHITE,       .50
                de      OP_BLACK,       .50

                de      .2
                de      OP_RED,         .50
                de      OP_BLACK,       .50

; Longstone Fl W 20s [24m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .190

; Lowestoft Fl W 15s [23m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .140

; Lundy North Fl W 15s [17m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .140

; Lundy South Fl W 5s [15m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .40

; Lynmouth Foreland Fl(4) W 15s [18m]

                de      .4
                de      OP_WHITE,       .10
                de      OP_BLACK,       .27
                de      OP_WHITE,       .10
                de      OP_BLACK,       .28

; Monkstone Fl W 5s [12m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .40

; Mumbles Fl(4) W 20s [15m]

                de      .2
                de      OP_WHITE,       .10
                de      OP_BLACK,       .40

; End of list marker

                de      -.1

;===============================================================================
; EEPROM Pattern Table
;-------------------------------------------------------------------------------

.Eeprom         code    h'f000'

EEPROM          de      h'00'                   ; Default source is ROM
                de      high Patterns
                de      low Patterns

UserPatterns:

; RGB Test

                de      .5
                de      OP_WHITE,       .20
                de      OP_RED,         .20
                de      OP_GREEN,       .20
                de      OP_BLUE,        .20
                de      OP_BLACK,       .20

                de      -.1

                end