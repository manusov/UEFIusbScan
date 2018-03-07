;-----------------------------------------------------------------------------;
;             USB3.0/3.1 xHCI controllers information utility                 ;
;                    (C) IC Book Labs. v0.1 from 07.03.2018                   ;
;-----------------------------------------------------------------------------;

; Bug 1 = Routing must be visualized for Intel 8 and 9 series chipsets only,
;         now visualized for all chipsets.
; TODO  = Coloring output.

format pe64 dll efi
entry main
section '.text' code executable readable
main:

;--- Save registers and UEFI application input parameters ---
push rbx rcx rdx rsi rdi rbp
push r8 r9 r10 r11 r12 r13 r14 r15
mov r15,VariablesPool
mov [r15+00],rcx                  ; Store UEFI application handle
mov [r15+08],rdx                  ; Store UEFI system table address
cld

;--- Scan PCI configuration space for USB3 xHCI controllers ---
mov ebx,80000000h     ; EBX = PCI: Bus=0, Device=0, Function=0, Register=0, E=1
xor edx,edx           ; EDX = Devices count
mov rdi,DataBuffer1   ; RDI = Base address of data buffer: scan results 
ScanPci:
xor ecx,ecx           ; ECX = PCI: Function=0 , E=1 separate field
lea eax,[ebx+ecx+12]  ; Select field with [0Eh]=Header Type
call ReadPci32
mov ebp,1             ; EBP = 1 for single-function
bt eax,23
jnc @f                ; Go skip if single function [0Eh]=Header Type, bit7 = 0
mov ebp,8             ; EBP = 8 for multi-function
@@:
ScanPciFunctions:
lea eax,[ebx+ecx]     ; Select VID:DID, register 00h
mov [rdi],eax         ; Buffer [00-03] = Bus,Dev,Fnc
call ReadPci32
cmp eax,0FFFFFFFFh
je ScanSkip           ; Skip if no device at this Bus:Dev:Fnc, RDI not changed
mov [rdi+4],rax       ; Buffer [04-07] = VID:DID
lea eax,[ebx+ecx+2Ch] ; Select SVID:SDID, register 2Ch
call ReadPci32
mov [rdi+08],rax      ; Buffer [04-07] = SVID:DID
lea eax,[ebx+ecx+08]  ; Select Rev, PI, Sub, BaseClass, register 08h
call ReadPci32
and eax,0FFFFFF00h    ; Mask for comparision
cmp eax,00C033000h    ; Value for comparision
jne ScanSkip          ; Skip if BaseCl, SubCl, PI mismatch, RDI not changed
add rdi,12            ; At this point only can change RDI - device detected
stosd                 ; Buffer [12-15] = Rev, PI, Sub, BaseClass
;--- Detecting BAR ---
; Buffer [16-23] = BAR0 + BAR1 as 64-bit extension
lea eax,[ebx+ecx+10h] ; Select BAR0, register 10h
call ReadPci32        ; Register=10h, xHCI MMIO BAR, low 32 bits
mov esi,eax
and al,0F0h
mov [rdi],eax         ; Store xHCI BAR, bits [31-00]
test si,1001b         ; Expected non-prefetchable memory BAR
jnz BadBar            ; Go error if prefetchable BAR or IO BAR
xor eax,eax
and si,0110b
jz HighBar            ; If 32-bit BAR, set bits [63-32]=0
cmp si,0100b
jne BadBar            ; Go error if unknown BAR width
lea eax,[ebx+ecx+14h] ; Register=14h, xHCI MMIO BAR, high 32 bits
call ReadPci32
HighBar:
mov [rdi+4],eax       ; Store xHCI BAR, bits [63-32]
jmp @f
BadBar:
mov qword [rdi],0
@@:
add rdi,8             ; At this point only can modify RDI  
;--- Check number of controllers limit ---
inc edx               ; Controllers count
cmp edx,9             ; Check number of controllers limit
jae EndScanPci        ; Exit cycle if limit detected   
;--- Cycle for controllers ---
ScanSkip:
add ecx,0100h         ; PCI function + 1
dec ebp               ; Count PCI function number
jnz ScanPciFunctions  ; This cycle for 8 PCI functions, if multi-functional
add ebx,800h          ; Field [Bus,Device] + 1
test ebx,00FFF800h    ; Select [Bus,Device] field for check                      ;*
jnz ScanPci           ; Go repeat cycle if [Bus,Device] not done
EndScanPci:

;--- Check number of controllers detected ---
test edx,edx
jz ApplicationError1  ; Go error if xHCI not detected

;--- Built and show screen 1 = USB3 xHCI controllers list ---
mov rsi,Screen1      ; RSI = Pointer to text strings source array 
mov rdi,AsciiBuffer  ; RDI = Pointer to destination buffer for built ASCII text
mov r8,DataBuffer1   ; R8  = Pointer to PCI scan results buffer
push rdi             ; Save destination buffer start address
@@:
lodsb
cmp al,0
je @f
stosb
jmp @b
@@:
mov ax,0A0Dh
stosw
mov ecx,72
mov al,'-'
rep stosb
@@:
mov dh,1             ; DH = Controllers counter, DL = Detected controllers #
@@:                  ; Start cycle for generating controllers list
mov ax,0A0Dh
stosw
mov al,' '
stosb
mov bl,0             ; BL = Number template mode, 0 = no template
movzx eax,dh         ; EAX = Port number
call DecimalPrint32  ; Built ASCII string - port number
mov ax,'  '
stosw
stosb
mov al,[r8+02]       ; AL = Get PCI bus number  
call HexPrint8       ; Built ASCII string - PCI bus number
mov ax,'  '
stosw
mov al,[r8+01]       ; AL = Get PCI device:function number
shr al,3             ; Select bits [7-3] = device  
call HexPrint8       ; Built ASCII string - PCI device number
mov ax,'  '
stosw
mov al,[r8+01]       ; AL = Get PCI device:function number
and al,7             ; Select bits [2-0] = function  
call HexPrint8       ; Built ASCII string - PCI function number
mov ax,'  '
stosw
stosb
mov ax,[r8+04]       ; AX = Get PCI vendor ID
call HexPrint16      ; Built ASCII string - PCI vendor ID
mov al,' '
stosb
mov ax,[r8+06]       ; AX = Get PCI device ID
call HexPrint16      ; Built ASCII string - PCI device ID
mov al,' '
stosb
mov ax,[r8+08]       ; AX = Get PCI subsystem vendor ID
call HexPrint16      ; Built ASCII string - PCI vendor ID
mov al,' '
stosb
mov ax,[r8+10]       ; AX = Get PCI subsystem device ID
call HexPrint16      ; Built ASCII string - PCI device ID
mov al,' '
stosb
mov al,[r8+12]       ; AL = Get PCI revision
call HexPrint8       ; Built ASCII string - PCI revision
mov ax,'  '
stosw
mov al,[r8+15]       ; AL = Get PCI base class
call HexPrint8       ; Built ASCII string - PCI base class
mov ax,'  '
stosw
mov al,[r8+14]       ; AL = Get PCI sub class
call HexPrint8       ; Built ASCII string - PCI sub class
mov ax,'  '
stosw
mov al,[r8+13]       ; AL = Get PCI program interface
call HexPrint8       ; Built ASCII string - PCI program interface
mov ax,'  '
stosw
stosb
mov rax,[r8+16]      ; RAX = Get xHCI MMIO address         
call HexPrint64      ; Built ASCII string - xHCI MMIO address 
add r8,24            ; Select next device entry in the buffer
inc dh               ; Port number + 1
cmp dh,dl
jbe @b               ; Cycle for generating controllers list

mov ax,0A0Dh
stosw
mov al,0
stosb                ; Write terminator byte
pop rsi              ; Restore buffer address for visual it
call StringWrite

;--- Wait for key press, select controller (1-9) ---  
mov rdi,SelectEntry
mov bl,0
movzx eax,dl
call DecimalPrint32  ; Print last detected controller number
mov rsi,SelectMsg
call StringWrite     ; Write select controller string
mov bl,dl            ; BL = Last controller number, 1-based
or bl,30h            ; Convert to ASCII
@@:
call WaitKey
mov rsi,CrLfMsg
call StringWrite
cmp dx,0017h         ; DX = EFI Scan Code
je ExitProgram       ; Go exit if ESC detected
shr edx,16           ; DX = UNICODE char
cmp dx,0031h
jb @b                ; Go wait if below "1"
cmp dx,0039h
ja @b                ; Go wait if above "9"
cmp dl,bl
ja @b                ; Go wait if above presented controller
mov rsi,CrLfMsg
call StringWrite

;--- Load and check selected controller parameters ---
mov rax,DataBuffer1-24  ; RAX = Parameters base address, -24 because 1=First
and edx,0Fh             ; EDX = Controller select number
imul edx,edx,24
mov r13d,[rax+rdx]      ; R13D = PCI Bus, Device, Function
mov r14,[rax+rdx+16]    ; R14  = MMIO base address
test r14,r14
jz ApplicationError2    ; Go if MMIO base address = 0 , detection error

;--- Scan xHCI capabilities for select USB2 and USB3 ports ---
; Result bitmaps:
; R9 = USB2.0 ports bitmap
; R10 = USB3.0 ports bitmap (5.0 Gbps)
; R11 = USB3.1 ports bitmap (10.0 Gbps)
;---
mov eax,[r14+10h]       ; EAX = HCCPARAMS1
shr eax,16-2
and eax,0003FFFCh       ; RAX = xECP, units=dwords
lea rbx,[r14+rax]       ; RBX = Address of capabilities list
xor r9d,r9d             ; R9  = Pre-blank bitmap USB2
xor r10d,r10d           ; R10 = Pre-blank bitmap USB3.0 (5.0 Gbps)
xor r11d,r11d           ; R10 = Pre-blank bitmap USB3.1 (10.0 Gbps)
lea rdx,[r9-1]          ; RDX = FFFFFFFFFFFFFFFFh

CapScan:
mov eax,[rbx]
cmp al,2                ; xHCI Supported Protocol Capability ID = 2
jne CapNext             ; Go next if capability ID mismatch

mov esi,10h
mov ebp,[rbx+08h]
shr ebp,28              ; EBP = PSIC field
jnz SpeedNext

mov ecx,[rbx+04h]       ; ECX = get signature field        
cmp ecx,020425355h      ; Compare with signature 'USB '
jne CapNext             ; Skip if mismatch
shld ecx,eax,16         ; CX = USB rev., for entire capability in this branch
cmp cx,0200h
je BitmapUsb2
cmp cx,0300h
je BitmapUsb3
cmp cx,0301h
je BitmapUsb31
cmp cx,0310h
je BitmapUsb31
jmp CapNext

SpeedNext:
mov edi,[rbx+rsi]
and edi,0FFFF0030h
cmp edi,001E00020h
je BitmapUsb2           ; Go if speed match USB 2.0
cmp edi,000050030h
je BitmapUsb3           ; Go if speed match USB 3.0
add rsi,4
dec ebp                 ; Protocol Speed ID count - 1
jnz SpeedNext

CapNext:
test ah,ah
jz BitmapsDone
movzx eax,ah
shl eax,2
add rbx,rax
jmp CapScan

BitmapUsb2:             ; Parse USB2 Supported Protocol Capability
mov cl,[rbx+09h]        ; CL = Compatible port count
shld r9,rdx,cl
mov cl,[rbx+08h]        ; CL = Compatible port offset, 1-based
dec ecx
shl r9,cl
jmp CapNext

BitmapUsb3:             ; Parse USB3.0 Supported Protocol Capability
mov cl,[rbx+09h]        ; CL = Compatible port count
shld r10,rdx,cl
mov cl,[rbx+08h]        ; CL = Compatible port offset, 1-based
dec ecx
shl r10,cl
jmp CapNext

BitmapUsb31:            ; Parse USB3.1 Supported Protocol Capability
mov cl,[rbx+09h]        ; CL = Compatible port count
shld r11,rdx,cl
mov cl,[rbx+08h]        ; CL = Compatible port offset, 1-based
dec ecx
shl r11,cl
jmp CapNext

BitmapsDone:

;--- Prepare data for screen 2 = USB3 xHCI capabilities and ports list --- 
mov rdi,DataBuffer1     
mov eax,[r14+04]        ; EAX = HCSPARAMS1
stosd                   ; Store HCSPARAMS1 value, buffer [00-03]
shr eax,24
jz ApplicationError3    ; Go if number of ports = 0

mov esi,1               ; ESI = Mask
xor r8d,r8d             ; R8D used later for built text
xchg ecx,eax            ; RCX = Number of ports
xor ebx,ebx             ; EBX = Ports count
xor eax,eax
mov al,[r14]            ; RAX = CAPLENGTH
lea r14,[r14+rax+0400h] ; Set R14 to PORTSC MMIO

StorePorts:
mov ah,2                ; AH = USB2 mode value
test r9,rsi
jnz KnownPort           ; Go if this port USB2
mov ah,3                ; AH = USB3 mode value
test r10,rsi
jnz KnownPort           ; Go if this port USB3
mov ah,31
test r11,rsi
jz SkipPort             ; Go if this port unknown

KnownPort:
mov al,bl
stosw                   ; Store port number+mode, buffer [00-01][+4]
mov rax,r14
stosq                   ; Store MMIO address, buffer [02-09][+4]
mov eax,[rax]           ; Read MMIO, xHCI Port Status and Control Reg [i].
stosd                   ; Store port data, buffer [10-11][+4]
inc r8d
SkipPort:
shl rsi,1
add r14,16
inc ebx
loop StorePorts

test r8d,r8d
jz ApplicationError3    ; Go if number of ports = 0

;--- Built and show screen 2 = USB3 xHCI capabilities and ports list --- 
mov rsi,Screen2      ; RSI = Pointer to text strings source array 
mov rdi,AsciiBuffer  ; RDI = Pointer to destination buffer for built ASCII text
mov rbx,DataBuffer1  ; R9 = Pointer to registers data source array
push rdi             ; Save destination buffer start address
@@:
lodsb
cmp al,0
je @f
stosb
jmp @b
@@:
mov ax,' ='
stosw
stosb
mov eax,[rbx]
add rbx,4
call HexPrint32
mov ax,0A0Dh
stosw
@@:
lodsb
cmp al,0
je @f
stosb
jmp @b
@@:
mov ax,0A0Dh
stosw
mov ecx,72
mov al,'-'
rep stosb
mov ax,0A0Dh
stosw
;--- Start ports registers strings sequence ---
PortScCycle:
push rsi
;--- Select USB2 or USB3, Port Number = R9D ---
push rbx rdi
mov rdi,NumberUSB
mov eax,'2.0 '
cmp byte [rbx+1],2
je @f                ; Go if current port = USB2
mov eax,'3.0 '
cmp byte [rbx+1],3
je @f                ; Go if current port = USB3
mov eax,'3.1 '
cmp byte [rbx+1],31
je @f                ; Go if current port = USB3
mov eax,'?   '
@@:
stosw                ; EAX = " 2.0" , " 3.0" , "3.1 "
shr eax,16
stosb  
mov rdi,NumberPort
movzx eax,byte [rbx]
mov bl,2             ; BL = 2, template size = 2 chars
call DecimalPrint32  ; EAX = USB port number
pop rdi rbx
inc rbx
inc rbx
;--- Built Port String ---
mov rax,[rbx]        ; RAX = MMIO address for this port
add rbx,8
call HexPrint64
mov ecx,4
mov al,' '
rep stosb
@@:
lodsb
cmp al,0
je @f
stosb
jmp @b
@@:
mov ax,'  '
stosw
mov eax,[rbx]        ; RAX = Data from port, previously saved
add rbx,4
call HexPrint32
mov rsi,PortNotConnected
test al,1
jz @f
mov rsi,PortUnknown
shr eax,10
and al,0Fh           ; AL = Speed mode bitfield
jz @f                ; AL = 0 , means unknown
cmp al,5
ja @f                ; AL > 5 , means unknown
imul eax,eax,PDsize
mov rsi,PortDecode - PDsize
add rsi,rax
@@:
mov cl,4
mov al,' '
rep stosb
@@:
lodsb                ; RSI = Pointer to speed mode string
cmp al,0
je @f
stosb
jmp @b
@@:
mov ax,0A0Dh
stosw
;--- Cycle for ports ---
pop rsi
dec r8d
jnz PortScCycle
;--- End ports registers strings sequence ---
mov al,0
stosb                ; Write terminator byte
pop rsi              ; Restore buffer address for visual it
call StringWrite

;--- Wait for any key press, returned code ignored yet ---
mov rsi,AnyKeyMsg
call StringWrite
call WaitKey
mov rsi,CrLfMsg
call StringWrite

;--- Prepare data for screen 3 = Intel 8 series chipset specific registers ---
; Read Configuration Registers
; All this registers is 32-bit
;---
; D0h = USB 2.0 Port Routing' , 0
; D4h = USB 2.0 Port Routing Mask' , 0
; D8h = USB 3.0 Port SuperSpeed Enable',0
; DCh = USB 3.0 Port Routing Mask',0
; E4h = USB 2.0 Port Disable Override',0
; E8h = USB 3.0 Port Disable Override',0
;---
mov rsi,CfgRegsList
mov rdi,DataBuffer1
mov ecx,CfgRegsCount
@@:
xor eax,eax
lodsb                             ; Load address of register
add eax,r13d
call ReadPci32
stosd                             ; Store current register value
loop @b

;--- Built and show screen 3 = Intel 8 series chipset specific registers ---
mov rsi,Screen3      ; RSI = Pointer to text strings source array 
mov rdi,AsciiBuffer  ; RDI = Pointer to destination buffer for built ASCII text
mov rbx,DataBuffer1  ; RBX = Pointer to registers data source array
push rdi             ; Save destination buffer start address
@@:
lodsb
cmp al,0
je @f
stosb
jmp @b
@@:
mov ax,0A0Dh
stosw
;--- Start registers strings sequence ---
mov ebp,CfgRegsCount
StartDevSpec:
mov ecx,31           ; String name size limit = 31 chars
BuiltDevSpec:
lodsb
cmp al,0
je EndNameDevSpec
stosb
dec ecx
jmp BuiltDevSpec
EndNameDevSpec:
mov al,' '
rep stosb            ; Align string size before "="
mov ax,'= '
stosw
mov eax,[rbx]
add rbx,4
call HexPrint32
mov al,'h'
stosb
mov ax,0A0Dh
stosw                ; Make next string
dec ebp
jnz StartDevSpec     ; Go cycle if terminator 00h not found
;--- End registers strings sequence ---
mov al,0
stosb                ; Write terminator byte
pop rsi              ; Restore buffer address for visual it
call StringWrite

;--- Exit points ---
ExitProgram:
mov rsi,NameMsg
call StringWrite     ; Write copyright string
jmp ApplicationExit

;--- Convert string: ASCII to UNICODE --- 
ApplicationError1:   ; No xHCI
mov rsi,ErrorMsg1
jmp ErrorWrite
ApplicationError2:   ; BAR failed
mov rsi,ErrorMsg2
jmp ErrorWrite
ApplicationError3:   ; Invalid configuration
mov rsi,ErrorMsg3
ErrorWrite:
call StringWrite

;--- Exit to UEFI with restore registers ---
ApplicationExit:
pop r15 r14 r13 r12 r11 r10 r9 r8
pop rbp rdi rsi rdx rcx rbx
xor rax,rax                       ; RAX=EFI_STATUS=0
ret                               ; Simple form of termination

;--- Subroutine for Read PCI Configuration space ---
; Input:  EAX = Address at Bus:Device:Function form
;               Bits:
;               EAX.31 = Must be "1", access enable
;               EAX.[32-24] = Reserved, 0
;               EAX.[23-16] = Bus number, 8-bit
;               EAX.[15-11] = Device number, 5-bit
;               EAX.[10-08] = Function number, 3-bit
;               EAX.[07-02] = Register number (4 byte window), 6-bit
;               EAX.[01-00] = 00b, because aligned 32-bit access
; Output: EAX = 32-bit data, read from PCI config. space
;---
ReadPci32:
push rdx
mov dx,0CF8h ; DX = Address of CONFIG_ADDRESS port
cli
out dx,eax   ; Write CONFIG_ADDRESS
mov dl,0FCh  ; DX = Address of CONFIG_DATA port
in eax,dx    ; Read CONFIG_DATA
sti
pop rdx
ret

;--- Subroutine for wait key ---
; Input:  R15 = Global variables pool base address
; Output: RAX = UEFI Status
;         RDX = UEFI Key Code
;---
WaitKey:
push rcx r8 r9 r10 r11 rbp
NotReady:
mov rbp,rsp                       ; Save RSP
mov rcx,[r15+008h]                ; RCX = EFI_SYSTEM_TABLE address
mov rcx,[rcx+030h]                ; RCX = EFI_SYSTEM_TABLE.ConInput
and rsp,0FFFFFFFFFFFFFFF0h        ; This for stack alignment
xor	edx,edx		                    ; Otherwise bad output RDX.63-32
push rdx rdx
mov	rdx,rsp		                    ; Pointer to RDX image in the stack
sub rsp,32                        ; This for 4 parameters shadow
call qword [rcx+008h]             ; +08h for select function, RDX = String
mov rdx,[rsp+32]                  ; RDX = Key code
mov rsp,rbp                       ; Restore RSP
mov rcx,8000000000000006h         ; UEFI Status code = Not Ready
cmp rax,rcx
je NotReady
pop rbp r11 r10 r9 r8 rcx
ret

;--- Subroutine for print ASCII string ---
; Input:  R15 = Global variables pool base address
;         RSI = Source string address
; Output: None
;---
StringWrite:
push rax rcx rdx rdi rbp r8 r9 r10 r11
;--- Convert string from ASCII (8-bit) to UNICODE (16-bit) ---
mov rdi,UnicodeBuffer
mov rdx,rdi                       ; RDX used for next step
mov ah,0
@@:
lodsb                             ; Read 8-bit char
stosw                             ; Write 16-bit char, high byte = 0
cmp al,0                          ; Last 16-bit word must write 0000h
jne @b                            ; Cycle for string 
;--- Output UNICODE string, RDX=String address --- 
mov rbp,rsp                       ; Save RSP
mov rcx,[r15+008h]                ; RCX = EFI_SYSTEM_TABLE address
mov rcx,[rcx+040h]                ; RCX = EFI_SYSTEM_TABLE.ConOut
and rsp,0FFFFFFFFFFFFFFF0h        ; This for stack alignment
sub rsp,32                        ; This for 4 parameters shadow
call qword [rcx+008h]             ; +08h for select function, RDX = String
mov rsp,rbp                       ; Restore RSP
;--- Exit ---
pop r11 r10 r9 r8 rbp rdi rdx rcx rax
ret

;--- Subroutine for print 32-bit Decimal Number ---
; Input:  EAX = Number value
;         BL  = Template size, chars. 0=No template
;         RDI = Destination Pointer (flat)
; Output: RDI = Modified by string write = next position
;---
DecimalPrint32:
cld
push rax rbx rcx rdx
mov bh,80h-10
add bh,bl
mov ecx,1000000000
.MainCycle:
xor edx,edx
div ecx         ; Produce current digit
and al,0Fh
test bh,bh
js .FirstZero
cmp ecx,1
je .FirstZero
cmp al,0        ; Not actual left zero ?
jz .SkipZero
.FirstZero:
mov bh,80h      ; Flag = 1
or al,30h
stosb           ; Store char
.SkipZero:
push rdx
xor edx,edx
mov eax,ecx
mov ecx,10
div ecx
mov ecx,eax
pop rax
inc bh
test ecx,ecx
jnz .MainCycle
pop rdx rcx rbx rax
ret

;--- Subroutine for print 64/32/16/8/4-bit numbers ---
HexPrint64:   ; Entry point: Input:  RAX=Qword, RDI=Destination string address
push rax      ;              Output: RDI=Modified by str. write = next position
ror rax,32
call HexPrint32
pop rax
HexPrint32:   ; Entry point: Input:  EAX=Dword, RDI=Destination string address
push rax      ;              Output: RDI=Modified by str. write = next position
ror eax,16
call HexPrint16
pop rax
HexPrint16:   ; Entry point: Input:  AX=Word, RDI=Destination string address
push rax      ;              Output: RDI=Modified by str. write = next position
xchg al,ah
call HexPrint8
pop rax
HexPrint8:   ; Entry point: Input:  AL=Byte, RDI=Destination string address
push rax     ;             Output: RDI=Modified by str. write = next position
ror al,4
call HexPrint4
pop rax
HexPrint4:   ; Entry point: Input:  AL.[3-0]=Nibble, RDI=Dest. string address
cld          ;              Output: RDI=Modified by str. write = next position
push rax
and al,0Fh
cmp al,9
ja .HP4_AF
add al,'0'
jmp .HP4_Store
.HP4_AF:
add al,'A'-10
.HP4_Store:
stosb        ; Store 8-bit ASCII char
pop rax
ret

;--- Data ---
section '.data' data readable writeable

NameMsg:
DB  0Dh,0Ah
DB  '(C)IC Book Labs 07.03.2018'
DB  0Dh,0Ah,0 

ErrorMsg1:
DB  0Dh,0Ah
DB  'No USB3 xHCI controllers found.'
DB  0Dh,0Ah,0

ErrorMsg2:
DB  0Dh,0Ah
DB  'xHCI BAR detection failed.'
DB  0Dh,0Ah,0

ErrorMsg3:
DB  0Dh,0Ah
DB  'xHCI configuration invalid.'
DB  0Dh,0Ah,0

AnyKeyMsg:
DB  0Dh,0Ah
DB  'Press any key...',0

CrLfMsg:
DB  0Dh,0Ah,0

SelectMsg:
DB  0Dh,0Ah
DB  'Select controller 1-'
SelectEntry:
DB  '_ or ESC...',0

Screen1:
DB   0Dh, 0Ah, 'USB3 xHCI controllers list', 0Dh, 0Ah, 0Dh, 0Ah
DB  ' #   Bus Dev Fnc  VID  DID  SVID SDID Rev Bcl Sub PI   MMIO', 0

Screen2:
DB  'HCSPARAMS1',0
DB  'MMIO address(h)     Register           Value(h)    Connection',0
DB  'PORTSC='
NumberPort:
DB  '__ USB='
NumberUSB:
DB  '___',0

Screen3:
DB  0Dh,0Ah
DB  'Intel 8/9 Series chipset specific registers',0 
DB  'USB 2.0 Port Routing' , 0
DB  'USB 2.0 Port Routing Mask' , 0
DB  'USB 3.0 Port SuperSpeed Enable',0
DB  'USB 3.0 Port Routing Mask',0
DB  'USB 2.0 Port Disable Override',0
DB  'USB 3.0 Port Disable Override',0

PDsize = 23 ; String length, chars 
PortDecode:
DB  'Full speed  = 12 Mb/s ',0  ; Port_Speed=0001b
DB  'Low speed   = 1.5 Mb/s',0  ; Port_Speed=0010b
DB  'High speed  = 480 Mb/s',0  ; Port_Speed=0011b
DB  'Super speed = 5 Gb/s  ',0  ; Port_Speed=0100b
DB  'Super speed = 10 Gb/s ',0  ; Port_Speed=0101b
PortNotConnected:
DB 0
PortUnknown:
DB '?',0

CfgRegsCount = 6
CfgRegsList:
DB  0D0h  ; USB 2.0 Port Routing
DB  0D4h  ; USB 2.0 Port Routing Mask
DB  0D8h  ; USB 3.0 Port SuperSpeed Enable
DB  0DCh  ; USB 3.0 Port Routing Mask
DB  0E4h  ; USB 2.0 Port Disable Override
DB  0E8h  ; USB 3.0 Port Disable Override

;--- This structures don't reserve space in the file, because "?" ---
VariablesPool:
EfiHandle      DQ  ?              ; UEFI firmware parameter - Application Handle
EfiTable       DQ  ?              ; UEFI firmware parameter - Sys.Table
AsciiBuffer    DB  2048 DUP (?)   ; ASCII buffer
UnicodeBuffer  DB  4096 DUP (?)   ; UNICODE buffer, for Text Output Protocol
DataBuffer1    DB  2048 DUP (?)   ; Miscellaneous data buffer

;--- Relocation elements ---
section '.reloc' fixups data discardable

;--- End ---

