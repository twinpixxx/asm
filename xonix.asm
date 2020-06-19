.model small
.stack 100h
.data
videoStart 			dw 0B800h 
dataStart 			dw 0000h 

KPlus				equ 0Dh		
KMinus				equ 0Ch		
KRes				equ 13h		
KMoveUp 			equ 11h 	
KMoveDown 			equ 1Fh 	
KMoveLeft 			equ 1Eh 	
KMoveRight 			equ 20h 	
KExit	 			equ 01h 

xSize 				equ 80 	
elementsNum			equ 2000

generalWaitTime 	dw 0001h
enemyDelayCounter	db 00h		
heroDelayCounter	db 00h

flagPushOff 		db 00h
flagEmptyCell 		db 00h
flagMoveEnemy 		db 00h
flagVirus 			db 00h
flagOnField 		db 00h
flagDoNothing 		db 00h
flagDrawLastLand 	db 00h
flagStartFillMap 	db 00h
Percent				db 00h
WINCounter			dw 0000h

vBorderSym 			equ 0F7Ch
hBorderSym 			equ 0F16h
spaceSym 			equ 0020h 
fillSym 			equ 0820h 
landSym 			equ 0723h 

virusPos 			dw 2614h
virusPosOld 		dw 2614h
virusDir 			dw 0101h
virusSym	 		equ 8C02h

heroLife 			db 03h
heroDir 			dw 0000h
heroPos 			dw 2601h
heroPosOld 			dw 2601h
heroDelay			db 00h
heroSym 			equ 0902h
trackSym 			equ 092Ah

enemyOne 			dw 2010h
enemyOnePos 		dw 1209h
enemyOnePosOld 		dw 1209h
enemyOneDir 		dw 0101h

enemyTwo 			dw 2907h
enemyTwoPos 		dw 2710h
enemyTwoPosOld 		dw 2710h
enemyTwoDir 		dw 0000h

enemyDelay			db 00h
enemySym 			equ 0A01h

fillArray 			dw 5472 dup (?)
ptrcur				dw 0000h
ptrend				dw 0001h

screen 				dw xSize dup(hBorderSym)
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw xSize dup(hBorderSym)


area 				dw 0430h,0430h,0F25h,fillSym
					dw 0F4Ch,0F69h,0F66h,0F65h,0F3Ah,0F20h, fillSym


life				dw 0933h,  xSize-10 dup(fillSym), xSize dup(fillSym), xSize dup(fillSym), xSize dup(fillSym), xSize dup(fillSym)

GObannerWidth 		equ 9
GObannerLines		equ 1		 
GObannerPos			equ 0488h
GameOverBanner 		dw 0F47h, 0F41h, 0F4Dh, 0F45h, fillSym, 0F4Fh, 0F56h, 0F45h, 0F52h
					

WINbannerWidth 		equ 7
WINbannerLines		equ 1	 			  
WINbannerPos		equ 0488h
WINBanner 			dw 	0F59h, 0F4Fh, 0F55h, fillSym, 0F57h, 0F49h, 0F4Eh



.code
clearScreen MACRO
	push ax
	mov ax, 0003h 
	int 10h
	pop ax
ENDM

CheckBuffer MACRO ; 
	mov ah, 01h
	int 16h
ENDM

ReadFromBuffer MACRO ; 
	mov ah, 00h
	int 16h
ENDM

main:
	mov ax, @data
	mov ds, ax
	mov dataStart, ax
	mov ax, videoStart
	mov es, ax
	xor ax, ax
	jmp startGame
restart:					
	push es				
	push si
	push cx
	push bx

	mov bx,virusPosOld
	call getPosOffset
	mov si,bx
	mov screen[si],landSym
	mov bx,virusPos
	call getPosOffset
	mov si,bx
	mov screen[si],landSym

	mov bx,heroPos
	call getPosOffset
	mov si,bx
	cmp flagOnField,1
	je setSpace
	mov screen[si],landSym
	jmp nextmmark
setSpace:	
	mov screen[si],spaceSym
nextmmark:	
	
	xor bx,bx
	mov es,dataStart
	mov si,01E6h
	mov cx,04A0h
restartLoop:	
	mov screen[si],spaceSym
	add si,2
	dec cx
	inc bx
	cmp bx,004Ah
	jb restartLoop
	xor bx,bx
	add si,0Ch
	cmp cx,0000h
	ja restartLoop

	mov di,offset Area - offset screen
	mov screen[di],0430h
	inc di
	inc di
	mov screen[di],0430h
	mov WINCounter,0
	mov Percent,00h
	mov di, offset life - offset screen
	mov screen[di],0933h
	mov heroLife, 03h
	

	call killFlags
	mov enemyOnePos,1209h
	mov enemyOnePosOld,1209h
	mov enemyTwoPos, 2710h
	mov enemyTwoPosOld,2710h
	mov virusPos,2614h
	mov virusPosOld,2614h
	mov heroPos,2601h
	mov heroPosOld,2601h
	mov heroDir,0000h

	pop bx
	pop cx
	pop si
	pop es

startGame:
	
gameLoop:					
	call initScreen 		
	call Sleep				
	call updateGame			
	cmp WINCounter,0428h	
	jge	exitWin				
	jmp gameLoop

exitGO:
	clearScreen
	mov ax,GObannerWidth
	mov bx, offset GameOverBanner
	mov cx, GObannerLines
	mov dx, GObannerPos
	call printBanner	
	mov ah,7h
	int 21h
	cmp al,72h
	je jmpRestart
	jmp exit

exitWin:
	clearScreen
	mov ax,WINbannerWidth
	mov bx, offset WINbanner
	mov cx, WINbannerLines
	mov dx, WINbannerPos
	call printBanner
	
	mov ah,7h
	int 21h
	cmp al,72h
	je jmpRestart

exit:
	clearScreen
	mov ah, 4ch		
	int 21h

jmpRestart:
	jmp restart


checkPercent proc		
	push ax			
	push bx
	push dx
	push cx
	mov ax,WINCounter
	mov bx,000Ch
	div bl
	cmp al,Percent
	je checkPercentEnd
	xor cx,cx
	mov cl,Percent
	mov Percent,al
	sub al,cl
	mov cl,al
	
loopIncArea:	
	call incArea
	loop loopIncArea
checkPercentEnd:
	pop cx
	pop dx
	pop bx
	pop ax
	ret
endp checkPercent
	
printBanner proc		
	push es				
	push videoStart		
	pop es
	mov di, dx
	mov si, bx
	cld
loopPrintBanner:
	push cx
	mov cx, ax
	rep movsw
	add di, 2*xSize 
	sub di,ax
	sub di,ax
	pop cx
	loop loopPrintBanner
	pop es
ret
endp printBanner

drawSymbol proc 		
	push si 			
	push bx 			

	mov bx,ax			
	call getPosOffset	
	mov si,bx		

	pop bx

	mov screen[si],bx

	push bx

	mov bx, dx
	call getPosOffset

	mov si,bx
	mov screen[si],cx
	pop bx
	pop si
	ret
endp drawSymbol

incArea proc  					        
	push ax              
	push es               
	push si               
	push di               
	push cx
	mov es, videoStart    
	mov cx, 2 	  		  
	mov di, offset area  - offset screen +2	
							
loopArea:	              
	mov ax, screen[di]    
	cmp al, 39h	
	jne nineNotNow                      
	sub al, 9			 
	mov screen[di], ax               
	sub di, 2               
	loop loopArea         
	jmp incAreaEnd	      
                          
nineNotNow:               
	inc ax                
	mov screen[di], ax       
incAreaEnd:		          
	pop cx
	pop di                
	pop si                
	pop es                
	pop ax                
	ret                   
endp incArea  
							
initScreen proc				
	push ax					
	push cx					
	push dx
	push bx
drawit:
	cmp flagDrawLastLand,1
	je setLand

	cmp flagStartFillMap,1
	je setTrack

	cmp flagOnField,1
	je setTrack
	jmp setLand

setLand:
	mov bx, landSym
	jmp drawHero
setTrack:
	mov bx, trackSym
drawHero:
	mov ax, heroPosOld
	mov dx, heroPos
	mov cx, heroSym
	call drawSymbol

	mov ax,enemyOnePosOld
	mov bx, spaceSym
	mov dx, enemyOnePos
	mov cx, enemySym
	call drawSymbol

	mov ax,enemyTwoPosOld
	mov bx, spaceSym
	mov dx, enemyTwoPos
	mov cx, enemySym
	call drawSymbol

	mov ax,virusPosOld
	mov bx, landSym
	mov dx, virusPos
	mov cx, virusSym
	call drawSymbol

	cmp flagStartFillMap,1
	jne showScreen
	mov flagStartFillMap,0
	
	mov ptrend,0000h
	mov ptrcur,0000h

	push bx

	mov bx, enemyOnePos

	push si

	mov si, ptrcur
	mov fillArray[si], bx

	pop si
	pop bx

	call fillMap
	call clearArray

	mov ptrend,0000h
	mov ptrcur,0000h

	push bx
	mov bx, enemyTwoPos
	push si
	mov si,ptrcur
	mov fillArray[si], bx
	pop si
	pop bx

	call fillMap
	call clearArray

	mov ptrend,0000h
	mov ptrcur,0000h

	call changeMap

	call checkPercent
	jmp drawit

showScreen:

	mov si, offset screen

	xor di, di

	mov cx, elementsNum

	rep movsw

initScreenEnd:

	pop bx
	pop dx
	pop cx
	pop ax

	ret
endp initScreen

Sleep proc 					
	push ax 
	push bx 
	push cx
	push dx 
	mov ax, 00h 
	int 1Ah 
	add dx, generalWaitTime 
	mov bx, dx 

checkTimeLoop: 
	mov ax, 00h 
	int 1Ah
	cmp dx, bx
	jl checkTimeLoop 
	
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	ret 
endp

getPosOffset proc 			
	push ax 			
	push dx
	push cx

	xor ah, ah
	mov al, bl 
	mov dl, xSize 
	mul dl 
	mov dl, bh 
	xor dh, dh 
	add ax, dx 
	mov dx, 2 
	mul dx 
	mov bx, ax

	pop cx
	pop dx
	pop ax
	ret
endp getPosOffset

toNextSym proc 				
	push ax 				
	mov ax,dx
	mov bx,cx

	cmp al,01h
	jne incYpos
	dec bl
	jmp changeXpos

incYpos:
	inc bl

changeXpos:
	cmp ah,01h
	jne incXpos
	dec bh
	jmp toNextSymEnd

incXpos :
	inc bh

toNextSymEnd:
	pop ax
	ret
endp toNextSym

checkCellPushOff proc 		
	push bx 				
	push si 				
	call getPosOffset
	mov si, bx

	cmp flagVirus,1
	je virusMark

	cmp screen[si],heroSym
	jne nextCheck
	cmp flagOnField,1
	jne pushOffMark
	call setGameOver
nextCheck:
	cmp screen[si],landSym
	je pushOffMark

	cmp screen[si],virusSym
	je pushOffMark

	cmp screen[si],enemySym
	je pushOffMark

	cmp screen[si],trackSym
	je goMark
	jmp nonOfPushOffSymbols
goMark:
	call setGameOver
nonOfPushOffSymbols:
	mov flagPushOff, 0
	jmp checkCellPushOffEnd

virusMark:					; virus logic
	cmp screen[si],heroSym
	je setVirusGo

	cmp screen[si],landSym
	jne pushOffMark

	jmp nonOfPushOffSymbols2
setVirusGo:
	call setGameOver
nonOfPushOffSymbols2:
	mov flagPushOff,0
	jmp checkCellPushOffEnd
pushOffMark:
	mov flagPushOff, 1
checkCellPushOffEnd:
	pop si
	pop bx
	ret
endp checkCellPushOff

setRightCell proc 		
						
	cmp ah,00 			
	je _plus

	cmp al,01
	je to_yRC

	sub bh,1
	jmp setRightCellEnd
to_yRC:
	sub bl,1
	jmp setRightCellEnd
_plus:
	cmp al,01
	je to_xRC

	add bl,1
	jmp setRightCellEnd
to_xRC:
	add bh,1

setRightCellEnd:
	ret
endp setRightCell

setLeftCell proc 		
						
						
	cmp al,01
	je minus_

	cmp ah,01
	je to_yLC

	add bh,1
	jmp setLeftEnd
to_yLC:
	add bl,1
	jmp setLeftEnd

minus_:
	cmp ah,01
	je to_xLC

	sub bl,1
	jmp setLeftEnd
to_xLC:
	sub bh,1

setLeftEnd:
	ret
endp setLeftCell

checkCell proc 			
						
	call checkCellPushOff

	cmp flagPushOff,1
	je setEmptyFlag
	mov flagEmptyCell,1

	jmp checkCellEnd
setEmptyFlag:
	mov flagEmptyCell,0
checkCellEnd:
	ret
endp checkCell

setLeftLow proc 						
						
	cmp ah,01 			
	jne minus__ 		
						
	cmp al,01  			
	je to_yLL  			

	add bh,1
	jmp setLeftLowEnd
to_yLL:
	add bl,1
	jmp setLeftLowEnd
minus__:
	cmp al,01
	je to_xLL

	sub bl,1
	jmp setLeftLowEnd
to_xLL:
	sub bh,1

setLeftLowEnd:
	ret
endp setLeftLow

setRightLow proc 		
				
	cmp al,01
	je __plus

	cmp ah,01
	je to_yRL

	sub bh,1
	jmp setRightLowEnd
to_yRL:
	sub bl,1
	jmp setRightLowEnd
__plus:
	cmp ah,01
	je to_xRL

	add bl,1
	jmp setRightLowEnd
to_xRL:
	add bh,1

setRightLowEnd:
	ret
endp setRightLow

setBackCell proc 			
	cmp ah,01
	je incX
	dec bh
	jmp metka_
incX:
	inc bh

metka_:
	cmp al,01
	je incY
	dec bl
	jmp setBackCellEnd
incY:
	inc bl

setBackCellEnd:
	ret
endp setBackCell

pushOff1 proc						
	mov bx, cx 			
	mov ax,dx 
	call setLeftCell
	call setLeftLow
	call checkCell
	cmp flagEmptyCell,1
	je PF1setFlagAndChangeVector
	call pushOff2

	jmp pushOff1End
PF1setFlagAndChangeVector:
	mov flagMoveEnemy,1
	push cx
	mov cx, ax

	cmp ch,01
	je _setVec1

	mov al,01
	jmp __setVec1
_setVec1:
	mov al,00

__setVec1:
	cmp cl, 01
	je __setVec1_

	mov ah,00
	jmp setVec1End
__setVec1_:
	mov ah,01

setVec1End:
	pop cx
pushOff1End:
	ret
endp pushOff1
		
pushOff2 proc 		
	mov bx, cx 
	mov ax,dx
	call setRightCeLL
	call setRightLow
	call checkCell
	cmp flagEmptyCell,1
	je PF2setFlagAndChangeVector
	call pushOff3

	jmp pushOff2End
PF2setFlagAndChangeVector:
	mov flagMoveEnemy,1
	push cx
	mov cx, ax

	cmp ch,01
	je _setVec2

	mov al,00
	jmp __setVec2
_setVec2:
	mov al,01

__setVec2:
	cmp cl, 01
	je __setVec2_

	mov ah,01
	jmp setVec2End
__setVec2_:
	mov ah,00

setVec2End:
	pop cx
pushOff2End:
	ret
endp pushOff2
			
pushOff3 proc 	
				
	mov bx, cx
	mov ax, dx
	call setBackCell
	call checkCell
	cmp flagEmptyCell,1
	je PF3setFlagAndChangeVector

	mov flagMoveEnemy,0
	jmp pushOff3End
PF3setFlagAndChangeVector:
	mov flagMoveEnemy,1
	push cx
	mov cx, ax

	cmp ch,01
	je _setVec3

	mov ah,01
	jmp __setVec3
_setVec3:
	mov ah,00

__setVec3:
	cmp cl, 01
	je __setVec3_

	mov al,01
	jmp setVec3End
__setVec3_:
	mov al,00

setVec3End:
	pop cx
pushOff3End:
	ret
endp pushOff3
				
checkAndSetPushOff proc 
	call checkCellPushOff
	cmp flagPushOff,1
	je checkSurroundings

	push bx
	mov bx, cx
	mov ax, dx
	call setLeftCell
	call checkCell
	pop bx

	cmp flagEmptyCell,1
	je moveToNextCell

	push bx
	mov bx, cx
	mov ax,dx
	call setRightCell
	call checkCell
	pop bx

	cmp flagEmptyCell,1
	je moveToNextCell

	jmp pushOff3_

checkSurroundings: 			
	mov bx, cx				
	mov ax,dx
	call setLeftCell
	call checkCell

	cmp flagEmptyCell,1
	je checkRight_

	mov bx, cx
	mov ax,dx
	call setRightCell
	call checkCell

	cmp flagEmptyCell,1
	je pushOff2_
	jmp pushOff3_

checkRight_:
	mov bx, cx
	mov ax,dx
	call setRightCell
	call checkCell

	cmp flagEmptyCell,1
	je pushOff3_
	jmp pushOff1_

pushOff1_:
	call pushOff1
	jmp checkAndPushOffEnd
pushOff2_:
	call pushOff2
	jmp checkAndPushOffEnd
pushOff3_:
	call pushOff3
	jmp checkAndPushOffEnd
moveToNextCell:
	mov flagMoveEnemy,1
checkAndPushOffEnd:
	ret
endp checkAndSetPushOff

moveEnemy proc			
	call toNextSym
	call checkAndSetPushOff 
							
	ret
endp moveEnemy

moveEnemies proc			
	push dx
	push cx
	push ax
	push bx

	mov dx, enemyOneDir
	mov cx, enemyOnePos
	call moveEnemy

	cmp flagMoveEnemy,1
	je startMove1

	jmp nextEnemy2
startMove1:
	mov enemyOneDir,ax
	mov enemyOnePosOld, cx 
	mov enemyOnePos, bx

nextEnemy2:
	mov flagMoveEnemy,0

	mov dx, enemyTwoDir
	mov cx, enemyTwoPos
	call moveEnemy

	cmp flagMoveEnemy,1
	je startMove2

	jmp moveEnemiesEnd
startMove2:
	mov enemyTwoDir,ax
	mov enemyTwoPosOld, cx 
	mov enemyTwoPos, bx

moveEnemiesEnd:
	mov flagMoveEnemy,0

	pop bx
	pop ax
	pop cx
	pop dx
	ret
endp moveEnemies

moveVirus proc	

	push dx
	push cx
	push ax
	push bx

	mov flagVirus, 1

	mov dx, virusDir
	mov cx, virusPos
	call moveEnemy

	cmp flagMoveEnemy,1
	je startMoveV

	jmp moveViEnd
startMoveV:
	mov virusDir,ax
	mov virusPosOld, cx 
	mov virusPos, bx

moveViEnd:
	mov flagMoveEnemy,0
	mov flagVirus,0

	pop bx
	pop ax
	pop cx
	pop dx
	ret
endp moveVirus

setPos proc	
	push ax
	push bx

	mov bx, heroPos
	mov ax,heroDir
	cmp ax, 0001h
	je toLeft

	cmp ax, 0010h
	je toRight

	cmp ax, 0100h
	je toUp

	cmp ax,1000h
	je toDown

	jmp setPosEnd
toLeft:
	sub bh,1
	jmp setPosEnd
toRight:
	add bh,1
	jmp setPosEnd
toUp:
	sub bl,1
	jmp setPosEnd
toDown:
	add bl,1
setPosEnd:

	mov heroPos,bx

	pop bx
	pop ax
	ret
endp setPos

setDir proc	
	push bx	

	mov bx, heroDir

	cmp bx,0000h
	je nextComp

	cmp ax,0000h
	je notSetD
	jmp setD
nextComp:
	cmp ax,0000h
	je doN
	jmp setD
setD:
	mov heroDir,ax
	jmp setDirEnd
doN:
	mov flagDoNothing,1
notSetD:
setDirEnd:
	pop bx
	ret
endp setDir

respawn proc		
	push cx	
	push si	
	push es
	mov es,dataStart
	xor si,si
	mov cx,elementsNum

respawnLoop:

	cmp screen[si],heroSym
	je changeHero

	cmp screen[si],virusSym
	je changeVirus

	cmp screen[si],trackSym
	je changeTrack
	jmp respawnLoopEnd
changeVirus:
	mov screen[si],landSym
	jmp respawnLoopEnd
changeHero:
	cmp flagOnField,1
	je changeHeroSpace
	mov screen[si],landSym
	jmp respawnLoopEnd
changeHeroSpace:
	mov screen[si],spaceSym
	jmp respawnLoopEnd
changeTrack:
	mov screen[si],spaceSym

respawnLoopEnd:
	inc si
	inc si
	dec cx
	cmp cx, 0000h
	jne respawnLoop

	mov heroPos,2601h
	mov heroPosOLd, 2601h
	mov heroDir, 0000h

	mov virusPos,2614h
	mov virusPosOld,2614h
	mov virusDir,0101h

	pop es
	pop si
	pop cx
	ret 
endp respawn

killFlags proc	

	mov flagOnField,0
	mov flagDoNothing,1
	mov flagDrawLastLand,0
	mov flagStartFillMap,0

	ret
endp killFlags

setGameOver proc
	push es
	push ax
	push di

	mov es, videoStart
	mov di,offset life - offset screen
	mov ax, screen[di]
	dec ax
	mov screen[di],ax
	
	pop di
	pop ax
	pop es

	dec heroLife

	cmp heroLife,00h
	je jmpexitGo
	call respawn
	call killFlags
	jmp gameLoop
jmpexitGo:
	jmp exitGO 
	ret
endp setGameOver

moveHero proc
	push ax	
	push bx					
	push cx					
	push dx					
							
	mov flagDoNothing,0

	mov bx, heroPos
	push bx
	push cx
	mov cx,heroDir
	call setDir
	call setPos
	mov bx, heroPos
	mov dx, bx
	call getPosOffset
	mov heroDir,cx
	pop cx
	mov si,bx
	pop bx
	mov heroPos,bx

	cmp flagDoNothing,1
	je jmpMoveHeroEnd
	jmp moveHeromark
jmpMoveHeroEnd:					
	jmp moveHeroEnd				
moveHeromark:
	cmp screen[si], spaceSym
	jne _continue

	cmp dx,enemyOnePos		
	jne check2enemy
	call setGameOver
	jmp moveHeroEnd
check2enemy:
	cmp dx,enemyTwoPos
	jne checkonff
	call setGameOver
	jmp moveHeroEnd
checkonff:
	cmp flagOnField,1
	je	mark1
	mov flagDrawLastLand,1
	mov flagOnField,1
	jmp mark2
mark1:	
	mov flagDrawLastLand,0
	mov flagOnField,1 
mark2:
	call setDir
	push cx
		mov cx, heroPos
		mov heroPosOld,cx
		call setPos
	pop cx
	jmp moveHeroEnd
	_continue:
	cmp screen[si],enemySym
	je evt
	cmp screen[si],virusSym
	je evt
	cmp screen[si],trackSym
	je evt
	jmp __continue
	evt:
	call setGameOver
	jmp moveHeroEnd
__continue:
	cmp screen[si],hBorderSym
	je borders
	cmp screen[si],vBorderSym
	je borders
	jmp __continue_
borders:
	mov heroDir,0000h
	jmp moveHeroEnd
	
__continue_:
	cmp screen[si],landSym
	je chenckOnf

	jmp moveHeroEnd
chenckOnf:	
	cmp flagOnField,1
	jne notOnField
	
	mov flagOnField,0
	mov flagStartFillMap,1

	call setDir
	push cx
		mov cx, heroPos
		mov heroPosOld,cx
		call setPos
	pop cx
	mov heroDir,0000h
	jmp moveHeroEnd
notOnField:
	mov flagStartFillMap,0
	call setDir
	push cx
		mov cx, heroPos
		mov heroPosOld,cx
		call setPos
	pop cx
moveHeroEnd:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp moveHero

updateGame proc					
	push ax		

	xor ax,ax

	mov al, enemyDelay

	cmp enemyDelayCounter, al

	pop ax

	jne incEnemyDelay

	call moveVirus

	call moveEnemies

	mov enemyDelayCounter, 00h

	jmp checkHeroDelay
incEnemyDelay:
	inc enemyDelayCounter

checkHeroDelay:
	push ax
	xor ax,ax
	mov al,heroDelay
	cmp heroDelayCounter,al
	pop ax
	jne jmpIncHeroDelay
	jmp continueHeroMovement
jmpIncHeroDelay:
	jmp incHeroDelay
continueHeroMovement:
	mov heroDelayCounter,00h
	CheckBuffer
	jnz mainloop
	jz setNoKey
setNoKey:
	mov ax,0000h
	jmp noKeyPressed 

mainloop:
	ReadFromBuffer

	cmp ah,KRes
	je jmpToRestart

	cmp ah,KPlus
	je incEnemySpeed

	cmp ah,KMinus
	je decEnemySpeed

	cmp ah,KExit
	je jmpToExit

	jmp continueMainLoop
incEnemySpeed:
	cmp enemyDelay,01h
	jb noKeyPressed
	dec enemyDelay
	mov enemyDelayCounter,00h
	jmp noKeyPressed
decEnemySpeed:
	cmp enemyDelay,09h
	ja noKeyPressed
	inc enemyDelay
	mov enemyDelayCounter,00h
	jmp noKeyPressed
jmpToExit:
	jmp exit
jmpToRestart:
	jmp restart	

continueMainLoop:
	cmp ah,KMoveUp
	je setMoveUp

	cmp ah,KMoveDown
	je setMoveDown

	cmp ah,KMoveLeft
	je setMoveLeft

	cmp ah,KMoveRight
	je setMoveRight

	mov ax,0000h
	jmp noKeyPressed
setMoveLeft: ;
	mov ax,0001h
	jmp noKeyPressed ;
setMoveRight: ;
	mov ax,0010h
	jmp noKeyPressed ;
setMoveUp: ;
	mov ax,0100h
	jmp noKeyPressed ;
setMoveDown: ;
	mov ax,1000h
	jmp noKeyPressed
noKeyPressed:			
	call moveHero
	jmp updateGameEnd
incHeroDelay:
	inc heroDelayCounter
updateGameEnd:
	ret
endp

changeMap proc			
	push cx
	push si
	push es
	push bx

	xor bx,bx

	mov es,dataStart
	mov si,01E6h
	mov cx,04A0h

changeMapLoop:

	cmp screen[si],enemySym
	je chEnemy

	cmp screen[si],spaceSym
	je chLand

	cmp screen[si],trackSym
	je chLand
	jmp changeMapLoopEnd

chEnemy:
	mov screen[si],spaceSym
	jmp changeMapLoopEnd

chLand:
	inc WINCounter
	mov screen[si],landSym

changeMapLoopEnd:
	inc si
	inc si
	dec cx
	inc bx
	cmp bx,004Ah
	jb changeMapLoop
	xor bx,bx
	add si,0Ch
	cmp cx, 0000h
	jne changeMapLoop

	pop bx
	pop es
	pop si
	pop cx
	ret
endp changeMap

clearArray proc					
	push si
clearArrLoop:
	mov si,ptrend
	mov fillArray[si],0000h

	dec ptrend
	dec ptrend
	cmp ptrend,0FFFh
	jb continueClear
	inc ptrend
	inc ptrend
continueClear:
	cmp ptrend,0000h
	jne clearArrLoop

	mov si,ptrend
	mov fillArray[si],0000h

	pop si
	ret
endp clearArray

fillMap proc

	push si	
	push bx		

fillMapLoop:
	
	push si

	mov si, ptrcur
	mov bx, fillArray[si]

	pop si

right:

	push bx
	inc bh
	push bx
	call getPosOffset
	mov si, bx
	pop bx
	cmp screen[si], spaceSym
	jne left

	call fillEnemy

left:

	pop bx

	push bx
	dec bh
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne up

	call fillEnemy

up:
	pop bx
	push bx
	dec bl
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne down

	call fillEnemy

down:
	pop bx

	push bx
	inc bl
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne fillMapLoopEnd

	call fillEnemy

fillMapLoopEnd:
	pop bx

	inc ptrcur
	inc ptrcur
	push ax
	mov ax,ptrcur
	cmp ax,ptrend
	pop ax
	jle jmpfillMapLoop

	jmp fillMapEnd
jmpfillMapLoop:
	jmp fillMapLoop

fillMapEnd:
	pop bx
	pop si
	ret
endp fillMap

fillEnemy proc
	mov screen[si], enemySym

	push di

	inc ptrend
	inc ptrend

	mov di,ptrend
	mov fillArray[di],bx

	pop di

	ret
endp fillEnemy

end main
