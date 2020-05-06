.model	small
.stack	256h
.data           
MaxArrayLength              equ 30            
            
RangeErrorMsg               db 0Ah,0Dh,'bad range$' 
ErrorInputMsgStr            db 0Ah,0Dh,'bad value!',0Ah,0Dh, '$' 
InputMsgStr                 db 0Ah, 0Dh,'enter number: $'       
InputLeftLimit              db 0Ah, 0Dh,'enter left: $' 
InputRightLimit             db 0Ah, 0Dh,'enter right: $'
AnswerMsg                   db 0Ah, 0Dh,'number of elements in the range: $'
nextStr                     db 0Ah,0Dh,'$'
ArrayLength                 dw MaxArrayLength
LeftLimit                   dw 0
RightLimit                  dw 0                        
NumBuffer                   dw 0
NumLength                   db 7
EnterredNum                 db 9 dup('$')                            
minus                       db 0  
Array                       dw MaxArrayLength dup (0)

.code      

start:                            
        mov	ax,@data                      
        mov	ds,ax                         
                                  
        xor	ax,ax                         
                      
        call inputRange                            
        call inputArr                   
        call CountElements                
        mov ax,4C00h
        int 21h
                                  
inputArr proc                               
        call inputArray                                     
    ret                           
endp     
    
inputArray proc
       xor di,di                     
                                               
       mov cx,ArrayLength            
inputArrayLoop:
       push dx
       lea dx, InputMsgStr
       ShowMsg
       pop dx
       push cx                    
       call inputElementBuff
       pop cx      
       
       mov Array[di], ax 

       add di,2                     
       loop inputArrayLoop           
    ret      
endp   

resetNumBuffer proc
       mov NumBuffer, 0    
    ret
endp    

inputElementBuff proc                                     
     
        xor ax,ax
        xor cx,cx
    
        mov al,NumLength
    
        mov [EnterredNum],al
        mov [EnterredNum+1],0
        lea dx,EnterredNum
        call input
    
        mov cl,[EnterredNum+1]    
        lea si,EnterredNum         
        add si,2                   
    
        xor ax,ax 
        xor bx,bx
        xor dx,dx
        mov dx,10        
NextSym:
        xor ax,ax
        lodsb
        cmp bl,0                  
        je checkMinus
    
checkSym:
         
         cmp al,'0'
         jl badNum      
         cmp al,'9'
         jg badNum      
         
         sub ax,'0'     
         mov bx,ax
         xor ax,ax
         mov ax,NumBuffer
         
         imul dx        
         jo badNum     
         cmp minus,1
         je doSub
         add ax, bx
comeBack:
         jo badNum 
         mov NumBuffer,ax
         mov bx,1
         mov dx,10
         
         loop NextSym 
    
         mov ax,NumBuffer
    
         mov minus,0
        
finish: 
         call resetNumBuffer                        
         ret
          
doSub:
         sub ax,bx  
         jmp comeBack       
checkMinus:
         inc bl
         cmp al, '-'
    
         je SetMinus
    
         jmp checkSym
                  
SetMinus:
         mov minus,1        
         dec cx
         cmp cx,0
         je badNum
         jmp NextSym
    
badNum:
         clc
         mov minus,0
         call ErrorInput
         call resetNumBuffer
         jmp inputElementBuff                            
endp
     
input proc near
        mov ah,0Ah
        int 21h
        ret
input endp

ErrorInput proc                   
        lea dx, ErrorInputMsgStr      
        mov ah, 09h                   
        int 21h                       
    ret                           
endp                                      
                                  
ShowMsg macro            
        mov ah,09h     
        int 21h                             
endm

Rangeerror proc 
        push ax
        push dx
        lea dx, rangeerrormsg
        ShowMsg
        pop dx
        pop ax
        jmp inpRight     
endp
inputRange proc 
       push ax
       push dx
       push cx
              
       lea dx, InputLeftLimit 
       ShowMsg 
       call inputElementBuff       
       mov Leftlimit,ax
inpRight:
       lea dx, InputRightLimit 
       ShowMsg 
       call inputElementBuff
       cmp Leftlimit, ax
       jg rangeerror
       mov RightLimit,ax 
       
       pop cx 
       pop dx
       pop ax  
     ret                           
endp   
;
CountElements proc 
        xor di,di
                          
        mov cx, ArrayLength
        cmp cx,0
        je endFind 
        xor bx,bx
        mov bx,0
        mov dx,Array[di]   
find:             
         cmp dx, LeftLimit
         jge highlimit
         jl nexxt
highlimit:
         cmp dx, RightLimit
         jle found   
         jg nexxt        
found: 
         inc bx          
nexxt:
         add di,2
         mov dx,Array[di]
         loop find 
endFind: 
        lea dx,nextStr
        mov ah,09h
        int 21h
        lea dx, AnswerMsg
        ShowMsg  
        mov ax, bx 
        call outint           
    ret                           
endp  

OutInt proc
        push bx
        push cx
        push dx     

        xor     cx, cx 
        mov     bx, 10 
division:
        xor     dx,dx
        div     bx     

        push    dx    
        inc     cx

        test    ax, ax
        jnz     division  

        mov     ah, 02h
outt:
        pop     dx

        add     dl, '0' 
        int     21h

        loop    outt
        pop dx
        pop cx 
        pop bx
    ret      
OutInt endp;
                     
