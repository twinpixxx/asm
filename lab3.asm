.model  small
.stack  100h
.data
            
maxSize equ 30            
            
arrSize db  ?
msgEnterSize db  0Dh,'enter size: $'
msgEnterLow db  0Dh,'enter low: $'  
msgEnterHigh db  0Dh,'enter high: $'  
msgEnter db  0Dh,'enter num: $'
                                
errInput db  0Dh,'bad input',0Ah, '$' 
errHigh db  0Dh,'high must be higher than low', 0Ah, '$' 
errSize db  0Dh,'length must be between 0 and 30', 0Ah, '$'

enterStr db  0Ah, 0Dh, '$'
answ db  2 dup(0)
msgResult db  0Dh, 'result: $'
Buffer db  ?              
MaxNumLen db  5  
Len db  ?
buff db  5 dup (0)              
minus db  0  
Array db  maxSize dup (0) 
LowerBound db  ?
HigherBound db  ?
                              


.code    


start:                            
mov ax,@data                      
mov ds,ax                         
                                  
xor ax,ax                         
                                  
call input                        
call Do                           
call output                       
                                  
                                  
input proc                        
    call inputLowerBound          
    call inputHigherBound         
    call inputarrSize         
    call inputArray               
                                  
    ret                           
endp     


inputLowerBound proc
    mov cx, 1                         
    inputLowerBoundLoop:
       call ShowInputLowerBoundMsg                    
       call inputElementBuff          
       
       test ah, ah
       jnz inputLowerBoundLoop 
       
       mov bl, Buffer 
       mov LowerBound, bl
    loop inputLowerBoundLoop                
    ret      
endp    

inputHigherBound proc                                    
    mov cx, 1                         
    inputHigherBoundLoop:
       call ShowInputHigherBoundMsg                    
       call inputElementBuff         
              
       test ah, ah
       jnz inputHigherBoundLoop 
       
       mov ah, LowerBound
       cmp Buffer,ah                                          
       jnl inputHigherBoundLoop_OK
       
       call ShowerrHigh 
       jmp inputHigherBoundLoop
       
       inputHigherBoundLoop_OK:
       
       mov bl, Buffer 
       mov HigherBound, bl
    loop inputHigherBoundLoop
    ret      
endp     

inputarrSize proc   
    mov cx, 1           
    inputarrSizeLoop:
       call ShowInputarrSizeMsg                    
       call inputElementBuff          
       
       test ah, ah
       jnz inputarrSizeLoop 
       
       cmp Buffer, maxSize
       jg inputarrSizeLoop_FAIL   
       
       cmp Buffer, 0
       jg inputarrSizeLoop_OK   
       ;jmp inputarrSizeLoop_FAIL
       
       inputarrSizeLoop_FAIL:
       
       call ShowerrSize 
       jmp inputarrSizeLoop
       
       inputarrSizeLoop_OK:
       
       mov bl, Buffer 
       mov arrSize, bl                 
    loop inputarrSizeLoop     
    ret      
endp 

inputArray proc
    xor di,di                     
                                               
    mov cl,arrSize            
    inputArrayLoop:
       call ShowInputMsg                    
       call inputElementBuff      
       
       test ah, ah
       jnz inputArrayLoop
       
       mov bl, Buffer 
       mov Array[di], bl
       inc di                     
    loop inputArrayLoop           
    ret      
endp  


resetBuffer proc
    mov Buffer, 0    
    ret
endp    

inputElementBuff proc             
    push cx                      
    inputElMain:                 
        call resetBuffer        
        
        mov ah,0Ah            
        lea dx, MaxNumLen       
        int 21h                
                               
        mov dl,10               
        mov ah,2               
        int 21h            
                              
        cmp Len,0                
        je errInputEl            
                                
        mov minus,0             
        xor bx,bx               
                                  
        mov bl,Len                
        lea si,Len                
                                  
        add si,bx                 
        mov bl,1                  
                                  
                                  
        xor cx,cx                 
        mov cl,Len                
        inputElLoop:              
            std                   
            lodsb                 
                                  
            call checkSym         
                                  
            cmp ah,1              
            je errInputEl         
                                  
            cmp ah,2              
            je nextSym            
                                  
            sub al,'0'            
            mul bl                
                                  
            test ah,ah            
                                  
            jnz errInputEl        
                                  
            add Buffer,al      
                                  
            jo errInputEl         
            js errInputEl        
                                
            mov al,bl            
            mov bl,10             
            mul bl                
                                  
            test ah,ah            
            jz ElNextCheck        
                                  
                                  
            cmp ah,3              
            jne errInputEl        
                                  
                                  
            ElNextCheck:          
                mov bl,al         
                jmp nextSym       
                                  
                                  
            errInputEl:           
                call ShowErrorInputMsg   
                jmp exitInputEl          
                                  
            nextSym: 
            xor ah, ah            
        loop inputElLoop          
                                  
    cmp minus,0                   
    je exitInputEl                
    neg Buffer                    
                                  
    exitInputEl:                  
    pop cx                        
    ret                           
endp 
      
                                  
checkSym proc                     
    cmp al,'-'                    
    je minusSym                   
                                  
    cmp al,'9'                    
    ja errCheckSym                
                                  
    cmp al,'0'                    
    jb errCheckSym                
                                  
    jmp exitCheckGood             
                                  
    minusSym:                     
        cmp si,offset Len         
        je exitWithMinus          
                                  
    errCheckSym:                  
        mov ah,1                  
        jmp exitCheckSym          
                                  
    exitWithMinus:                
        mov ah,2                  
        mov minus, 1            
        cmp Len, 1                
        je errCheckSym            
                                  
        jmp exitCheckSym          
                                  
    exitCheckGood:                
        xor ah,ah                 
                                  
    exitCheckSym:                 
        ret                       
endp                              
                                  
ShowErrorInputMsg proc                   
    lea dx, errInput      
    mov ah, 09h                   
    int 21h                       
    ret                           
endp                              
      

ShowInputarrSizeMsg proc
    push ax
    push dx
      
    mov ah,09h                      
    lea dx, msgEnterSize           
    int 21h  
    
    pop ax
    pop dx 
     
    ret
endp       
         
ShowInputLowerBoundMsg proc
    push ax
    push dx
      
    mov ah, 09h                      
    lea dx, msgEnterLow           
    int 21h  
    
    pop ax
    pop dx 
     
    ret
endp    

ShowInputHigherBoundMsg proc
    push ax
    push dx
      
    mov ah,09h                      
    lea dx, msgEnterHigh           
    int 21h  
    
    pop ax
    pop dx 
     
    ret
endp  
                                  
ShowInputMsg proc                     
                  
                                  
    mov ah,09h                    
    lea dx, msgEnter           
    int 21h 
                        
    ret                           
endp    

ShowerrHigh proc
    push ax
    push dx
      
    mov ah,09h                      
    lea dx, errHigh           
    int 21h  
    
    pop ax
    pop dx 
     
    ret
endp       

ShowerrSize proc
    push ax
    push dx
      
    mov ah,09h                      
    lea dx, errSize           
    int 21h  
    
    pop ax
    pop dx 
     
    ret
endp

                                  
CheckMatch proc                   
    mov ah, LowerBound              
    cmp Array[di],ah                                                     
    jl notMatch                  
                               
    mov ah, HigherBound           
    cmp Array[di],ah              
    jg notMatch                   
                                
    inc bx                       
                                  
    notMatch:                     
    ret                           
endp                              
                                  
Do proc                           
    xor bx, bx                    
    mov cl,arrSize            
    xor di, di                    
    DoLoop:                     
        call CheckMatch          
        inc  di                  
    loop DoLoop                   
    ret                           
endp                              
                                  
output proc                       
    lea dx, msgResult                                                                
    mov ah, 09h
    int 21h
            
    mov ax, bx         
    mov al, bl
    mov bl, 10
    div bl
                   
    xor di, di    
    inc di
    mov answ[di], ah
    add answ[di], '0'
    
    test al, al 
    jz lessThanTen1
    
    dec di
    mov answ[di], al                      
    add answ[di], '0'           
           
    lessThanTen1:                      
    
    lea dx, answ
    mov ah, 09h 
    int 21h      
    
    lea dx, enterStr
    mov ah, 09h 
    int 21h  
        
    xor ax, ax                              
    mov ah,4ch                    
    int 21h                       
    ret                           
endp                              
                                  
end start                         