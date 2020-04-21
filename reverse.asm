.model small
.stack 256
.data
    helloMassage db "Input str", 10, 13, "$"
    input db 200, 201 dup('$')
    delim db 10, 13, "$"
.code
jmp start
 
print_str macro str
    mov ah, 09h
    mov dx, offset str
    int 21h
endm print_str
 
reverse proc           
    push ax
    push di
    push si
 
    reverse_loop:
        mov ah, [di]
        mov al, [si]
        mov [di], al
        mov [si], ah
        inc si
        dec di
        cmp si, di
    jl reverse_loop
 
    pop si
    pop di
    pop ax
    ret
reverse endp
 
start:
    mov ax, @data
    mov ds, ax
    mov es, ax
 
    print_str helloMassage
 
    mov ah, 0ah
    lea dx, input
    int 21h
 
    print_str delim
 
    xor cx, cx
    mov cl, input[1]
    mov di, offset input+2
    add di, cx
    mov [di], '$'
 
    lea si, input+2
    lea di, input+2
    mov al, ' '
    find_eof:
        repne scasb
        jcxz to_reverse 
        dec di     
        to_reverse:
        dec di
        call reverse
        inc di            
        cmp [di], '$'     
            je ende
        inc di            
        mov si, di     
    jmp find_eof
 
    ende:  
    print_str input+2
    mov ah, 4ch
    int 21h
    end start