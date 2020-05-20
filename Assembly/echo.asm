; A 64-bit program that displays its command line arguments, one per line.
; On entry, rdi will contain argc and rsi will contain argv.
; nasm -felf64 echo.asm && gcc echo.o && ./a.out dog 22 -zzz "hi there"

    global main
    extern puts
    section .text
main:
    push rdi                    ; save registers that puts uses
    push rsi
    sub rsp, 8                  ; must align stack before the call

    mov rdi, [rsi]
    call puts WRT ..plt

    add rsp, 8                  ; restore rsp to where it was
    pop rsi                     ; restore registers
    pop rdi

    add rsi, 8                  ; point to next argument
    dec rdi                     ; decrement the counter
    jnz main                    ; repeat if there are arguments left

    xor rax, rax
    ret
