
    global _start

    section .text

_start:
    mov rax, 1                  ; syscall code for write into rax/R0
    mov rdi, 1                  ; file handle 1 - stdout into rdi/R7
    mov rsi, message            ; output string pointer into rsi/R6
    mov rdx, 13                 ; number of bytes into rdx/R2
    syscall                     ; performing the write

    mov rax, 60                 ; syscall code for exit
    xor rdi, rdi                ; 0 into rdi/R7 - exit code 0
    syscall                     ; perform the exit

    section .data

message:
    db "Hello, World!", 10
