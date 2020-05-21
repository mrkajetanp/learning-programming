; A 64-bit command line application to compute x^y.
; nasm -felf64 power.asm && gcc -o power power.o
; Syntax: power x y

    global main
    extern printf
    extern puts
    extern atoi

    section .text
main:
    push r12                    ; save callee-save registers
    push r13
    push r14
    ;;  Stack already aligned for calls
    cmp rdi, 3                  ; we need exactly 2 arguments
    jne error1

    mov r12, rsi                ; argv

    ;; ecx - countdown to 0
    ;; esi - base value
    ;; eax - running product

    mov rdi, [r12 + 8*2]        ; argv[2]
    call atoi WRT ..plt         ; y in eax
    cmp eax, 0                  ; disallow negative exponents
    jl error2
    mov r13d, eax               ; y in r13d

    mov rdi, [r12 + 8]        ; argv[1]
    call atoi WRT ..plt       ; x in eax
    mov r14d, eax             ; x in r14d

    mov eax, 1                  ; start with result = 1
check:
    test r13d, r13d             ; counting y down to 0
    jz gotit
    imul eax, r14d
    dec r13d
    jmp check

gotit:
    lea rdi, [rel answer]
    movsxd rsi, eax
    xor rax, rax
    call printf WRT ..plt
    jmp done

error1:
    lea edi, [rel badArgumentCount]
    call puts WRT ..plt
    jmp done

error2:
    lea edi, [rel negativeExponent]
    call puts WRT ..plt

done:
    pop r14
    pop r13
    pop r12

    xor rax, rax
    ret

answer:
    db  "%d", 10, 0
badArgumentCount:
    db      "Requires exactly two arguments", 10, 0
negativeExponent:
    db      "The exponent may not be negative", 10, 0
