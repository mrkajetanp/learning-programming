    ;; Prints 10 first Fibonacci numbers
    ;;
    ;; nasm -felf64 fib.asm && gcc fib.o && ./a.out

    global main
    extern printf

    section .text

main:
    push rbx                    ; save the register value
    mov ecx, 10                 ; ecx - counter - 10 times
    xor rax, rax                ; rax - current number
    xor rbx, rbx                ; rbx - next number
    inc rbx                     ; rbx (next number) starts at 1

print:
    ;; Printf may destroy rax & rcx so they have to be saved beforehand

    push rax                    ; save on the stack
    push rcx

    lea rdi, [rel format]       ; first parameter = format
    mov rsi, rax                ; second parameter = current_number
    xor rax, rax                ; printf is varargs, clean R0

    ;; rbx, rax, rcx (3*8 byte) were pushed so stack is aligned
    call printf WRT ..plt       ; printf(format, current_number)

    pop rcx                     ; restore registers after the call
    pop rax

    mov rdx, rax                ; save the current number
    mov rax, rbx                ; next number is now current
    add rbx, rdx                ; get the new next number
    dec ecx                     ; decrement the counter
    jnz print                   ; if not done counting, jump to print

    pop rbx                     ; restore rbx to its original state
    xor rax, rax                ; set exit code to 0
    ret

format:
    db "%d", 10, 0
