    ;; Prints "Ahoy!" using a C library
    ;;
    ;; nasm -felf64 ahoy.asm && gcc ahoy.o && ./a.out

    global main
    extern puts

    section .text
main:
    lea rdi, [rel message]      ; set first argument to str pointer
    call puts WRT ..plt         ; puts(message);
    xor rax, rax                ; set exit code to 0
    ret

    section .rodata
message:
    db "Ahoy!", 0               ; String terminated with \n
