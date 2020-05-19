    ;; Prints "Ahoy!" using a C library
    ;;
    ;; nasm -felf64 ahoy.asm && gcc ahoy.o && ./a.out

    global main
    extern puts

    section .text
main:
    lea rdi, [rel message]
    call puts WRT ..plt
    ret

    section .rodata
message:
    db "Ahoy!", 0               ; String terminated with \n
