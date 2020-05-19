
    global maxofthree
    section .text

maxofthree:
    mov rax, rdi                ; rax (result) initially has x
    cmp rax, rsi                ; is x < y?
    cmovl rax, rsi              ; if so, set result to y
    cmp rax, rdx                ; is max(x,y) < z?
    cmovl rax, rdx              ; if so, set the result to z
    ret                         ; return max from rax
