        bits 64

        extern __libamp_start_main

        section .text

        global _start
_start:
        mov rbp, 0

        pop rdi
        mov rsi, rsp
        lea rdx, [rsp + rdi * 8 + 8]
        
        push rbp
        push rbp

        and rsp, ~0xf
        mov rcx, rsp
        
        call __libamp_start_main

        mov rdi, rax
        jmp _exit

        global _exit
_exit:
        mov rax, 60
        syscall
