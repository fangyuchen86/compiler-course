.globl _scheme_entry 
 _scheme_entry:  
 movq $5, %rax 
 movq $1, %rbx 
 imulq %rax, %rbx 
 subq $1, %rax 
 imulq %rax, %rbx 
 subq $1, %rax 
 imulq %rax, %rbx 
 subq $1, %rax 
 imulq %rax, %rbx 
 movq %rbx, %rax 
ret