typedef long long sysarg_t;
extern sysarg_t syscall(sysarg_t number, ...);

#define NULL        ((void *)0)
#define __NR_execve 59
#define __NR_exit   60

void output(void) {
    /* Command string: run as, then ld */
    const char cmd[] = "as output.s -o output.o && ld output.o -o program && "
                       "rm output.o";

    /* argv for: /bin/sh -c "<cmd>" */
    char *const argv[] = {"sh", "-c", (char *)cmd, NULL};
    char *const envp[] = {NULL};

    /* execve("/bin/sh", argv, envp) */

    syscall(__NR_execve, "/bin/sh", argv, envp);

    /* if execve fails, exit(1) */
    syscall(__NR_exit, 1);
}
