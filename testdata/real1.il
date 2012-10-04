addr 0x8631a0 @asm "test   $0x1,%rdi"
label pc_0x8631a0
T_t:u32 = R_EAX:u32
R_EAX:u32 = R_EAX:u32 - 1:u32
R_OF:bool = high:bool((T_t:u32 ^ 1:u32) & (T_t:u32 ^ R_EAX:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ T_t:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32
addr 0x8631a1 @asm "test   $0x1,%edi"
label pc_0x8631a1
T_t_81:u32 = R_EDI:u32 & 1:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(T_t_81:u32 >> 7:u32 ^ T_t_81:u32 >> 6:u32 ^ T_t_81:u32 >> 5:u32 ^
            T_t_81:u32 >> 4:u32 ^ T_t_81:u32 >> 3:u32 ^ T_t_81:u32 >> 2:u32 ^
            T_t_81:u32 >> 1:u32 ^ T_t_81:u32)
R_SF:bool = high:bool(T_t_81:u32)
R_ZF:bool = 0:u32 == T_t_81:u32
addr 0x8631a7 @asm "je     0x00000000008631bc"
label pc_0x8631a7
cjmp R_ZF:bool, 0x8631bc:u32, "nocjmp0"
label nocjmp0
addr 0x8631a9 @asm "test   $0x1,%rsi"
label pc_0x8631a9
T_t_82:u32 = R_EAX:u32
R_EAX:u32 = R_EAX:u32 - 1:u32
R_OF:bool = high:bool((T_t_82:u32 ^ 1:u32) & (T_t_82:u32 ^ R_EAX:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ T_t_82:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32
addr 0x8631aa @asm "test   $0x1,%esi"
label pc_0x8631aa
T_t_83:u32 = R_ESI:u32 & 1:u32
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(T_t_83:u32 >> 7:u32 ^ T_t_83:u32 >> 6:u32 ^ T_t_83:u32 >> 5:u32 ^
            T_t_83:u32 >> 4:u32 ^ T_t_83:u32 >> 3:u32 ^ T_t_83:u32 >> 2:u32 ^
            T_t_83:u32 >> 1:u32 ^ T_t_83:u32)
R_SF:bool = high:bool(T_t_83:u32)
R_ZF:bool = 0:u32 == T_t_83:u32
addr 0x8631b0 @asm "je     0x00000000008631bc"
label pc_0x8631b0
cjmp R_ZF:bool, 0x8631bc:u32, "nocjmp1"
label nocjmp1
addr 0x8631b2 @asm "lea    -0x1(%rdi),%rax"
label pc_0x8631b2
T_t_84:u32 = R_EAX:u32
R_EAX:u32 = R_EAX:u32 - 1:u32
R_OF:bool = high:bool((T_t_84:u32 ^ 1:u32) & (T_t_84:u32 ^ R_EAX:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ T_t_84:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32
addr 0x8631b3 @asm "lea    -0x1(%rdi),%eax"
label pc_0x8631b3
R_EAX:u32 = R_EDI:u32 + -1:u32
addr 0x8631b6 @asm "add    %rsi,%rax"
label pc_0x8631b6
T_t_85:u32 = R_EAX:u32
R_EAX:u32 = R_EAX:u32 - 1:u32
R_OF:bool = high:bool((T_t_85:u32 ^ 1:u32) & (T_t_85:u32 ^ R_EAX:u32))
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ T_t_85:u32 ^ 1:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32
addr 0x8631b7 @asm "add    %esi,%eax"
label pc_0x8631b7
T_t1:u32 = R_EAX:u32
T_t2:u32 = R_ESI:u32
R_EAX:u32 = R_EAX:u32 + T_t2:u32
R_CF:bool = R_EAX:u32 < T_t1:u32
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ T_t1:u32 ^ T_t2:u32))
R_OF:bool = high:bool((T_t1:u32 ^ ~T_t2:u32) & (T_t1:u32 ^ R_EAX:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32
addr 0x8631b9 @asm "jo     0x00000000008631bc"
label pc_0x8631b9
cjmp R_OF:bool, 0x8631bc:u32, "nocjmp2"
label nocjmp2
addr 0x8631bb @asm "retq   "
label pc_0x8631bb
T_ra:u32 = mem:?u32[R_ESP:u32, e_little]:u32
R_ESP:u32 = R_ESP:u32 + 4:u32
jmp T_ra:u32 @str "ret"
addr 0x8631bc @asm "jmpq   0x0000000000866fd0"
label pc_0x8631bc
jmp 0x866fd0:u32
addr 0x8631c1
  @asm "data32 data32 data32 data32 data32 nopw %cs:0x0(%rax,%rax,1)"
label pc_0x8631c1
addr 0x8631ca @asm "test   %al,(%rax)"
label pc_0x8631ca
T_t_89:u8 = mem:?u32[R_EAX:u32, e_little]:u8 & low:u8(R_EAX:u32)
R_OF:bool = false
R_CF:bool = false
R_AF:bool = unknown "AF is undefined after and":bool
R_PF:bool =
  ~low:bool(T_t_89:u8 >> 7:u8 ^ T_t_89:u8 >> 6:u8 ^ T_t_89:u8 >> 5:u8 ^
            T_t_89:u8 >> 4:u8 ^ T_t_89:u8 >> 3:u8 ^ T_t_89:u8 >> 2:u8 ^
            T_t_89:u8 >> 1:u8 ^ T_t_89:u8)
R_SF:bool = high:bool(T_t_89:u8)
R_ZF:bool = 0:u8 == T_t_89:u8
addr 0x8631cc @asm "add    %al,(%rax)"
label pc_0x8631cc
T_t1_90:u8 = mem:?u32[R_EAX:u32, e_little]:u8
T_t2_91:u8 = low:u8(R_EAX:u32)
mem:?u32 =
  mem:?u32 with [R_EAX:u32, e_little]:u8 =
  mem:?u32[R_EAX:u32, e_little]:u8 + T_t2_91:u8
R_CF:bool = mem:?u32[R_EAX:u32, e_little]:u8 < T_t1_90:u8
R_AF:bool =
  0x10:u8 ==
  (0x10:u8 & (mem:?u32[R_EAX:u32, e_little]:u8 ^ T_t1_90:u8 ^ T_t2_91:u8))
R_OF:bool =
  high:bool((T_t1_90:u8 ^ ~T_t2_91:u8) &
            (T_t1_90:u8 ^ mem:?u32[R_EAX:u32, e_little]:u8))
R_PF:bool =
  ~low:bool(mem:?u32[R_EAX:u32, e_little]:u8 >> 7:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 6:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 5:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 4:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 3:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 2:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 1:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8)
R_SF:bool = high:bool(mem:?u32[R_EAX:u32, e_little]:u8)
R_ZF:bool = 0:u8 == mem:?u32[R_EAX:u32, e_little]:u8
addr 0x8631ce @asm "add    %al,(%rax)"
label pc_0x8631ce
T_t1_92:u8 = mem:?u32[R_EAX:u32, e_little]:u8
T_t2_93:u8 = low:u8(R_EAX:u32)
mem:?u32 =
  mem:?u32 with [R_EAX:u32, e_little]:u8 =
  mem:?u32[R_EAX:u32, e_little]:u8 + T_t2_93:u8
R_CF:bool = mem:?u32[R_EAX:u32, e_little]:u8 < T_t1_92:u8
R_AF:bool =
  0x10:u8 ==
  (0x10:u8 & (mem:?u32[R_EAX:u32, e_little]:u8 ^ T_t1_92:u8 ^ T_t2_93:u8))
R_OF:bool =
  high:bool((T_t1_92:u8 ^ ~T_t2_93:u8) &
            (T_t1_92:u8 ^ mem:?u32[R_EAX:u32, e_little]:u8))
R_PF:bool =
  ~low:bool(mem:?u32[R_EAX:u32, e_little]:u8 >> 7:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 6:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 5:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 4:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 3:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 2:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8 >> 1:u8 ^
            mem:?u32[R_EAX:u32, e_little]:u8)
R_SF:bool = high:bool(mem:?u32[R_EAX:u32, e_little]:u8)
R_ZF:bool = 0:u8 == mem:?u32[R_EAX:u32, e_little]:u8


