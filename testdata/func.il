label pc_0x8048464
R_ESP_84:u32 = R_ESP_1:u32 - 4:u32
mem_85:?u32 = mem_56:?u32 with [R_ESP_84:u32, e_little]:u32 = R_EBP_0:u32
R_EAX_91:u32 = 3:u32
temp_92:u32 = mem_85:?u32[R_ESP_84 + 4:u32, e_little]:u32
R_ESP_87:u32 = R_ESP_84 + 4:u32
jmp temp_92:u32
