/*                              LC-3 architecture		*/


/* Includes section {{{ */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <signal.h>

#ifdef __unix__
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/termios.h>
#include <sys/mman.h>
#endif

#ifdef _WIN32
#include <Windows.h>
#include <conio.h>      /* _kbhit */
#endif

/* }}} */


/* Defines section {{{ */
#define MEMORY_MAX      0x10000         /* or (1 << 16) - program max memory - 64KB address space, 65,536 bytes */
#define PC_START        0x3000          /* program counter start address */
/* }}} */


/* Enums section {{{ */
typedef enum {
        R_R0 = 0,       /* <----##########################	*/
        R_R1,           /* <----##	                ##	*/
        R_R2,           /* <----##       general        ##	*/
        R_R3,           /* <----##       purpose        ##	*/
        R_R4,           /* <----##       registers      ##	*/
        R_R5,           /* <----##                      ##	*/
        R_R6,           /* <----##                      ##	*/
        R_R7,           /* <----##########################	*/
        R_PC,           /*	program counter register	*/
        R_COND,         /*	condition flags register	*/
        R_COUNT
} Registers;


typedef enum {
        FL_POS = 0x0001,      /* or (1 << 0) - bit 0 - Positive */
        FL_ZRO = 0x0002,      /* or (1 << 1) - bit 1 - Zero     */
        FL_NEG = 0x0004       /* or (1 << 2) - bit 2 - Negative */
} ConditionFlags;


typedef enum {
        OP_BR = 0,      /*      Branch				*/
        OP_ADD,         /*      Add                             */
        OP_LD,          /*      Load                            */
        OP_ST,          /*      Store                           */
        OP_JSR,         /*      Jump to Subroutine              */
        OP_AND,         /*      Bitwise AND                     */
        OP_LDR,         /*      Load Register                   */
        OP_STR,         /*      Store Register                  */
        OP_RTI,         /*      Return from Interrupt (unused)  */
        OP_NOT,         /*      Bitwise NOT                     */
        OP_LDI,         /*      Load Indirect                   */
        OP_STI,         /*      Store Indirect                  */
        OP_JMP,         /*      Jump (also handles RET)         */
        OP_RES,         /*      Reserved (unused)               */
        OP_LEA,         /*      Load Effective Address          */
        OP_TRAP         /*      Execute Trap                    */
} Opcodes;


/*
 * Special registers that are not accessible from the normal register table,
 * but a special address is reserved for them im memory.
 */
typedef enum {
        MR_KBSR = 0xFE00,       /* Keyboard Status register */
        MR_KBDR = 0xFE02        /* Keyboard Data register   */
} MemoryMappedRegisters;


/* Trap routines for performing common tasks and interacting with I/O devices */
typedef enum {
        TRAP_GETC       = 0x20,         /* Get character from keyboard, not echoed onto the terminal */
        TRAP_OUT        = 0x21,         /* Output a character                                        */
        TRAP_PUTS       = 0x22,         /* Output a word string                                      */
        TRAP_IN         = 0x23,         /* Get character from keyboard, echoed onto the terminal     */
        TRAP_PUTSP      = 0x24,         /* Output a byte string                                      */
        TRAP_HALT       = 0x25          /* Halt the program                                          */
} Trapcodes;
/* }}} */


/* Global variables {{{ */
uint16_t memory[MEMORY_MAX];    /*      65536 locations      */
uint16_t register_storage[R_COUNT];
/* }}} */


/* Input Buffering section {{{ */
/* UNIX <<< */
#ifdef __unix__
struct termios original_tio;

void disable_input_buffering(void) {
        struct termios new_tio;
       	new_tio = original_tio;

        tcgetattr(STDIN_FILENO, &original_tio);
        new_tio.c_lflag &= ~ICANON & ~ECHO;     /* Disable terminal canonical mode and input echoing */
        tcsetattr(STDIN_FILENO, TCSANOW, &new_tio);
}

void restore_input_buffering(void) {
        tcsetattr(STDIN_FILENO, TCSANOW, &original_tio);
}

uint16_t check_key(void) {
        struct timeval timeout;

        fd_set readfds;
        FD_ZERO(&readfds);
        FD_SET(STDIN_FILENO, &readfds);
   
        timeout.tv_sec = 0;
        timeout.tv_usec = 0;

        return select(1, &readfds, NULL, NULL, &timeout) != 0;
}
#endif
/* >>> */

/* Windows <<< */
#ifdef _WIN32
HANDLE hStdin = INVALID_HANDLE_VALUE;
DWORD fdwMode, fdwOldMode;

void disable_input_buffering() {
        hStdin = GetStdHandle(STD_INPUT_HANDLE);
        GetConsoleMode(hStdin, &fdwOldMode);    /* Save old mode                                    */
        fdwMode = fdwOldMode
                ^ ENABLE_ECHO_INPUT             /* No input echo                                    */
                ^ ENABLE_LINE_INPUT;            /* Return when one or more characters are available */

        SetConsoleMode(hStdin, fdwMode);        /* Set new mode                                     */
        FlushConsoleInputBuffer(hStdin);        /* Clear buffer                                     */
}

void restore_input_buffering() {
        SetConsoleMode(hStdin, fdwOldMode);
}

uint16_t check_key() {
        return WaitForSingleObject(hStdin, 1000) == WAIT_OBJECT_0 && _kbhit();
}
#endif
/* >>> */
/* }}} */


/* Functions section {{{ */
/* Swap uint16 to big endian from little endian */
void handle_interrupt(int signal) {
        restore_input_buffering();
        printf("%d signal received\n", signal);
        exit(-2);       /* exit code fail */
}


uint16_t swap_uint16(uint16_t x) {
        return (x << 8) | (x >> 8);
}


void read_image_file(FILE *file) {
        uint16_t origin;
        uint16_t max_read;
        uint16_t *ptr;
        size_t read;

        /* Variables for endianess detection */
        uint16_t test;
        uint8_t *byte;
        int is_big_endian;

        /*
         * LC-3 programs are big-endian, but most modern computers are little-endian,
         * therefore each uint16 that is loaded needs to be swapped
         */

        /* Endianess detection */
        test = 0x0102;
        byte = (uint8_t*)&test;
        is_big_endian = (byte[0] == 0x01);      /* True if big-endian */
        /*
         * On Big-Endian systems:
         * Memory: [0x01] [0x02]
         * byte[0] = 0x01, byte[1] = 0x02
         * byte[0] == 0x01 -> TRUE -> is_big_endian = 1
         *
         * On Little-Endian systems:
         * Memory: [0x02] [0x01]
         * byte[0] == 0x02, byte[1] == 0x01
         * byte[0] == 0x01 -> FALSE -> is_big_endian = 0
         */

        /* The origin points in memory where the image to be stored */
        fread(&origin, sizeof(origin), 1, file);
        if(!is_big_endian) {
                origin = swap_uint16(origin);
        }

        /*
         * The maximum file size is known (MEMORY_MAX = 1 << 16 or MEMORY_MAX = 65536 (0x10000))
         * therefore only one "fread" is needed
         */
        max_read = (uint16_t)(MEMORY_MAX - origin);
        ptr = memory + origin;
        read = fread(ptr, sizeof(uint16_t), max_read, file);

        if(!is_big_endian) {
                /* swap to little endian */
                while(read-- > 0) {
                        *ptr = swap_uint16(*ptr);
                        ++ptr;
                }
        }
}


int read_image(const char *image_path) {
	FILE *file;
        /* Open the BINARY file in READ mode. If the file does not exist, the open() function returns NULL. */
        file = fopen(image_path, "rb");
	
        if(!file) {
                return 0;
        }

        read_image_file(file);
        fclose(file);
        return 1;
}


uint16_t sign_extend(uint16_t x, int bit_count) {
        if((x >> (bit_count - 1)) & 1) {
                x |= (0xFFFF << bit_count);
        }

        return x;
}


void update_flags(uint16_t reg) {
        if(register_storage[reg] == 0) {
                register_storage[R_COND] = FL_ZRO;
        }
        
        /* an 1 in the left-most bit indicates negative */
        else if (register_storage[reg] >> 15) {
                register_storage[R_COND] = FL_NEG;
        }

        else {
                register_storage[R_COND] = FL_POS;
        }
}


void mem_write(uint16_t address, uint16_t val) {
	/* address will never be more than MEMORY_MAX in memory[MEMORY_MAX] */
        memory[address] = val;
}


uint16_t mem_read(uint16_t address) {
        /* Check if reading keyboard status */
        if(address == MR_KBSR) {
                /* Check if key is pressed */
                if(check_key()) {
                        /* Set most significant bit (bit 15) of MR_KBSR to 1 - "ready" flag */
                        memory[MR_KBSR] = (1 << 15);
                        /* Read the keypress into MR_KBDR */
                        memory[MR_KBDR] = getchar();
                }

                else {
                        /* Clear KBSR to 0 - "ready" flag = 0 */
                        memory[MR_KBSR] = 0;
                }
        }

        return memory[address];
}
/* }}} */


/* Main section {{{ */
int main(int argc, const char* argv[]) {
        int running;
        uint16_t instruction;
        uint16_t op_code;
        int read_image_index;

        /* Load arguments {{{ */
        if(argc < 2) {
                /* show usage string */
                fprintf(stderr, "lc3 [image-file1] ...\n");
                exit(2);
        }
        
        for(read_image_index = 1; read_image_index < argc; ++read_image_index) {
                if(!read_image(argv[read_image_index])) {
                        fprintf(stderr, "Failed to load image: %s\n", argv[read_image_index]);
                        exit(1);
                }
        }
        /* }}} */


        /* Setup {{{ */
        signal(SIGINT, handle_interrupt);
        disable_input_buffering(); /* IMPORTANT: before this line exit() can be used, after it use handle_interrupt() */
        /* }}} */

        /* Since exactly one condition flag should be set at any given time, set the ZRO flag */
        register_storage[R_COND] = FL_ZRO;

        /*
         * Set the program counter (PC) to starting position
         * 0x3000 is the default starting position in memory
         *
         * The reason why programs start at address 0x3000 instead 0x0
         * is because the lower addresses are left empty for space for the trap routine code
         */
        register_storage[R_PC] = PC_START;

        running = 1;
        while(running) {
                /* FETCH */
                instruction = mem_read(register_storage[R_PC]++);
                op_code = instruction >> 12;

                switch(op_code) {
                        case OP_ADD: {
                                /*
                                 * Register mode (Mode bit 0):
                                 *
                                 *  15       12 11    9  8     6  5  4  3  2      0
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--|--+--+--|
                                 * |   0001    |   DR   |  SR1   |0 | 00  |  SR2   |
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--|--+--+--|
                                 *
                                 *  Immediate mode (Mode bit 1):
                                 *
                                 *  15       12 11    9  8     6  5  4            0
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--+--+--+--|
                                 * |   0001    |   DR   |  SR1   |1 |     imm5     |
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--+--+--+--|
                                 */
                                
                                uint16_t destination, source1, source2, immediate_flag, immediate_value5;

                                /* destination register (DR) */
                                destination = (instruction >> 9) & 0x7;
                                /* first operand (source register SR1) */
                                source1 = (instruction >> 6) & 0x7;
                                /* whether we are in immediate mode */
                                immediate_flag = (instruction >> 5) & 0x1;
                                
                                if(immediate_flag) {
                                        immediate_value5 = sign_extend(instruction & 0x1F, 5);
                                        register_storage[destination] = register_storage[source1] + immediate_value5;
                                }

                                else {
                                        source2 = instruction & 0x7;
                                        register_storage[destination] = register_storage[source1] + register_storage[source2];
                                }
                                
                                update_flags(destination);
                                break;
                        }


                        case OP_AND: {
                                /*
                                 * Register mode (Mode bit 0):
                                 *
                                 *  15       12 11    9  8     6  5  4  3  2      0
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--|--+--+--|
                                 * |   0101    |   DR   |  SR1   |0 | 00  |  SR2   |
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--|--+--+--|
                                 *
                                 *  Immediate mode (Mode bit 1):
                                 *
                                 *  15       12 11    9  8     6  5  4            0
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--+--+--+--|
                                 * |   0101    |   DR   |  SR1   |1 |     imm5     |
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--+--+--+--|
                                 */

                                uint16_t destination, source1, source2, immediate_flag, immediate_value5;

                                destination = (instruction >> 9) & 0x7;
                                source1 = (instruction >> 6) & 0x7;
                                immediate_flag = (instruction >> 5) & 0x1;

                                if(immediate_flag) {
                                        immediate_value5 = sign_extend(instruction & 0x1F, 5);
                                        register_storage[destination] = register_storage[source1] & immediate_value5;
                                }

                                else {
                                        source2 = instruction & 0x7;
                                        register_storage[destination] = register_storage[source1] & register_storage[source2];
                                }

                                update_flags(destination);
                                break;
                        }


                        case OP_NOT: {
                                /*
                                 *  15       12 11    9  8     6  5  4            0
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--+--+--+--|
                                 * |   1001    |   DR   |  SR    |1 |     11111    |
                                 * |--+--+--+--|--+--+--|--+--+--|--|--+--+--+--+--|
                                 */

                                uint16_t destination, source;

                                destination = (instruction >> 9) & 0x7;
                                source = (instruction >> 6) & 0x7;

                                register_storage[destination] = ~register_storage[source];
                                update_flags(destination);
                                break;
                        }


                        case OP_BR: {
                                /*
                                 *  15       12 11 10 9  8                        0
                                 * |--+--+--+--|--|--|--|--+--+--+--+--+--+--+--+--|
                                 * |   0000    |n |z |p |         PCoffset9        |
                                 * |--+--+--+--|--|--|--|--+--+--+--+--+--+--+--+--|
                                 *              ^flags^
                                 */

                                uint16_t pc_offset, condition_flag;

                                pc_offset = sign_extend(instruction & 0x1FF, 9);
                                condition_flag = (instruction >> 9) & 0x7;

                                if(condition_flag & register_storage[R_COND]) {
                                        register_storage[R_PC] += pc_offset;
                                }
                                break;
                        }


                        case OP_JMP: {
                                /* Also handles RET */

                                /*
                                 * JMP:
                                 *
                                 *  15       12 11    9  8     6  5               0
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 * |   1100    |  000   | BaseR  |      000000     |
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 *
                                 * RET:
                                 *
                                 *  15       12 11    9  8     6  5               0
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 * |   1100    |   000  |  111   |      000000     |
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 */

                                uint16_t base;
                                
                                base = (instruction >> 6) & 0x7;
                                register_storage[R_PC] = register_storage[base];
                                break;
                        }


                        case OP_JSR: {
                                /*
                                 * JSR:
                                 *
                                 *  15       12 11 10                             0
                                 * |--+--+--+--|--|--+--+--+--+--+--+--+--+--+--+--|
                                 * |   0100    | 1|           PCoffset11           |
                                 * |--+--+--+--|--|--+--+--+--+--+--+--+--+--+--+--|
                                 *
                                 * JSRR:
                                 *
                                 *  15       12 11 10  9  8     6  5               0
                                 * |--+--+--+--|--|--+--|--+--+--|--+--+--+--+--+--|
                                 * |   0100    | 0| 00  |  BaseR |      000000     |
                                 * |--+--+--+--|--|--+--|--+--+--|--+--+--+--+--+--|
                                 */

                                uint16_t base, long_flag, pc_offset;
                                
                                long_flag = (instruction >> 11) & 1;
                                register_storage[R_R7] = register_storage[R_PC];

                                if(long_flag) {
                                        pc_offset = sign_extend(instruction & 0x7FF, 11);
                                        register_storage[R_PC] += pc_offset;       /* JSR */
                                }

                                else {
                                        base = (instruction >> 6) & 0x7;
                                        register_storage[R_PC] = register_storage[base];  /* JSRR */
                                }
                                break;
                        }


                        case OP_LD: {
                                /*
                                 *  15       12 11    9  8                        0
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 * |   0010    |   DR   |         PCoffset9        |
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 */

                                uint16_t destination, pc_offset;

                                destination = (instruction >> 9) & 0x7;
                                pc_offset = sign_extend(instruction & 0x1FF, 9);

                                register_storage[destination] = mem_read(register_storage[R_PC] + pc_offset);
                                update_flags(destination);
                                break;
                        }


                        case OP_LDI: {
                                /*
                                 *  15       12 11    9  8                        0
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 * |   1010    |   DR   |         PCoffset9        |
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 */

                                uint16_t destination, pc_offset;

                                /* Destination register */
                                destination = (instruction >> 9) & 0x7;
                                /* Program Counter offset 9 */
                                pc_offset = sign_extend(instruction & 0x1FF, 9);
                                /* 
                                 * add pc_offset to the current PC,
                                 * look at that memory location to get the final address
                                 */
                                register_storage[destination] = mem_read(mem_read(register_storage[R_PC] + pc_offset));
                                update_flags(destination);
                                break;
                        }


                        case OP_LDR: {
                                /*
                                 *  15       12 11    9  8     6  5               0
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 * |   0110    |   DR   |  BaseR |     offset6     |
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 */

                                uint16_t destination, base, offset;

                                destination = (instruction >> 9) & 0x7;
                                base = (instruction >> 6) & 0x7;
                                offset = sign_extend(instruction & 0x3F, 6);
                                register_storage[destination] = mem_read(register_storage[base] + offset);
                                update_flags(destination);
                                break;
                        }


                        case OP_LEA: {
                                /*
                                 *  15       12 11    9  8                        0
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 * |   1110    |   DR   |         PCoffset6        |
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 */

                                uint16_t destination, pc_offset;

                                destination = (instruction >> 9) & 0x7;
                                pc_offset = sign_extend(instruction & 0x1FF, 9);
                                register_storage[destination] = register_storage[R_PC] + pc_offset;
                                update_flags(destination);
                                break;
                        }


                        case OP_ST: {
                                /*
                                 *  15       12 11    9  8                        0
                                 * |--+--+--+--|--+--+--+--+--+--+--+--+--+--+--+--|
                                 * |   0011    |   SR   |         PCoffset9        |
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 */

                                uint16_t source, pc_offset;

                                source = (instruction >> 9) & 0x7;
                                pc_offset = sign_extend(instruction & 0x1FF, 9);
                                mem_write(register_storage[R_PC] + pc_offset, register_storage[source]);
                                break;
                        }


                        case OP_STI: {
                                /*
                                 *  15       12 11    9  8                        0
                                 * |--+--+--+--|--+--+--+--+--+--+--+--+--+--+--+--|
                                 * |   1011    |   SR   |         PCoffset9        |
                                 * |--+--+--+--|--+--+--|--+--+--+--+--+--+--+--+--|
                                 */

                                uint16_t source, pc_offset;

                                source = (instruction >> 9) & 0x7;
                                pc_offset = sign_extend(instruction & 0x1FF, 9);
                                mem_write(mem_read(register_storage[R_PC] + pc_offset), register_storage[source]);
                                break;
                        }


                        case OP_STR: {
                                /*
                                 *  15       12 11    9  8     6                  0
                                 * |--+--+--+--|--+--+--+--+--+--|--+--+--+--+--+--|
                                 * |   0111    |   SR   |  BaseR |     offset6     |
                                 * |--+--+--+--|--+--+--|--+--+--|--+--+--+--+--+--|
                                 */

                                uint16_t source, base, offset;

                                source = (instruction >> 9) & 0x7;
                                base = (instruction >> 6) & 0x7;
                                offset = sign_extend(instruction & 0x3F, 6);
                                
                                mem_write(register_storage[base] + offset, register_storage[source]);
                                break;
                        }

                                
                        case OP_TRAP: {
                                /*
                                 *  15       12 11       8  7                     0
                                 * |--+--+--+--|--+--+--+--|--+--+--+--+--+--+--+--|
                                 * |   1111    |    0000   |      trapvect8        |
                                 * |--+--+--+--|--+--+--+--|--+--+--+--+--+--+--+--|
                                 */

                                register_storage[R_R7] = register_storage[R_PC];

                                switch(instruction & 0xFF) {
                                        case TRAP_GETC: {
                                                /* Read single ASCII char */
                                                register_storage[R_R0] = (uint16_t)getchar();
                                                update_flags(R_R0);
                                                break;
                                        }

                                        case TRAP_OUT: {
                                                putc((char)register_storage[R_R0], stdout);
                                                fflush(stdout);
                                                break;
                                        }

                                        case TRAP_PUTS: {
                                                /* Output one char per word */
                                                uint16_t *character;
                                                character = memory + register_storage[R_R0];
                                                
                                                while(*character) {
                                                        putc((char)*character, stdout);
                                                        ++character;
                                                }

                                                fflush(stdout);
                                                break;
                                        }

                                        case TRAP_IN: {
                                                char character;
                                                character = getchar();

                                                printf("Enter a character: ");
                                                putc(character, stdout);

                                                fflush(stdout);
                                                register_storage[R_R0] = (uint16_t)character;
                                                break;
                                        }

                                        case TRAP_PUTSP: {
                                                /*
                                                 * one char per byte (two bytes per word)
                                                 * here we need to swap back to big endian format
                                                 */
                                                uint16_t *character;
                                                character = memory + register_storage[R_R0];
                                                
                                                while(*character) {
                                                        char char1, char2;

                                                        char1 = (*character) & 0xFF;
                                                        putc(char1, stdout);

                                                        char2 = (*character) >> 8;
                                                        if(char2) {
                                                                putc(char2, stdout);
                                                        }
                                                        ++character;

                                                }
                                                

                                                fflush(stdout);
                                                break;
                                        }

                                        case TRAP_HALT: {
                                                puts("HALT");
                                                fflush(stdout);
                                                running = 0;
                                                break;
                                        }
                                }
                                break;
                        }


                        case OP_RTI:
                        case OP_RES:
                        default: {
                                /* Unused and unimplemented opcodes*/
                                abort();
                                break;
                        }
                }
        }

        restore_input_buffering();
        return 0;
}
/* }}} */
