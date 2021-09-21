#include <stdio.h> // for stderr
#include <stdlib.h> // for exit()
#include "types.h"
#include "utils.h"
#include "riscv.h"

void execute_rtype(Instruction, Processor *);
void execute_itype_except_load(Instruction, Processor *);
void execute_branch(Instruction, Processor *);
void execute_jal(Instruction, Processor *);
void execute_load(Instruction, Processor *, Byte *);
void execute_store(Instruction, Processor *, Byte *);
void execute_ecall(Processor *, Byte *);
void execute_lui(Instruction, Processor *);

void execute_instruction(uint32_t instruction_bits, Processor *processor,Byte *memory) {    
    Instruction instruction = parse_instruction(instruction_bits);
    switch(instruction.opcode) {
        case 0x0b:
        case 0x33:
            execute_rtype(instruction, processor);
            break;
        case 0x13:
            execute_itype_except_load(instruction, processor);
            break;
        case 0x73:
            execute_ecall(processor, memory);
            break;
        case 0x63:
            execute_branch(instruction, processor);
            break;
        case 0x6F:
            execute_jal(instruction, processor);
            break;
        case 0x23:
            execute_store(instruction, processor, memory);
            break;
        case 0x03:
            execute_load(instruction, processor, memory);
            break;
        case 0x37:
            execute_lui(instruction, processor);
            break;
        default: // undefined opcode
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
}

void execute_rtype(Instruction instruction, Processor *processor) {
    switch (instruction.rtype.funct3){
        case 0x0:
            switch (instruction.rtype.funct7) {
                case 0x0:
                // Mac
                if (instruction.rtype.opcode == 0x0b) {
                  processor->R[instruction.rtype.rd] +=
                      ((sWord)processor->R[instruction.rtype.rs1]) *
                      ((sWord)processor->R[instruction.rtype.rs2]);
                      break;
    }
                  // Add
                  processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) +
                      ((sWord)processor->R[instruction.rtype.rs2]);
                  break;
                case 0x1:
                  // Mul
                  processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) *
                      ((sWord)processor->R[instruction.rtype.rs2]);
                  break;
                case 0x20:
                    // Sub
                    processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) -
                      ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x1:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // SLL
                    processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) <<
                      ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x1:
                    // MULH
                    processor->R[instruction.rtype.rd] =
                      (((sDouble)processor->R[instruction.rtype.rs1]) *
                      ((sDouble)processor->R[instruction.rtype.rs2])) >> 32;
                    break;
            }
            break;
        case 0x2:
            // SLT
            processor->R[instruction.rtype.rd] = (((sWord) processor->R[instruction.rtype.rs1])
            < ((sWord) processor->R[instruction.rtype.rs2]))? 1 : 0;
            break;
        case 0x4:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // XOR
                    processor->R[instruction.rtype.rd] =
                    ((sWord)processor->R[instruction.rtype.rs1]) ^
                    ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x1:
                    // DIV
                    processor->R[instruction.rtype.rd] =
                    ((sWord)processor->R[instruction.rtype.rs1]) /
                    ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x5:
            switch (instruction.rtype.funct7) {
                case 0x0:
                // SRL  
                processor->R[instruction.rtype.rd] =
                (processor->R[instruction.rtype.rs1]) >>
                (processor->R[instruction.rtype.rs2]);    
                    break;
                case 0x20:
                    // SRA
                processor->R[instruction.rtype.rd] =
                ((sWord)processor->R[instruction.rtype.rs1]) >>
                ((sWord)processor->R[instruction.rtype.rs2]);  
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                break;
            }
            break;
        case 0x6:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // OR
                    processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) |
                      ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x1:
                    // REM
                    processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) %
                      ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x7:
            // AND
            processor->R[instruction.rtype.rd] =
                ((sWord)processor->R[instruction.rtype.rs1]) &
                ((sWord)processor->R[instruction.rtype.rs2]);
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    processor->PC += 4;
}

void execute_itype_except_load(Instruction instruction, Processor *processor) {
    switch (instruction.itype.funct3) {
        case 0x0:
            // ADDI
            processor->R[instruction.itype.rd] =
                ((sWord)processor->R[instruction.itype.rs1]) +
                sign_extend_number(instruction.itype.imm, 12);
            break;
        case 0x1:
            // SLLI
            processor->R[instruction.itype.rd] =
                ((sWord)processor->R[instruction.itype.rs1]) <<
                sign_extend_number(instruction.itype.imm, 12);
            break;
        case 0x2:
            // STLI
            processor->R[instruction.itype.rd] = 
                (((sWord) processor->R[instruction.itype.rs1]) <
                sign_extend_number(instruction.itype.imm, 12))? 1 : 0;
            break;
        case 0x4:
            // XORI
            processor->R[instruction.itype.rd] =
                ((sWord)processor->R[instruction.itype.rs1]) ^
                sign_extend_number(instruction.itype.imm, 12);
            break;
        case 0x5:
            // SRLI
            processor->R[instruction.itype.rd] =
                ((sWord)processor->R[instruction.itype.rs1]) >>
                sign_extend_number(instruction.itype.imm, 12);
                break;
        case 0x6:
            // ORI
            processor->R[instruction.itype.rd] =
                ((sWord)processor->R[instruction.itype.rs1]) |
                sign_extend_number(instruction.itype.imm, 12);
            break;
        case 0x7:
            // ANDI
            processor->R[instruction.itype.rd] =
                ((sWord)processor->R[instruction.itype.rs1]) &
                (sign_extend_number(instruction.itype.imm, 12));
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }
    processor->PC += 4;
}

void execute_ecall(Processor *p, Byte *memory) {
    Register i;
    
    // syscall number is given by a0 (x10)
    // argument is given by a1
    switch(p->R[10]) {
        case 1: // print an integer
            printf("%d",p->R[11]);
            break;
        case 4: // print a string
            for(i=p->R[11];i<MEMORY_SPACE && load(memory,i,LENGTH_BYTE);i++) {
                printf("%c",load(memory,i,LENGTH_BYTE));
            }
            break;
        case 10: // exit
            printf("exiting the simulator\n");
            exit(0);
            break;
        case 11: // print a character
            printf("%c",p->R[11]);
            break;
        default: // undefined ecall
            printf("Illegal ecall number %d\n", p->R[10]);
            exit(-1);
            break;
    }
    p->PC += 4;
}

void execute_branch(Instruction instruction, Processor *processor) {
    switch (instruction.sbtype.funct3) {
        case 0x0:
            // BEQ
            if (((sHalf)processor->R[instruction.sbtype.rs1]) == ((sHalf)processor->R[instruction.sbtype.rs2]))
            {
                processor->PC += (sHalf) get_branch_offset(instruction);
            }else{
                processor->PC += 4;
            }  
            break;
        case 0x1:
            // BNE
            if (((sHalf)processor->R[instruction.sbtype.rs1]) != ((sHalf)processor->R[instruction.sbtype.rs2]))
            {
                processor->PC += (sHalf) get_branch_offset(instruction);
            }else{
                processor->PC += 4;
            }  
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
}

void execute_load(Instruction instruction, Processor *processor, Byte *memory) {
        Address address = ((sWord) processor->R[instruction.itype.rs1]) + ((sWord) sign_extend_number(instruction.itype.imm, 12));
        Alignment alignment;
    switch (instruction.itype.funct3) {
        case 0x0:
            // LB
        alignment = LENGTH_BYTE;
        processor->R[instruction.itype.rd] = sign_extend_number((load(memory, address, alignment) & 0xf),4);
            break;
        case 0x1:
            // LH
        alignment = LENGTH_HALF_WORD;
        processor->R[instruction.itype.rd] = sign_extend_number((load(memory, address, alignment) & 0xfff),8);
            break;
        case 0x2:
            // LW
        alignment = LENGTH_WORD;
        processor->R[instruction.itype.rd] = (sWord) load(memory, address, alignment);
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }
    processor->PC += 4;
}

void execute_store(Instruction instruction, Processor *processor, Byte *memory) {
       int imm = get_store_offset(instruction);
       Address address = processor->R[instruction.stype.rs1] + imm;
       Word value = (Word) processor->R[instruction.stype.rs2];
       Alignment alignment;
    switch (instruction.stype.funct3) {
        case 0x0:
            // SB
        alignment = LENGTH_BYTE;
            break;
        case 0x1:
            // SH
            alignment = LENGTH_HALF_WORD;
            break;
        case 0x2:
            // SW
            alignment = LENGTH_WORD;
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    store(memory, address, alignment, value);
    processor->PC += 4;
}

void execute_jal(Instruction instruction, Processor *processor) {
    /* YOUR CODE HERE */
    processor->R[instruction.ujtype.rd] = ((processor->PC) + 4);
    processor->PC += get_jump_offset(instruction);
}

void execute_lui(Instruction instruction, Processor *processor) {
    /* YOUR CODE HERE */
    processor->R[instruction.utype.rd] = instruction.utype.imm << 12;
    processor->PC += 4;
}

void store(Byte *memory, Address address, Alignment alignment, Word value) {
    /* YOUR CODE HERE */
    switch (alignment) {
        case LENGTH_BYTE:
            // SB
        memory[address] = (Byte) (value & ((1U << 7) - 1));
            break;
        case LENGTH_HALF_WORD:
            // SH
        memory[address] = (Byte) (value & ((1U << 7) - 1));
        memory[address + 1] = (Byte) (value & ((1U << 15) - 256));
            break;
        case LENGTH_WORD:
            // SW
        memory[address] = (Byte) (value & ((1U << 7) - 1));
        memory[address + 1] = (Byte) (value & ((1U << 15) - 0x100));
        memory[address + 2] = (Byte) (value & ((1U << 23) - 0x10000));
        memory[address + 3] = (Byte) (value & ((1U << 31) - 0x100000000));
            break;
    }
}

Word load(Byte *memory, Address address, Alignment alignment) {
    /* YOUR CODE HERE */
        switch (alignment) {
        case LENGTH_BYTE:
            // LB
            return memory[address];
            break;
        case LENGTH_HALF_WORD:
            // LH
            {
            return memory[address] | (memory[address + 1] << 8);
            }
            break;
        case LENGTH_WORD:
            // LW
           { 
        return (memory[address] | (memory[address + 1] << 8) | (memory[address + 2] << 16) | (memory[address + 3] << 24));
            }
            break;
    }
    return 0;
}
