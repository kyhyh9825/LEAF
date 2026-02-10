/*
 * ======================================================================================
 * LEAF CPU Assembler (v5.0 Updated ISA)
 * ======================================================================================
 * [Features]
 * 1. Label Support: Jump via 'LOOP:' labels (2-Pass)
 * 2. Direct Streaming: zlib based schematic generation
 * 3. Updated ISA: Bit-field based opcode mapping (v5.0 spec)
 * ======================================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <math.h>
#include <zlib.h>

// --- Settings ---
#define MAX_PAGES_LIMIT 1024
#define INSTRUCTIONS_PER_PAGE 64
#define MAX_CODE_SIZE (MAX_PAGES_LIMIT * INSTRUCTIONS_PER_PAGE)
#define DEFAULT_INPUT_FILE "input.asm"
#define DEFAULT_OUTPUT_FILE "rom_output.schem"
#define MC_DATA_VERSION 2586

// --- Label Settings ---
#define MAX_LABELS 1024
#define MAX_LABEL_LEN 64

typedef uint32_t instruction_t;

const int ITEMS_PER_SIGNAL[16] =
{
    0, 1, 124, 247, 371, 494, 618, 741, 865, 988, 1111, 1235, 1358, 1482, 1605, 1728
};

typedef struct
{
    int x, y, z;
} Coord;

// Label Structure
typedef struct
{
    char name[MAX_LABEL_LEN];
    int address;
} Label;

Label label_table[MAX_LABELS];
int label_count = 0;

// --- Assembler Utils ---
char* trim_whitespace(char* str)
{
    char* e;
    while (isspace((unsigned char)*str)) str++;
    if (*str == 0) return str;
    e = str + strlen(str) - 1;
    while (e > str && isspace((unsigned char)*e)) e--;
    *(e + 1) = 0;
    return str;
}

int parse_reg(const char* s)
{
    if (!s) return -1;

    // SP (Stack Pointer)
    if (!strcasecmp(s, "SP")) return 15;

    // R0~R15
    if (toupper(s[0]) == 'R')
    {
        char* endptr;
        long val = strtol(s + 1, &endptr, 10);

        if (*endptr != '\0') return -1;
        if (val < 0 || val > 15) return -1;

        return (int)val;
    }

    return -1;
}

int get_label_address(const char* name)
{
    for (int i = 0; i < label_count; i++)
    {
        if (!strcmp(label_table[i].name, name))
        {
            return label_table[i].address;
        }
    }
    return -1;
}

int parse_arg(const char* s)
{
    if (!s) return 0;

    char* endptr;
    long val = strtol(s, &endptr, 0);

    // Number
    if (*s != '\0' && *endptr == '\0')
    {
        return (int)val;
    }

    // Label
    int addr = get_label_address(s);
    if (addr != -1) return addr;

    return 0;
}

// --- Assembler Core (Updated for v5.0 ISA) ---
instruction_t assemble_line(char* line, int line_num)
{
    static int last_heavy_op = 0;

    // Helper Macro for Register Parsing
#define GET_REG(dest, str) do { \
        dest = parse_reg(str); \
        if (dest == -1) { \
            printf("\033[0;31m[Error] Line %d: Invalid Register '%s'\n\033[0m", line_num, str); \
            return 0xFFFFFFFF; \
        } \
    } while(0)

    char m[32], a[3][32] = {0};
    int ac = 0;
    char line_buf[256];
    strncpy(line_buf, line, 256);
    line_buf[255] = 0;

    // Remove comments and trim
    char* c = strchr(line_buf, ';');
    if (c) *c = 0;
    char* trimmed = trim_whitespace(line_buf);
    if (strlen(trimmed) == 0) return 0;

    // Tokenize
    char* t = strtok(trimmed, " ,");
    if (!t) return 0;
    strcpy(m, t);
    while ((t = strtok(NULL, " ,")) && ac < 3) strcpy(a[ac++], t);

    // Parse Subtype (e.g., .MUL)
    char* dot = strchr(m, '.');
    char sub_type[32] = {0};
    if (dot)
    {
        *dot = '\0';
        strcpy(sub_type, dot + 1);
    }

    if (m[strlen(m) - 1] == ':') return 0; // Skip Labels

    // --- Warnings ---
    // 1. Redundant MOV
    if (!strcmp(m, "MOV") && !strcmp(a[0], a[1]))
    {
        printf("\033[0;33m[Warning] Line %d: Redundant 'MOV %s, %s' (NOP)\n\033[0m", line_num, a[0], a[1]);
    }
    // 2. Redundant ADDI/SUBI 0
    else if ((!strcmp(m, "ADDI") || !strcmp(m, "SUBI")) && !strcmp(a[0], a[1]))
    {
        if (parse_arg(a[2]) == 0)
        {
            printf("\033[0;33m[Warning] Line %d: Redundant '%s %s, 0'\n\033[0m", line_num, m, a[0]);
        }
    }
    // 3. Heavy Op Stall Warning
    if (!strcmp(m, "MFHI") || !strcmp(m, "MFLO"))
    {
        int warning_trigger = 0;
        if (last_heavy_op == 1 && !strcmp(sub_type, "MUL")) warning_trigger = 1;
        else if (last_heavy_op == 2 && !strcmp(sub_type, "DIV")) warning_trigger = 1;
        else if (last_heavy_op == 3 && !strcmp(sub_type, "SQRT")) warning_trigger = 1;

        if (warning_trigger)
        {
            printf("\033[0;33m[Warning] Line %d: '%s.%s' follows heavy op. STALL induced.\n\033[0m", line_num, m, sub_type);
        }
    }

    // Update Heavy Op State
    if (!strcmp(m, "MUL")) last_heavy_op = 1;
    else if (!strcmp(m, "DIV")) last_heavy_op = 2;
    else if (!strcmp(m, "SQRT")) last_heavy_op = 3;
    else last_heavy_op = 0;

    // --- Instruction Assembly ---

    // 1. NOP / HLT
    if (!strcmp(m, "NOP")) return 0;
    if (!strcmp(m, "HLT")) return 0x0F000000; // Op: 15 (001111)

    // 2. Pseudo-ops (Recursive)
    if (!strcmp(m, "MOV")) { char buf[64]; snprintf(buf, 64, "ADD %s, %s, R0", a[0], a[1]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "NEG")) { char buf[64]; snprintf(buf, 64, "SUB %s, R0, %s", a[0], a[1]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "NOT")) { char buf[64]; snprintf(buf, 64, "NOR %s, %s, R0", a[0], a[1]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "LODI")) { char buf[64]; snprintf(buf, 64, "ADDI %s, R0, %s", a[0], a[1]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "CMP")) { char buf[64]; snprintf(buf, 64, "SUB R0, %s, %s", a[0], a[1]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "CLR")) { char buf[64]; snprintf(buf, 64, "XOR %s, %s, %s", a[0], a[0], a[0]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "INC")) { char buf[64]; snprintf(buf, 64, "ADDI %s, %s, 1", a[0], a[0]); return assemble_line(buf, line_num); }
    if (!strcmp(m, "DEC")) { char buf[64]; snprintf(buf, 64, "SUBI %s, %s, 1", a[0], a[0]); return assemble_line(buf, line_num); }

    uint32_t op = 0, ctrl = 0;
    int r_dest = 0, r_src1 = 0, r_src2 = 0;
    int imm_val = 0;

    // --- ALU (R-Type) ---
    // [Op] [00] [Rd] [Rs] [Rt(15-12)]
    int is_rtype = 0;
    if (!strcmp(m, "ADD")) { op = 32; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "SUB")) { op = 33; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "AND")) { op = 34; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "OR"))  { op = 35; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "XOR")) { op = 36; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "NAND")) { op = 37; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "NOR")) { op = 38; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "XNOR")) { op = 39; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "ADC")) { op = 32; ctrl = 1; is_rtype = 1; }
    else if (!strcmp(m, "SBB")) { op = 33; ctrl = 1; is_rtype = 1; }
    else if (!strcmp(m, "SHLV")) { op = 43; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "SHRV")) { op = 44; ctrl = 0; is_rtype = 1; }
    else if (!strcmp(m, "SARV")) { op = 45; ctrl = 0; is_rtype = 1; }

    if (is_rtype)
    {
        GET_REG(r_dest, a[0]);
        GET_REG(r_src1, a[1]);
        GET_REG(r_src2, a[2]);
        // Byte 1: Op(6)|Ctrl(2), Byte 2: Dest(4)|Src1(4), Byte 3: Src2(4)|Pad(4)
        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (r_src1 << 16) | (r_src2 << 12);
    }

    // --- ALU (I-Type) & Shifts ---
    // [Op] [10/11] [Rd] [Rs] [Imm]
    int is_itype = 0;
    if (!strcmp(m, "ADDI")) { op = 32; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "SUBI")) { op = 33; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "ANDI")) { op = 34; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "ORI"))  { op = 35; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "XORI")) { op = 36; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "NANDI")) { op = 37; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "NORI")) { op = 38; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "XNORI")) { op = 39; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "ADCI")) { op = 32; ctrl = 3; is_itype = 1; }
    else if (!strcmp(m, "SBBI")) { op = 33; ctrl = 3; is_itype = 1; }
    else if (!strcmp(m, "SHL"))  { op = 40; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "SHR"))  { op = 41; ctrl = 2; is_itype = 1; }
    else if (!strcmp(m, "SAR"))  { op = 42; ctrl = 2; is_itype = 1; }

    if (is_itype)
    {
        GET_REG(r_dest, a[0]);
        GET_REG(r_src1, a[1]);
        imm_val = parse_arg(a[2]);
        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (r_src1 << 16) | (imm_val & 0xFFFF);
    }

    // --- Math Execution ---
    // [Op] [00] [0000] [Rs1] [Rs2(15-12)]
    if (!strcmp(m, "MUL") || !strcmp(m, "DIV") || !strcmp(m, "SQRT"))
    {
        ctrl = 0;
        if (!strcmp(m, "MUL")) op = 1;
        else if (!strcmp(m, "DIV")) op = 2;
        else if (!strcmp(m, "SQRT")) op = 3;

        GET_REG(r_src1, a[0]);
        if (op != 3) // SQRT only takes 1 argument
        {
            GET_REG(r_src2, a[1]);
        }
        else
        {
            r_src2 = 0;
        }

        return (op << 26) | (ctrl << 24) | (0 << 20) | (r_src1 << 16) | (r_src2 << 12);
    }

    // --- Math Move (MFHI/MFLO with Offset) ---
    // [Op] [10] [Rd] [0000] [Imm]
    if (!strcmp(m, "MFHI") || !strcmp(m, "MFLO"))
    {
        ctrl = 2; // Control 10
        int is_hi = (!strcmp(m, "MFHI"));

        if (!strcmp(sub_type, "MUL")) op = is_hi ? 48 : 49;
        else if (!strcmp(sub_type, "DIV")) op = is_hi ? 50 : 51;
        else if (!strcmp(sub_type, "SQRT")) op = is_hi ? 52 : 53;
        else
        {
            printf("\033[0;31m[Error] Line %d: Invalid Subtype '%s' (Use .MUL, .DIV, .SQRT)\n\033[0m", line_num, sub_type);
            return 0xFFFFFFFF;
        }

        GET_REG(r_dest, a[0]);
        // Argument 1 is immediate (default 0)
        imm_val = (ac > 1) ? parse_arg(a[1]) : 0;

        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (0 << 16) | (imm_val & 0xFFFF);
    }

    // --- Memory Access ---
    // LOD: [56] [10] [Rd] [Base] [Imm]
    if (!strcmp(m, "LOD"))
    {
        op = 56; ctrl = 2;
        GET_REG(r_dest, a[0]);
        GET_REG(r_src1, a[1]);
        imm_val = parse_arg(a[2]);
        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (r_src1 << 16) | (imm_val & 0xFFFF);
    }

    // STR: [16] [10] [SrcData] [Base] [Imm]
    // Note: STR Src, Base, Offset
    if (!strcmp(m, "STR"))
    {
        op = 16; ctrl = 2;
        GET_REG(r_dest, a[0]); // SrcData maps to Dest field
        GET_REG(r_src1, a[1]); // Base maps to Src1 field
        imm_val = parse_arg(a[2]);
        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (r_src1 << 16) | (imm_val & 0xFFFF);
    }

    // --- Stack Operations ---
    // PSH: [17] [10] [SrcData] [SP] [Imm]
    // Syntax: PSH Src, Offset
    if (!strcmp(m, "PSH"))
    {
        op = 17; ctrl = 2;
        GET_REG(r_dest, a[0]); // SrcData maps to Dest field
        r_src1 = 15; // SP
        imm_val = parse_arg(a[1]);
        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (r_src1 << 16) | (imm_val & 0xFFFF);
    }

    // POP: [57] [10] [Dest] [SP] [Imm]
    // Syntax: POP Dest, Offset
    if (!strcmp(m, "POP"))
    {
        op = 57; ctrl = 2;
        GET_REG(r_dest, a[0]);
        r_src1 = 15; // SP
        imm_val = parse_arg(a[1]);
        return (op << 26) | (ctrl << 24) | (r_dest << 20) | (r_src1 << 16) | (imm_val & 0xFFFF);
    }

    // --- Control Flow ---
    // BRN (Merged Branch): [10] [00] [Cond] [0000] [Addr]
    int cond = -1;
    if (!strcmp(m, "JMP") || !strcmp(m, "BRN")) cond = 0;
    else if (!strcmp(m, "JEZ")) cond = 1;
    else if (!strcmp(m, "JNZ")) cond = 2;
    else if (!strcmp(m, "JLZ")) cond = 3;
    else if (!strcmp(m, "JLEZ")) cond = 4;
    else if (!strcmp(m, "JGZ")) cond = 5;
    else if (!strcmp(m, "JGEZ")) cond = 6;
    else if (!strcmp(m, "JOF")) cond = 7;
    else if (!strcmp(m, "JCF")) cond = 8;
    else if (!strcmp(m, "JE")) cond = 9;
    else if (!strcmp(m, "JO")) cond = 10;

    if (cond != -1)
    {
        op = 10; ctrl = 0;
        imm_val = parse_arg(a[0]);
        // Cond goes to Dest field (20-23)
        return (op << 26) | (ctrl << 24) | (cond << 20) | (0 << 16) | (imm_val & 0xFFFF);
    }

    // CAL: [8] [00] ... [Addr]
    if (!strcmp(m, "CAL"))
    {
        op = 8; ctrl = 0;
        imm_val = parse_arg(a[0]);
        return (op << 26) | (ctrl << 24) | (imm_val & 0xFFFF);
    }

    // RET: [9] [00] ...
    if (!strcmp(m, "RET"))
    {
        op = 9; ctrl = 0;
        return (op << 26) | (ctrl << 24);
    }

    // --- Misc ---
    // RAND: [54] [00] [Rd] ...
    if (!strcmp(m, "RAND"))
    {
        op = 54; ctrl = 0;
        GET_REG(r_dest, a[0]);
        return (op << 26) | (ctrl << 24) | (r_dest << 20);
    }

    // Unknown Instruction
    printf("\033[0;31m[Error] Line %d: Unknown Instruction '%s'\n\033[0m", line_num, m);
    return 0xFFFFFFFF;
}

// --- Geometry ---
Coord get_barrel_pos(int addr, int bit)
{
    Coord pos;
    int page = addr / 64, group = page / 32, local = page % 32, batch = (addr % 64) / 4;
    pos.z = (group * 40) + (batch * 2);
    pos.x = (local * -4) - (((bit < 16) ^ (page & 1)) * 2);
    int rel = (bit >= 16) ? (31 - bit) : (15 - bit);
    pos.y = rel * -2;
    if (rel >= 8) pos.y -= 2;
    return pos;
}

// ======================================================================================
// Streaming NBT Writer
// ======================================================================================
uint16_t swap16(uint16_t val)
{
    return (val << 8) | (val >> 8);
}
uint32_t swap32(uint32_t val)
{
    return (val << 24) | ((val << 8) & 0x00FF0000) | ((val >> 8) & 0x0000FF00) | (val >> 24);
}

void w_byte(gzFile f, uint8_t b)
{
    gzwrite(f, &b, 1);
}
void w_short(gzFile f, int16_t s)
{
    uint16_t v = swap16((uint16_t)s);
    gzwrite(f, &v, 2);
}
void w_int(gzFile f, int32_t i)
{
    uint32_t v = swap32((uint32_t)i);
    gzwrite(f, &v, 4);
}
void w_data(gzFile f, const void* data, int len)
{
    gzwrite(f, data, len);
}

void w_tag_name(gzFile f, uint8_t type, const char* name)
{
    w_byte(f, type);
    int len = strlen(name);
    w_short(f, (int16_t)len);
    w_data(f, name, len);
}

void w_string_payload(gzFile f, const char* str)
{
    int len = strlen(str);
    w_short(f, (int16_t)len);
    w_data(f, str, len);
}

void stream_schematic(uint32_t* machine_code, int code_count, int target_pages, const char* filename)
{
    printf("[*] Streaming Schematic (Labels + Y-5 Fixed)... \n");

    int total_instr_slots = target_pages * 64;
    int min_x = 0, max_z = 0, min_y = -32;

    for (int i = 0; i < total_instr_slots; i += 4)
    {
        Coord p = get_barrel_pos(i, 0);
        if (p.x < min_x) min_x = p.x;
        if (p.z > max_z) max_z = p.z;
    }
    if (min_x > -2) min_x = -2;
    min_x -= 2;
    int width = abs(min_x) + 1, height = abs(min_y) + 1, length = max_z + 1;
    Coord origin = get_barrel_pos(0, 31);

    gzFile f = gzopen(filename, "wb");
    if (!f)
    {
        printf("Error: Cannot open %s for writing.\n", filename);
        return;
    }

    w_byte(f, 10);
    w_short(f, 0);

    w_tag_name(f, 10, "Schematic");
    w_tag_name(f, 3, "Version");
    w_int(f, 3);
    w_tag_name(f, 3, "DataVersion");
    w_int(f, MC_DATA_VERSION);
    w_tag_name(f, 2, "Width");
    w_short(f, (int16_t)width);
    w_tag_name(f, 2, "Height");
    w_short(f, (int16_t)height);
    w_tag_name(f, 2, "Length");
    w_short(f, (int16_t)length);

    w_tag_name(f, 11, "Offset"); // Int Array
    w_int(f, 3);
    w_int(f, -(origin.x - min_x));
    w_int(f, -(origin.y - min_y) - 5); // [Fixed] Y-offset - 5
    w_int(f, -(origin.z));

    w_tag_name(f, 10, "Blocks");

    w_tag_name(f, 10, "Palette");
    w_tag_name(f, 3, "minecraft:air");
    w_int(f, 0);
    w_tag_name(f, 3, "minecraft:barrel[facing=up,open=false]");
    w_int(f, 1);
    w_byte(f, 0);

    int total_blocks = width * height * length;
    w_tag_name(f, 7, "Data");
    w_int(f, total_blocks);

    unsigned char* raw_block_data = calloc(total_blocks, 1);
    int entity_count_prediction = 0;

    for (int base_addr = 0; base_addr < total_instr_slots; base_addr += 4)
    {
        for (int bit = 31; bit >= 0; bit--)
        {
            Coord logical_pos = get_barrel_pos(base_addr, bit);
            int sx = logical_pos.x - min_x, sy = logical_pos.y - min_y, sz = logical_pos.z;
            int index = (sy * length + sz) * width + sx;
            if (index >= 0 && index < total_blocks)
            {
                raw_block_data[index] = 1;
                entity_count_prediction++;
            }
        }
    }
    w_data(f, raw_block_data, total_blocks);
    free(raw_block_data);

    printf("    -> Streaming BlockEntities (%d barrels)...\n", entity_count_prediction);
    w_tag_name(f, 9, "BlockEntities");
    w_byte(f, 10);
    w_int(f, entity_count_prediction);

    for (int base_addr = 0; base_addr < total_instr_slots; base_addr += 4)
    {
        for (int bit = 31; bit >= 0; bit--)
        {
            Coord logical_pos = get_barrel_pos(base_addr, bit);
            int sx = logical_pos.x - min_x, sy = logical_pos.y - min_y, sz = logical_pos.z;
            if (sx < 0 || sx >= width || sy < 0 || sy >= height || sz < 0 || sz >= length) continue;

            int signal = 0;
            for (int i = 0; i < 4; i++)
            {
                if (base_addr + i < code_count)
                {
                    int b_val = (machine_code[base_addr + i] >> bit) & 1;
                    signal |= (b_val << i);
                }
            }

            w_tag_name(f, 11, "Pos");
            w_int(f, 3);
            w_int(f, sx);
            w_int(f, sy);
            w_int(f, sz);

            w_tag_name(f, 8, "Id");
            w_string_payload(f, "minecraft:barrel");
            w_tag_name(f, 3, "ContentVersion");
            w_int(f, 1);

            w_tag_name(f, 10, "Data");
            char name_buf[64];
            snprintf(name_buf, 64, "{\"text\":\"%d\"}", signal);
            w_tag_name(f, 8, "CustomName");
            w_string_payload(f, name_buf);

            int count = ITEMS_PER_SIGNAL[signal];
            int stacks = 0;
            if (count > 0) stacks = (count + 63) / 64;

            w_tag_name(f, 9, "Items");
            w_byte(f, 10);
            w_int(f, stacks);

            int remaining = count;
            int slot = 0;
            while (remaining > 0)
            {
                int stack_size = (remaining > 64) ? 64 : remaining;
                w_tag_name(f, 1, "Slot");
                w_byte(f, slot++);
                w_tag_name(f, 8, "id");
                w_string_payload(f, "minecraft:sculk_shrieker");
                w_tag_name(f, 1, "Count");
                w_byte(f, (int8_t)stack_size);
                w_byte(f, 0);
                remaining -= stack_size;
            }
            w_byte(f, 0);
            w_byte(f, 0);
        }
    }

    w_byte(f, 0);
    w_byte(f, 0);
    w_byte(f, 0);

    gzclose(f);
    printf("[+] Success! Streamed '%s'.\n", filename);
}

int main()
{
    printf("--- LEAF Assembler (v5.0 Updated ISA) ---\n");
    char input_buf[100];
    int target_pages = 64;
    int debug_mode = 0;

    printf("Enter Target ROM Size in Pages (default 64): ");
    if (fgets(input_buf, sizeof(input_buf), stdin))
    {
        int val = atoi(input_buf);
        if (val > 0 && val <= MAX_PAGES_LIMIT) target_pages = val;
    }

    printf("Enable Debug Mode (Show detailed logs)? (y/n): ");
    if (fgets(input_buf, sizeof(input_buf), stdin))
    {
        if (input_buf[0] == 'y' || input_buf[0] == 'Y')
        {
            debug_mode = 1;
            printf("[*] Debug Mode: ON\n");
        }
    }

    uint32_t* code_mem = (uint32_t*)calloc(MAX_CODE_SIZE, sizeof(uint32_t));
    int code_count = 0;

    FILE* fp = fopen(DEFAULT_INPUT_FILE, "r");
    if (!fp)
    {
        printf("Error: No '%s' found.\n", DEFAULT_INPUT_FILE);
        system("pause");
        return 1;
    }

    // =========================================================
    // [Pass 1] Scan Labels
    // =========================================================
    printf("[*] Pass 1: Scanning Labels...\n");
    char line[256];
    int pc_counter = 0;

    while (fgets(line, sizeof(line), fp))
    {
        char* c = strchr(line, ';');
        if (c) *c = 0;
        char* trimmed = trim_whitespace(line);
        if (strlen(trimmed) == 0) continue;

        int len = strlen(trimmed);
        if (trimmed[len - 1] == ':')
        {
            trimmed[len - 1] = '\0';
            if (label_count < MAX_LABELS)
            {
                strcpy(label_table[label_count].name, trimmed);
                label_table[label_count].address = pc_counter;
                printf("    [Label] %s -> PC: %d\n", trimmed, pc_counter);
                label_count++;
            }
            continue;
        }
        pc_counter++;
    }

    // =========================================================
    // [Pass 2] Assemble Code
    // =========================================================
    printf("[*] Pass 2: Assembling Code...\n");
    rewind(fp);

    int line_num = 0;
    int error_flag = 0;

    while (fgets(line, sizeof(line), fp))
    {
        line_num++;

        if (code_count >= MAX_CODE_SIZE) break;

        char* c = strchr(line, ';');
        if (c) *c = 0;
        char* trimmed = trim_whitespace(line);
        if (strlen(trimmed) == 0) continue;

        if (trimmed[strlen(trimmed) - 1] == ':') continue;

        uint32_t inst = assemble_line(line, line_num);

        if (inst == 0xFFFFFFFF)
        {
            error_flag = 1;
        }
        else
        {
            if (debug_mode)
            {
                printf("[PC:%04d] 0x%08X | %s\n", code_count, inst, trimmed);
            }
        }

        code_mem[code_count++] = inst;
    }
    fclose(fp);

    if (error_flag)
    {
        printf("\n\033[0;31m[!] Critical Errors found. Assembly aborted.\033[0m\n");
        printf("Please fix the syntax errors in 'input.asm' and try again.\n");
        free(code_mem);
        system("pause");
        return 1;
    }

    printf("[*] Assembled %d instructions.\n", code_count);

    stream_schematic(code_mem, code_count, target_pages, DEFAULT_OUTPUT_FILE);

    free(code_mem);
    system("pause");
    return 0;
}