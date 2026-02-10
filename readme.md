# Lookahead Execution Architecture & Flow (LEAF™) CPU Assembler

**Assembler for a 16-bit Minecraft Redstone CPU featuring BTB-based Branch Prediction.**

This tool converts custom assembly language into a Minecraft `.schem` file (Sponge Schematic), allowing you to import the compiled machine code directly into the game using WorldEdit or Litematica. It is designed specifically for the **LEAF ISA**.

---

### Features
* **Label Support:** Easy jumping and branching using labels (e.g., `LOOP:`, `JMP LOOP`) without manual address calculation.
* **Safety Checks:** Detects redundant instructions and long pipeline stalls.

### Prerequisites
* **GCC** Compiler
* **zlib** Development Library
    * Windows: Install via MinGW/MSYS2.
    * Linux: `sudo apt-get install zlib1g-dev`

### Build
Since this project uses zlib for generating compressed schematic files, you must link the library during compilation.
```
gcc main.c zlib-1.3.1/*.c -o assembler -I zlib-1.3.1
```

### Usage
1. Place your code in `input.asm`.
2. Run the assembler.
3. Enter **ROM Size** (default:64) and **Debug Mode** (y/n).
4. Load `rom_output.schem` into Minecraft world.

-----

# Lookahead Execution Architecture & Flow (LEAF™) CPU 어셈블러

**BTB 기반의 분기 예측을 지원하는 16비트 마인크래프트 레드스톤 CPU의 어셈블러입니다.**

이 도구는 커스텀 어셈블리어 프로그램을 마인크래프트 `.schem` 파일(Sponge Schematic)로 변환해줍니다. 생성된 파일은 월드에딧(WorldEdit)이나 라이트매티카(Litematica)를 통해 게임 내로 불러올 수 있습니다. **LEAF ISA**에 맞춰 설계되었습니다.

---

### 주요 기능
* **라벨 지원:** 라벨을 이용하여 주소 계산 없이 편리하게 분기 (e.g., `LOOP:`, `JMP LOOP`)할 수 있습니다.
* **최적화 경고:** 불필요한 명령어나 긴 파이프라인 스톨(Stall)을 유발할 수 있는 코드를 감지합니다.

### 요구사항
* **GCC** 컴파일러
* **zlib** 압축 라이브러리
    * Windows: MinGW 또는 MSYS2 환경에 `zlib` 라이브러리가 설치되어 있어야 합니다.
    * Linux: `sudo apt-get install zlib1g-dev`

### 빌드
```
gcc main.c zlib-1.3.1/*.c -o assembler -I zlib-1.3.1
```

### 사용법
1. `input.asm`파일에 어셈블리 코드를 작성합니다.
2. 어셈블러를 실행합니다.
3. **ROM 크기**(기본 64)와 **디버그 모드**(y/n)를 입력합니다.
4. 같은 폴더 내에 생성된 `rom_output.schem` 파일을 마인크래프트 월드에 적용합니다.