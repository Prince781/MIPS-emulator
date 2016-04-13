#include <stdio.h>
#include <inttypes.h>
#include "RegFile.h"

#define KNRM	"\x1B[0m"	// normal console color
#define KRED	"\x1B[31m"
#define KGRN	"\x1B[32m"
#define KYEL	"\x1B[33m"
#define KBLU	"\x1B[34m"
#define KMAG	"\x1B[35m"
#define KCYN	"\x1B[36m"
#define KWHT	"\x1B[37m"
#define KRST	"\033[0m"	// resets to previous console color

// 32x32 Register File
int32_t RegFile[NUMBER_OF_REGS];

int32_t OldRegFile[NUMBER_OF_REGS];

void initRegFile(int32_t val) {
    int i;
    
    for(i=0;i<34;i++)
        RegFile[i] = OldRegFile[i] = val;
}

void printRegFile() {
    printf("\n ----- REG DUMP ----- \n");
    int j;
    for ( j=0; j < NUMBER_OF_REGS; j++) {
        if (OldRegFile[j] != RegFile[j])
            printf("REG[%2d]: "KRED"0x%08x (%d)"KRST,j,RegFile[j],RegFile[j]);
        else
            printf("REG[%2d]: 0x%08x (%d)",j,RegFile[j],RegFile[j]);
        if(j%2==0){printf("\t\t");}
        else{printf("\n");}
        OldRegFile[j] = RegFile[j];
    }
}




