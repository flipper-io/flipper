#ifndef __is25lp_h__
#define __is25lp_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper.h>

#define IS25LP_PAGE_SIZE 256
#define IS25LP_SECTOR_SIZE 16

/* IS25LP Opcodes */
#define IS25LP_NORD			0x03
#define IS25LP_FRD			0x0B
#define IS25LP_FRDIO		0xBB
#define IS25LP_FRDO			0x3B
#define IS25LP_FDQIO		0xEB
#define IS25LP_FRQO			0x6B
#define IS25LP_FRDTR		0x0D
#define IS25LP_FRDDTR		0xBD
#define IS25LP_FRQDTR		0xED
#define IS25LP_PP			0x02
#define IS25LP_PPQ			0x32
#define IS25LP_SER			0xD7
#define IS25LP_BER32		0x52
#define IS25LP_BER64		0xD8
#define IS25LP_CER			0xC7
#define IS25LP_WREN			0x06
#define IS25LP_WRDI			0x04
#define IS25LP_RDSR			0x05
#define IS25LP_WRSR			0x01
#define IS25LP_RDFR			0x48
#define IS25LP_WRFR			0x42
#define IS25LP_QPIEN		0x35
#define IS25LP_QPIDI		0xF5
#define IS25LP_PERSUS		0x75
#define IS25LP_PERRSM		0x7A
#define IS25LP_DP			0xB9
#define IS25LP_RDID			0xAB
#define IS25LP_SRP			0xC0
#define IS25LP_RDJDID		0x9F
#define IS25LP_RDMDID		0x90
#define IS25LP_RDJIDQ		0xAF
#define IS25LP_RDUID		0x4B
#define IS25LP_RDSFDP		0x5A
#define IS25LP_NOP			0x00
#define IS25LP_RSTEN		0x66
#define IS25LP_RST			0x99
#define IS25LP_IRER			0x64
#define IS25LP_IRP			0x62
#define IS25LP_IRRD			0x68
#define IS25LP_SECUNLOCK2	0x26
#define IS25LP_SECLOCK		0x24

/* Status register/ */
enum IS25LP_SR {
	IS25LP_SR_WIP,
	IS25LP_SR_WEL,
	IS25LP_SR_BP0,
	IS25LP_BP1,
	IS25LP_BP2,
	IS25LP_BP3,
	IS25LP_QE,
	IS25LP_SRWD
};

int is25lp_configure(void);

#endif
