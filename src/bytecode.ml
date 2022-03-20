(* Copyright (C) 2021 Nunuhara Cabbage <nunuhara@haniwa.technology>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://gnu.org/licenses/>.
 *)

open Core

type opcode =
  | PUSH
  | POP
  | REF
  | REFREF
  | PUSHGLOBALPAGE
  | PUSHLOCALPAGE
  | INV
  | NOT
  | COMPL
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | AND
  | OR
  | XOR
  | LSHIFT
  | RSHIFT
  | LT
  | GT
  | LTE
  | GTE
  | NOTE
  | EQUALE
  | ASSIGN
  | PLUSA
  | MINUSA
  | MULA
  | DIVA
  | MODA
  | ANDA
  | ORA
  | XORA
  | LSHIFTA
  | RSHIFTA
  | F_ASSIGN
  | F_PLUSA
  | F_MINUSA
  | F_MULA
  | F_DIVA
  | DUP2
  | DUP_X2
  | CMP
  | JUMP
  | IFZ
  | IFNZ
  | RETURN
  | CALLFUNC
  | INC
  | DEC
  | FTOI
  | ITOF
  | F_INV
  | F_ADD
  | F_SUB
  | F_MUL
  | F_DIV
  | F_LT
  | F_GT
  | F_LTE
  | F_GTE
  | F_NOTE
  | F_EQUALE
  | F_PUSH
  | S_PUSH
  | S_POP
  | S_ADD
  | S_ASSIGN
  | S_PLUSA
  | S_REF
  | S_REFREF
  | S_NOTE
  | S_EQUALE
  | SF_CREATE
  | SF_CREATEPIXEL
  | SF_CREATEALPHA
  | SR_POP
  | SR_ASSIGN
  | SR_REF
  | SR_REFREF
  | A_ALLOC
  | A_REALLOC
  | A_FREE
  | A_NUMOF
  | A_COPY
  | A_FILL
  | C_REF
  | C_ASSIGN
  | MSG
  | CALLHLL
  | PUSHSTRUCTPAGE
  | CALLMETHOD
  | SH_GLOBALREF
  | SH_LOCALREF
  | SWITCH
  | STRSWITCH
  | FUNC
  | EOF
  | CALLSYS
  | SJUMP
  | CALLONJUMP
  | SWAP
  | SH_STRUCTREF
  | S_LENGTH
  | S_LENGTHBYTE
  | I_STRING
  | CALLFUNC2
  | DUP2_X1
  | R_ASSIGN
  | FT_ASSIGNS
  | ASSERT
  | S_LT
  | S_GT
  | S_LTE
  | S_GTE
  | S_LENGTH2
  | S_LENGTHBYTE2
  | NEW
  | DELETE
  | CHECKUDO
  | A_REF
  | DUP
  | DUP_U2
  | SP_INC
  | SP_DEC
  | ENDFUNC
  | R_EQUALE
  | R_NOTE
  | SH_LOCALCREATE
  | SH_LOCALDELETE
  | STOI
  | A_PUSHBACK
  | A_POPBACK
  | S_EMPTY
  | A_EMPTY
  | A_ERASE
  | A_INSERT
  | SH_LOCALINC
  | SH_LOCALDEC
  | SH_LOCALASSIGN
  | ITOB
  | S_FIND
  | S_GETPART
  | A_SORT
  | S_PUSHBACK
  | S_POPBACK
  | FTOS
  | S_MOD
  | S_PLUSA2
  | OBJSWAP
  | S_ERASE
  | SR_REF2
  | S_ERASE2
  | S_PUSHBACK2
  | S_POPBACK2
  | ITOLI
  | LI_ADD
  | LI_SUB
  | LI_MUL
  | LI_DIV
  | LI_MOD
  | LI_ASSIGN
  | LI_PLUSA
  | LI_MINUSA
  | LI_MULA
  | LI_DIVA
  | LI_MODA
  | LI_ANDA
  | LI_ORA
  | LI_XORA
  | LI_LSHIFTA
  | LI_RSHIFTA
  | LI_INC
  | LI_DEC
  | A_FIND
  | A_REVERSE
  | SH_SR_ASSIGN
  | SH_MEM_ASSIGN_LOCAL
  | A_NUMOF_GLOB_1
  | A_NUMOF_STRUCT_1
  | SH_MEM_ASSIGN_IMM
  | SH_LOCALREFREF
  | SH_LOCALASSIGN_SUB_IMM
  | SH_IF_LOC_LT_IMM
  | SH_IF_LOC_GE_IMM
  | SH_LOCREF_ASSIGN_MEM
  | PAGE_REF
  | SH_GLOBAL_ASSIGN_LOCAL
  | SH_STRUCTREF_GT_IMM
  | SH_STRUCT_ASSIGN_LOCALREF_ITOB
  | SH_LOCAL_ASSIGN_STRUCTREF
  | SH_IF_STRUCTREF_NE_LOCALREF
  | SH_IF_STRUCTREF_GT_IMM
  | SH_STRUCTREF_CALLMETHOD_NO_PARAM
  | SH_STRUCTREF2
  | SH_REF_STRUCTREF2
  | SH_STRUCTREF3
  | SH_STRUCTREF2_CALLMETHOD_NO_PARAM
  | SH_IF_STRUCTREF_Z
  | SH_IF_STRUCT_A_NOT_EMPTY
  | SH_IF_LOC_GT_IMM
  | SH_IF_STRUCTREF_NE_IMM
  | THISCALLMETHOD_NOPARAM
  | SH_IF_LOC_NE_IMM
  | SH_IF_STRUCTREF_EQ_IMM
  | SH_GLOBAL_ASSIGN_IMM
  | SH_LOCALSTRUCT_ASSIGN_IMM
  | SH_STRUCT_A_PUSHBACK_LOCAL_STRUCT
  | SH_GLOBAL_A_PUSHBACK_LOCAL_STRUCT
  | SH_LOCAL_A_PUSHBACK_LOCAL_STRUCT
  | SH_IF_SREF_NE_STR0
  | SH_S_ASSIGN_REF
  | SH_A_FIND_SREF
  | SH_SREF_EMPTY
  | SH_STRUCTSREF_EQ_LOCALSREF
  | SH_LOCALSREF_EQ_STR0
  | SH_STRUCTSREF_NE_LOCALSREF
  | SH_LOCALSREF_NE_STR0
  | SH_STRUCT_SR_REF
  | SH_STRUCT_S_REF
  | S_REF2
  | SH_REF_LOCAL_ASSIGN_STRUCTREF2
  | SH_GLOBAL_S_REF
  | SH_LOCAL_S_REF
  | SH_LOCALREF_SASSIGN_LOCALSREF
  | SH_LOCAL_APUSHBACK_LOCALSREF
  | SH_S_ASSIGN_CALLSYS19
  | SH_S_ASSIGN_STR0
  | SH_SASSIGN_LOCALSREF
  | SH_STRUCTREF_SASSIGN_LOCALSREF
  | SH_LOCALSREF_EMPTY
  | SH_GLOBAL_APUSHBACK_LOCALSREF
  | SH_STRUCT_APUSHBACK_LOCALSREF
  | SH_STRUCTSREF_EMPTY
  | SH_GLOBALSREF_EMPTY
  | SH_SASSIGN_STRUCTSREF
  | SH_SASSIGN_GLOBALSREF
  | SH_STRUCTSREF_NE_STR0
  | SH_GLOBALSREF_NE_STR0
  | SH_LOC_LT_IMM_OR_LOC_GE_IMM
  | A_SORT_MEM
  | DG_ADD
  | DG_SET
  | DG_CALL
  | DG_NUMOF
  | DG_EXIST
  | DG_ERASE
  | DG_CLEAR
  | DG_COPY
  | DG_ASSIGN
  | DG_PLUSA
  | DG_POP
  | DG_NEW_FROM_METHOD
  | DG_MINUSA
  | DG_CALLBEGIN
  | DG_NEW
  | DG_STR_TO_METHOD
  | OP_0X102
  | X_GETENV
  | X_SET
  | X_ICAST
  | X_OP_SET
  | OP_0X107
  | OP_0X108
  | OP_0X109
  | X_DUP
  | X_MOV
  | X_REF
  | X_ASSIGN
  | X_A_INIT
  | X_A_SIZE
  | X_TO_STR

let int_of_opcode = function
  | PUSH           -> 0x00
  | POP            -> 0x01
  | REF            -> 0x02
  | REFREF         -> 0x03
  | PUSHGLOBALPAGE -> 0x04
  | PUSHLOCALPAGE  -> 0x05
  | INV            -> 0x06
  | NOT            -> 0x07
  | COMPL          -> 0x08
  | ADD            -> 0x09
  | SUB            -> 0x0a
  | MUL            -> 0x0b
  | DIV            -> 0x0c
  | MOD            -> 0x0d
  | AND            -> 0x0e
  | OR             -> 0x0f
  | XOR            -> 0x10
  | LSHIFT         -> 0x11
  | RSHIFT         -> 0x12
  | LT             -> 0x13
  | GT             -> 0x14
  | LTE            -> 0x15
  | GTE            -> 0x16
  | NOTE           -> 0x17
  | EQUALE         -> 0x18
  | ASSIGN         -> 0x19
  | PLUSA          -> 0x1a
  | MINUSA         -> 0x1b
  | MULA           -> 0x1c
  | DIVA           -> 0x1d
  | MODA           -> 0x1e
  | ANDA           -> 0x1f
  | ORA            -> 0x20
  | XORA           -> 0x21
  | LSHIFTA        -> 0x22
  | RSHIFTA        -> 0x23
  | F_ASSIGN       -> 0x24
  | F_PLUSA        -> 0x25
  | F_MINUSA       -> 0x26
  | F_MULA         -> 0x27
  | F_DIVA         -> 0x28
  | DUP2           -> 0x29
  | DUP_X2         -> 0x2a
  | CMP            -> 0x2b
  | JUMP           -> 0x2c
  | IFZ            -> 0x2d
  | IFNZ           -> 0x2e
  | RETURN         -> 0x2f
  | CALLFUNC       -> 0x30
  | INC            -> 0x31
  | DEC            -> 0x32
  | FTOI           -> 0x33
  | ITOF           -> 0x34
  | F_INV          -> 0x35
  | F_ADD          -> 0x36
  | F_SUB          -> 0x37
  | F_MUL          -> 0x38
  | F_DIV          -> 0x39
  | F_LT           -> 0x3a
  | F_GT           -> 0x3b
  | F_LTE          -> 0x3c
  | F_GTE          -> 0x3D
  | F_NOTE         -> 0x3E
  | F_EQUALE       -> 0x3f
  | F_PUSH         -> 0x40
  | S_PUSH         -> 0x41
  | S_POP          -> 0x42
  | S_ADD          -> 0x43
  | S_ASSIGN       -> 0x44
  | S_PLUSA        -> 0x45
  | S_REF          -> 0x46
  | S_REFREF       -> 0x47
  | S_NOTE         -> 0x48
  | S_EQUALE       -> 0x49
  | SF_CREATE      -> 0x4A
  | SF_CREATEPIXEL -> 0x4B
  | SF_CREATEALPHA -> 0x4C
  | SR_POP         -> 0x4d
  | SR_ASSIGN      -> 0x4e
  | SR_REF         -> 0x4f
  | SR_REFREF      -> 0x50
  | A_ALLOC        -> 0x51
  | A_REALLOC      -> 0x52
  | A_FREE         -> 0x53
  | A_NUMOF        -> 0x54
  | A_COPY         -> 0x55
  | A_FILL         -> 0x56
  | C_REF          -> 0x57
  | C_ASSIGN       -> 0x58
  | MSG            -> 0x59
  | CALLHLL        -> 0x5a
  | PUSHSTRUCTPAGE -> 0x5b
  | CALLMETHOD     -> 0x5c
  | SH_GLOBALREF   -> 0x5d
  | SH_LOCALREF    -> 0x5e
  | SWITCH         -> 0x5f
  | STRSWITCH      -> 0x60
  | FUNC           -> 0x61
  | EOF            -> 0x62
  | CALLSYS        -> 0x63
  | SJUMP          -> 0x64
  | CALLONJUMP     -> 0x65
  | SWAP           -> 0x66
  | SH_STRUCTREF   -> 0x67
  | S_LENGTH       -> 0x68
  | S_LENGTHBYTE   -> 0x69
  | I_STRING       -> 0x6a
  | CALLFUNC2      -> 0x6b
  | DUP2_X1        -> 0x6c
  | R_ASSIGN       -> 0x6d
  | FT_ASSIGNS     -> 0x6e
  | ASSERT         -> 0x6f
  | S_LT           -> 0x70
  | S_GT           -> 0x71
  | S_LTE          -> 0x72
  | S_GTE          -> 0x73
  | S_LENGTH2      -> 0x74
  | S_LENGTHBYTE2  -> 0x75
  | NEW            -> 0x76
  | DELETE         -> 0x77
  | CHECKUDO       -> 0x78
  | A_REF          -> 0x79
  | DUP            -> 0x7a
  | DUP_U2         -> 0x7b
  | SP_INC         -> 0x7c
  | SP_DEC         -> 0x7d
  | ENDFUNC        -> 0x7e
  | R_EQUALE       -> 0x7f
  | R_NOTE         -> 0x80
  | SH_LOCALCREATE -> 0x81
  | SH_LOCALDELETE -> 0x82
  | STOI           -> 0x83
  | A_PUSHBACK     -> 0x84
  | A_POPBACK      -> 0x85
  | S_EMPTY        -> 0x86
  | A_EMPTY        -> 0x87
  | A_ERASE        -> 0x88
  | A_INSERT       -> 0x89
  | SH_LOCALINC    -> 0x8a
  | SH_LOCALDEC    -> 0x8b
  | SH_LOCALASSIGN -> 0x8c
  | ITOB           -> 0x8d
  | S_FIND         -> 0x8e
  | S_GETPART      -> 0x8f
  | A_SORT         -> 0x90
  | S_PUSHBACK     -> 0x91
  | S_POPBACK      -> 0x92
  | FTOS           -> 0x93
  | S_MOD          -> 0x94
  | S_PLUSA2       -> 0x95
  | OBJSWAP        -> 0x96
  | S_ERASE        -> 0x97
  | SR_REF2        -> 0x98
  | S_ERASE2       -> 0x99
  | S_PUSHBACK2    -> 0x9A
  | S_POPBACK2     -> 0x9B
  | ITOLI          -> 0x9c
  | LI_ADD         -> 0x9d
  | LI_SUB         -> 0x9e
  | LI_MUL         -> 0x9f
  | LI_DIV         -> 0xa0
  | LI_MOD         -> 0xa1
  | LI_ASSIGN      -> 0xa2
  | LI_PLUSA       -> 0xa3
  | LI_MINUSA      -> 0xa4
  | LI_MULA        -> 0xa5
  | LI_DIVA        -> 0xa6
  | LI_MODA        -> 0xa7
  | LI_ANDA        -> 0xa8
  | LI_ORA         -> 0xa9
  | LI_XORA        -> 0xaa
  | LI_LSHIFTA     -> 0xab
  | LI_RSHIFTA     -> 0xac
  | LI_INC         -> 0xad
  | LI_DEC         -> 0xae
  | A_FIND         -> 0xaf
  | A_REVERSE      -> 0xb0

  | SH_SR_ASSIGN                      -> 0xb1
  | SH_MEM_ASSIGN_LOCAL               -> 0xb2
  | A_NUMOF_GLOB_1                    -> 0xb3
  | A_NUMOF_STRUCT_1                  -> 0xb4
  | SH_MEM_ASSIGN_IMM                 -> 0xb5
  | SH_LOCALREFREF                    -> 0xb6
  | SH_LOCALASSIGN_SUB_IMM            -> 0xb7
  | SH_IF_LOC_LT_IMM                  -> 0xb8
  | SH_IF_LOC_GE_IMM                  -> 0xb9
  | SH_LOCREF_ASSIGN_MEM              -> 0xba
  | PAGE_REF                          -> 0xbb
  | SH_GLOBAL_ASSIGN_LOCAL            -> 0xbc
  | SH_STRUCTREF_GT_IMM               -> 0xbd
  | SH_STRUCT_ASSIGN_LOCALREF_ITOB    -> 0xbe
  | SH_LOCAL_ASSIGN_STRUCTREF         -> 0xbf
  | SH_IF_STRUCTREF_NE_LOCALREF       -> 0xc0
  | SH_IF_STRUCTREF_GT_IMM            -> 0xc1
  | SH_STRUCTREF_CALLMETHOD_NO_PARAM  -> 0xc2
  | SH_STRUCTREF2                     -> 0xc3
  | SH_REF_STRUCTREF2                 -> 0xc4
  | SH_STRUCTREF3                     -> 0xc5
  | SH_STRUCTREF2_CALLMETHOD_NO_PARAM -> 0xc6
  | SH_IF_STRUCTREF_Z                 -> 0xc7
  | SH_IF_STRUCT_A_NOT_EMPTY          -> 0xc8
  | SH_IF_LOC_GT_IMM                  -> 0xc9
  | SH_IF_STRUCTREF_NE_IMM            -> 0xca
  | THISCALLMETHOD_NOPARAM            -> 0xcb
  | SH_IF_LOC_NE_IMM                  -> 0xcc
  | SH_IF_STRUCTREF_EQ_IMM            -> 0xcd
  | SH_GLOBAL_ASSIGN_IMM              -> 0xce
  | SH_LOCALSTRUCT_ASSIGN_IMM         -> 0xcf
  | SH_STRUCT_A_PUSHBACK_LOCAL_STRUCT -> 0xd0
  | SH_GLOBAL_A_PUSHBACK_LOCAL_STRUCT -> 0xd1
  | SH_LOCAL_A_PUSHBACK_LOCAL_STRUCT  -> 0xd2
  | SH_IF_SREF_NE_STR0                -> 0xd3
  | SH_S_ASSIGN_REF                   -> 0xd4
  | SH_A_FIND_SREF                    -> 0xd5
  | SH_SREF_EMPTY                     -> 0xd6
  | SH_STRUCTSREF_EQ_LOCALSREF        -> 0xd7
  | SH_LOCALSREF_EQ_STR0              -> 0xd8
  | SH_STRUCTSREF_NE_LOCALSREF        -> 0xd9
  | SH_LOCALSREF_NE_STR0              -> 0xda
  | SH_STRUCT_SR_REF                  -> 0xdb
  | SH_STRUCT_S_REF                   -> 0xdc
  | S_REF2                            -> 0xdd
  | SH_REF_LOCAL_ASSIGN_STRUCTREF2    -> 0xde
  | SH_GLOBAL_S_REF                   -> 0xdf
  | SH_LOCAL_S_REF                    -> 0xe0
  | SH_LOCALREF_SASSIGN_LOCALSREF     -> 0xe1
  | SH_LOCAL_APUSHBACK_LOCALSREF      -> 0xe2
  | SH_S_ASSIGN_CALLSYS19             -> 0xe3
  | SH_S_ASSIGN_STR0                  -> 0xe4
  | SH_SASSIGN_LOCALSREF              -> 0xe5
  | SH_STRUCTREF_SASSIGN_LOCALSREF    -> 0xe6
  | SH_LOCALSREF_EMPTY                -> 0xe7
  | SH_GLOBAL_APUSHBACK_LOCALSREF     -> 0xe8
  | SH_STRUCT_APUSHBACK_LOCALSREF     -> 0xe9
  | SH_STRUCTSREF_EMPTY               -> 0xea
  | SH_GLOBALSREF_EMPTY               -> 0xeb
  | SH_SASSIGN_STRUCTSREF             -> 0xec
  | SH_SASSIGN_GLOBALSREF             -> 0xed
  | SH_STRUCTSREF_NE_STR0             -> 0xee
  | SH_GLOBALSREF_NE_STR0             -> 0xef
  | SH_LOC_LT_IMM_OR_LOC_GE_IMM       -> 0xf0

  | A_SORT_MEM         -> 0xf1
  | DG_ADD             -> 0xf2
  | DG_SET             -> 0xf3
  | DG_CALL            -> 0xf4
  | DG_NUMOF           -> 0xf5
  | DG_EXIST           -> 0xf6
  | DG_ERASE           -> 0xf7
  | DG_CLEAR           -> 0xf8
  | DG_COPY            -> 0xf9
  | DG_ASSIGN          -> 0xfa
  | DG_PLUSA           -> 0xfb
  | DG_POP             -> 0xfc
  | DG_NEW_FROM_METHOD -> 0xfd
  | DG_MINUSA          -> 0xfe
  | DG_CALLBEGIN       -> 0xff
  | DG_NEW             -> 0x100
  | DG_STR_TO_METHOD   -> 0x101

  | OP_0X102 -> 0x102
  | X_GETENV -> 0x103
  | X_SET    -> 0x104
  | X_ICAST  -> 0x105
  | X_OP_SET -> 0x106
  | OP_0X107 -> 0x107
  | OP_0X108 -> 0x108
  | OP_0X109 -> 0x109
  | X_DUP    -> 0x10A
  | X_MOV    -> 0x10B
  | X_REF    -> 0x10C
  | X_ASSIGN -> 0x10D
  | X_A_INIT -> 0x10E
  | X_A_SIZE -> 0x10F
  | X_TO_STR -> 0x110

let opcode_of_int = function
  | 0x00  -> PUSH
  | 0x01  -> POP
  | 0x02  -> REF
  | 0x03  -> REFREF
  | 0x04  -> PUSHGLOBALPAGE
  | 0x05  -> PUSHLOCALPAGE
  | 0x06  -> INV
  | 0x07  -> NOT
  | 0x08  -> COMPL
  | 0x09  -> ADD
  | 0x0a  -> SUB
  | 0x0b  -> MUL
  | 0x0c  -> DIV
  | 0x0d  -> MOD
  | 0x0e  -> AND
  | 0x0f  -> OR
  | 0x10  -> XOR
  | 0x11  -> LSHIFT
  | 0x12  -> RSHIFT
  | 0x13  -> LT
  | 0x14  -> GT
  | 0x15  -> LTE
  | 0x16  -> GTE
  | 0x17  -> NOTE
  | 0x18  -> EQUALE
  | 0x19  -> ASSIGN
  | 0x1a  -> PLUSA
  | 0x1b  -> MINUSA
  | 0x1c  -> MULA
  | 0x1d  -> DIVA
  | 0x1e  -> MODA
  | 0x1f  -> ANDA
  | 0x20  -> ORA
  | 0x21  -> XORA
  | 0x22  -> LSHIFTA
  | 0x23  -> RSHIFTA
  | 0x24  -> F_ASSIGN
  | 0x25  -> F_PLUSA
  | 0x26  -> F_MINUSA
  | 0x27  -> F_MULA
  | 0x28  -> F_DIVA
  | 0x29  -> DUP2
  | 0x2a  -> DUP_X2
  | 0x2b  -> CMP
  | 0x2c  -> JUMP
  | 0x2d  -> IFZ
  | 0x2e  -> IFNZ
  | 0x2f  -> RETURN
  | 0x30  -> CALLFUNC
  | 0x31  -> INC
  | 0x32  -> DEC
  | 0x33  -> FTOI
  | 0x34  -> ITOF
  | 0x35  -> F_INV
  | 0x36  -> F_ADD
  | 0x37  -> F_SUB
  | 0x38  -> F_MUL
  | 0x39  -> F_DIV
  | 0x3a  -> F_LT
  | 0x3b  -> F_GT
  | 0x3c  -> F_LTE
  | 0x3D  -> F_GTE
  | 0x3E  -> F_NOTE
  | 0x3f  -> F_EQUALE
  | 0x40  -> F_PUSH
  | 0x41  -> S_PUSH
  | 0x42  -> S_POP
  | 0x43  -> S_ADD
  | 0x44  -> S_ASSIGN
  | 0x45  -> S_PLUSA
  | 0x46  -> S_REF
  | 0x47  -> S_REFREF
  | 0x48  -> S_NOTE
  | 0x49  -> S_EQUALE
  | 0x4A  -> SF_CREATE
  | 0x4B  -> SF_CREATEPIXEL
  | 0x4C  -> SF_CREATEALPHA
  | 0x4d  -> SR_POP
  | 0x4e  -> SR_ASSIGN
  | 0x4f  -> SR_REF
  | 0x50  -> SR_REFREF
  | 0x51  -> A_ALLOC
  | 0x52  -> A_REALLOC
  | 0x53  -> A_FREE
  | 0x54  -> A_NUMOF
  | 0x55  -> A_COPY
  | 0x56  -> A_FILL
  | 0x57  -> C_REF
  | 0x58  -> C_ASSIGN
  | 0x59  -> MSG
  | 0x5a  -> CALLHLL
  | 0x5b  -> PUSHSTRUCTPAGE
  | 0x5c  -> CALLMETHOD
  | 0x5d  -> SH_GLOBALREF
  | 0x5e  -> SH_LOCALREF
  | 0x5f  -> SWITCH
  | 0x60  -> STRSWITCH
  | 0x61  -> FUNC
  | 0x62  -> EOF
  | 0x63  -> CALLSYS
  | 0x64  -> SJUMP
  | 0x65  -> CALLONJUMP
  | 0x66  -> SWAP
  | 0x67  -> SH_STRUCTREF
  | 0x68  -> S_LENGTH
  | 0x69  -> S_LENGTHBYTE
  | 0x6a  -> I_STRING
  | 0x6b  -> CALLFUNC2
  | 0x6c  -> DUP2_X1
  | 0x6d  -> R_ASSIGN
  | 0x6e  -> FT_ASSIGNS
  | 0x6f  -> ASSERT
  | 0x70  -> S_LT
  | 0x71  -> S_GT
  | 0x72  -> S_LTE
  | 0x73  -> S_GTE
  | 0x74  -> S_LENGTH2
  | 0x75  -> S_LENGTHBYTE2
  | 0x76  -> NEW
  | 0x77  -> DELETE
  | 0x78  -> CHECKUDO
  | 0x79  -> A_REF
  | 0x7a  -> DUP
  | 0x7b  -> DUP_U2
  | 0x7c  -> SP_INC
  | 0x7d  -> SP_DEC
  | 0x7e  -> ENDFUNC
  | 0x7f  -> R_EQUALE
  | 0x80  -> R_NOTE
  | 0x81  -> SH_LOCALCREATE
  | 0x82  -> SH_LOCALDELETE
  | 0x83  -> STOI
  | 0x84  -> A_PUSHBACK
  | 0x85  -> A_POPBACK
  | 0x86  -> S_EMPTY
  | 0x87  -> A_EMPTY
  | 0x88  -> A_ERASE
  | 0x89  -> A_INSERT
  | 0x8a  -> SH_LOCALINC
  | 0x8b  -> SH_LOCALDEC
  | 0x8c  -> SH_LOCALASSIGN
  | 0x8d  -> ITOB
  | 0x8e  -> S_FIND
  | 0x8f  -> S_GETPART
  | 0x90  -> A_SORT
  | 0x91  -> S_PUSHBACK
  | 0x92  -> S_POPBACK
  | 0x93  -> FTOS
  | 0x94  -> S_MOD
  | 0x95  -> S_PLUSA2
  | 0x96  -> OBJSWAP
  | 0x97  -> S_ERASE
  | 0x98  -> SR_REF2
  | 0x99  -> S_ERASE2
  | 0x9A  -> S_PUSHBACK2
  | 0x9B  -> S_POPBACK2
  | 0x9c  -> ITOLI
  | 0x9d  -> LI_ADD
  | 0x9e  -> LI_SUB
  | 0x9f  -> LI_MUL
  | 0xa0  -> LI_DIV
  | 0xa1  -> LI_MOD
  | 0xa2  -> LI_ASSIGN
  | 0xa3  -> LI_PLUSA
  | 0xa4  -> LI_MINUSA
  | 0xa5  -> LI_MULA
  | 0xa6  -> LI_DIVA
  | 0xa7  -> LI_MODA
  | 0xa8  -> LI_ANDA
  | 0xa9  -> LI_ORA
  | 0xaa  -> LI_XORA
  | 0xab  -> LI_LSHIFTA
  | 0xac  -> LI_RSHIFTA
  | 0xad  -> LI_INC
  | 0xae  -> LI_DEC
  | 0xaf  -> A_FIND
  | 0xb0  -> A_REVERSE
  | 0xb1  -> SH_SR_ASSIGN
  | 0xb2  -> SH_MEM_ASSIGN_LOCAL
  | 0xb3  -> A_NUMOF_GLOB_1
  | 0xb4  -> A_NUMOF_STRUCT_1
  | 0xb5  -> SH_MEM_ASSIGN_IMM
  | 0xb6  -> SH_LOCALREFREF
  | 0xb7  -> SH_LOCALASSIGN_SUB_IMM
  | 0xb8  -> SH_IF_LOC_LT_IMM
  | 0xb9  -> SH_IF_LOC_GE_IMM
  | 0xba  -> SH_LOCREF_ASSIGN_MEM
  | 0xbb  -> PAGE_REF
  | 0xbc  -> SH_GLOBAL_ASSIGN_LOCAL
  | 0xbd  -> SH_STRUCTREF_GT_IMM
  | 0xbe  -> SH_STRUCT_ASSIGN_LOCALREF_ITOB
  | 0xbf  -> SH_LOCAL_ASSIGN_STRUCTREF
  | 0xc0  -> SH_IF_STRUCTREF_NE_LOCALREF
  | 0xc1  -> SH_IF_STRUCTREF_GT_IMM
  | 0xc2  -> SH_STRUCTREF_CALLMETHOD_NO_PARAM
  | 0xc3  -> SH_STRUCTREF2
  | 0xc4  -> SH_REF_STRUCTREF2
  | 0xc5  -> SH_STRUCTREF3
  | 0xc6  -> SH_STRUCTREF2_CALLMETHOD_NO_PARAM
  | 0xc7  -> SH_IF_STRUCTREF_Z
  | 0xc8  -> SH_IF_STRUCT_A_NOT_EMPTY
  | 0xc9  -> SH_IF_LOC_GT_IMM
  | 0xca  -> SH_IF_STRUCTREF_NE_IMM
  | 0xcb  -> THISCALLMETHOD_NOPARAM
  | 0xcc  -> SH_IF_LOC_NE_IMM
  | 0xcd  -> SH_IF_STRUCTREF_EQ_IMM
  | 0xce  -> SH_GLOBAL_ASSIGN_IMM
  | 0xcf  -> SH_LOCALSTRUCT_ASSIGN_IMM
  | 0xd0  -> SH_STRUCT_A_PUSHBACK_LOCAL_STRUCT
  | 0xd1  -> SH_GLOBAL_A_PUSHBACK_LOCAL_STRUCT
  | 0xd2  -> SH_LOCAL_A_PUSHBACK_LOCAL_STRUCT
  | 0xd3  -> SH_IF_SREF_NE_STR0
  | 0xd4  -> SH_S_ASSIGN_REF
  | 0xd5  -> SH_A_FIND_SREF
  | 0xd6  -> SH_SREF_EMPTY
  | 0xd7  -> SH_STRUCTSREF_EQ_LOCALSREF
  | 0xd8  -> SH_LOCALSREF_EQ_STR0
  | 0xd9  -> SH_STRUCTSREF_NE_LOCALSREF
  | 0xda  -> SH_LOCALSREF_NE_STR0
  | 0xdb  -> SH_STRUCT_SR_REF
  | 0xdc  -> SH_STRUCT_S_REF
  | 0xdd  -> S_REF2
  | 0xde  -> SH_REF_LOCAL_ASSIGN_STRUCTREF2
  | 0xdf  -> SH_GLOBAL_S_REF
  | 0xe0  -> SH_LOCAL_S_REF
  | 0xe1  -> SH_LOCALREF_SASSIGN_LOCALSREF
  | 0xe2  -> SH_LOCAL_APUSHBACK_LOCALSREF
  | 0xe3  -> SH_S_ASSIGN_CALLSYS19
  | 0xe4  -> SH_S_ASSIGN_STR0
  | 0xe5  -> SH_SASSIGN_LOCALSREF
  | 0xe6  -> SH_STRUCTREF_SASSIGN_LOCALSREF
  | 0xe7  -> SH_LOCALSREF_EMPTY
  | 0xe8  -> SH_GLOBAL_APUSHBACK_LOCALSREF
  | 0xe9  -> SH_STRUCT_APUSHBACK_LOCALSREF
  | 0xea  -> SH_STRUCTSREF_EMPTY
  | 0xeb  -> SH_GLOBALSREF_EMPTY
  | 0xec  -> SH_SASSIGN_STRUCTSREF
  | 0xed  -> SH_SASSIGN_GLOBALSREF
  | 0xee  -> SH_STRUCTSREF_NE_STR0
  | 0xef  -> SH_GLOBALSREF_NE_STR0
  | 0xf0  -> SH_LOC_LT_IMM_OR_LOC_GE_IMM
  | 0xf1  -> A_SORT_MEM
  | 0xf2  -> DG_ADD
  | 0xf3  -> DG_SET
  | 0xf4  -> DG_CALL
  | 0xf5  -> DG_NUMOF
  | 0xf6  -> DG_EXIST
  | 0xf7  -> DG_ERASE
  | 0xf8  -> DG_CLEAR
  | 0xf9  -> DG_COPY
  | 0xfa  -> DG_ASSIGN
  | 0xfb  -> DG_PLUSA
  | 0xfc  -> DG_POP
  | 0xfd  -> DG_NEW_FROM_METHOD
  | 0xfe  -> DG_MINUSA
  | 0xff  -> DG_CALLBEGIN
  | 0x100 -> DG_NEW
  | 0x101 -> DG_STR_TO_METHOD
  | 0x102 -> OP_0X102
  | 0x103 -> X_GETENV
  | 0x104 -> X_SET
  | 0x105 -> X_ICAST
  | 0x106 -> X_OP_SET
  | 0x107 -> OP_0X107
  | 0x108 -> OP_0X108
  | 0x109 -> OP_0X109
  | 0x10A -> X_DUP
  | 0x10B -> X_MOV
  | 0x10C -> X_REF
  | 0x10D -> X_ASSIGN
  | 0x10E -> X_A_INIT
  | 0x10F -> X_A_SIZE
  | 0x110 -> X_TO_STR
  | _     -> failwith "invalid opcode"

let string_of_opcode = function
  | PUSH           -> "PUSH"
  | POP            -> "POP"
  | REF            -> "REF"
  | REFREF         -> "REFREF"
  | PUSHGLOBALPAGE -> "PUSHGLOBALPAGE"
  | PUSHLOCALPAGE  -> "PUSHLOCALPAGE"
  | INV            -> "INV"
  | NOT            -> "NOT"
  | COMPL          -> "COMPL"
  | ADD            -> "ADD"
  | SUB            -> "SUB"
  | MUL            -> "MUL"
  | DIV            -> "DIV"
  | MOD            -> "MOD"
  | AND            -> "AND"
  | OR             -> "OR"
  | XOR            -> "XOR"
  | LSHIFT         -> "LSHIFT"
  | RSHIFT         -> "RSHIFT"
  | LT             -> "LT"
  | GT             -> "GT"
  | LTE            -> "LTE"
  | GTE            -> "GTE"
  | NOTE           -> "NOTE"
  | EQUALE         -> "EQUALE"
  | ASSIGN         -> "ASSIGN"
  | PLUSA          -> "PLUSA"
  | MINUSA         -> "MINUSA"
  | MULA           -> "MULA"
  | DIVA           -> "DIVA"
  | MODA           -> "MODA"
  | ANDA           -> "ANDA"
  | ORA            -> "ORA"
  | XORA           -> "XORA"
  | LSHIFTA        -> "LSHIFTA"
  | RSHIFTA        -> "RSHIFTA"
  | F_ASSIGN       -> "F_ASSIGN"
  | F_PLUSA        -> "F_PLUSA"
  | F_MINUSA       -> "F_MINUSA"
  | F_MULA         -> "F_MULA"
  | F_DIVA         -> "F_DIVA"
  | DUP2           -> "DUP2"
  | DUP_X2         -> "DUP_X2"
  | CMP            -> "CMP"
  | JUMP           -> "JUMP"
  | IFZ            -> "IFZ"
  | IFNZ           -> "IFNZ"
  | RETURN         -> "RETURN"
  | CALLFUNC       -> "CALLFUNC"
  | INC            -> "INC"
  | DEC            -> "DEC"
  | FTOI           -> "FTOI"
  | ITOF           -> "ITOF"
  | F_INV          -> "F_INV"
  | F_ADD          -> "F_ADD"
  | F_SUB          -> "F_SUB"
  | F_MUL          -> "F_MUL"
  | F_DIV          -> "F_DIV"
  | F_LT           -> "F_LT"
  | F_GT           -> "F_GT"
  | F_LTE          -> "F_LTE"
  | F_GTE          -> "F_GTE"
  | F_NOTE         -> "F_NOTE"
  | F_EQUALE       -> "F_EQUALE"
  | F_PUSH         -> "F_PUSH"
  | S_PUSH         -> "S_PUSH"
  | S_POP          -> "S_POP"
  | S_ADD          -> "S_ADD"
  | S_ASSIGN       -> "S_ASSIGN"
  | S_PLUSA        -> "S_PLUSA"
  | S_REF          -> "S_REF"
  | S_REFREF       -> "S_REFREF"
  | S_NOTE         -> "S_NOTE"
  | S_EQUALE       -> "S_EQUALE"
  | SF_CREATE      -> "SF_CREATE"
  | SF_CREATEPIXEL -> "SF_CREATEPIXEL"
  | SF_CREATEALPHA -> "SF_CREATEALPHA"
  | SR_POP         -> "SR_POP"
  | SR_ASSIGN      -> "SR_ASSIGN"
  | SR_REF         -> "SR_REF"
  | SR_REFREF      -> "SR_REFREF"
  | A_ALLOC        -> "A_ALLOC"
  | A_REALLOC      -> "A_REALLOC"
  | A_FREE         -> "A_FREE"
  | A_NUMOF        -> "A_NUMOF"
  | A_COPY         -> "A_COPY"
  | A_FILL         -> "A_FILL"
  | C_REF          -> "C_REF"
  | C_ASSIGN       -> "C_ASSIGN"
  | MSG            -> "MSG"
  | CALLHLL        -> "CALLHLL"
  | PUSHSTRUCTPAGE -> "PUSHSTRUCTPAGE"
  | CALLMETHOD     -> "CALLMETHOD"
  | SH_GLOBALREF   -> "SH_GLOBALREF"
  | SH_LOCALREF    -> "SH_LOCALREF"
  | SWITCH         -> "SWITCH"
  | STRSWITCH      -> "STRSWITCH"
  | FUNC           -> "FUNC"
  | EOF            -> "EOF"
  | CALLSYS        -> "CALLSYS"
  | SJUMP          -> "SJUMP"
  | CALLONJUMP     -> "CALLONJUMP"
  | SWAP           -> "SWAP"
  | SH_STRUCTREF   -> "SH_STRUCTREF"
  | S_LENGTH       -> "S_LENGTH"
  | S_LENGTHBYTE   -> "S_LENGTHBYTE"
  | I_STRING       -> "I_STRING"
  | CALLFUNC2      -> "CALLFUNC2"
  | DUP2_X1        -> "DUP2_X1"
  | R_ASSIGN       -> "R_ASSIGN"
  | FT_ASSIGNS     -> "FT_ASSIGNS"
  | ASSERT         -> "ASSERT"
  | S_LT           -> "S_LT"
  | S_GT           -> "S_GT"
  | S_LTE          -> "S_LTE"
  | S_GTE          -> "S_GTE"
  | S_LENGTH2      -> "S_LENGTH2"
  | S_LENGTHBYTE2  -> "S_LENGTHBYTE2"
  | NEW            -> "NEW"
  | DELETE         -> "DELETE"
  | CHECKUDO       -> "CHECKUDO"
  | A_REF          -> "A_REF"
  | DUP            -> "DUP"
  | DUP_U2         -> "DUP_U2"
  | SP_INC         -> "SP_INC"
  | SP_DEC         -> "SP_DEC"
  | ENDFUNC        -> "ENDFUNC"
  | R_EQUALE       -> "R_EQUALE"
  | R_NOTE         -> "R_NOTE"
  | SH_LOCALCREATE -> "SH_LOCALCREATE"
  | SH_LOCALDELETE -> "SH_LOCALDELETE"
  | STOI           -> "STOI"
  | A_PUSHBACK     -> "A_PUSHBACK"
  | A_POPBACK      -> "A_POPBACK"
  | S_EMPTY        -> "S_EMPTY"
  | A_EMPTY        -> "A_EMPTY"
  | A_ERASE        -> "A_ERASE"
  | A_INSERT       -> "A_INSERT"
  | SH_LOCALINC    -> "SH_LOCALINC"
  | SH_LOCALDEC    -> "SH_LOCALDEC"
  | SH_LOCALASSIGN -> "SH_LOCALASSIGN"
  | ITOB           -> "ITOB"
  | S_FIND         -> "S_FIND"
  | S_GETPART      -> "S_GETPART"
  | A_SORT         -> "A_SORT"
  | S_PUSHBACK     -> "S_PUSHBACK"
  | S_POPBACK      -> "S_POPBACK"
  | FTOS           -> "FTOS"
  | S_MOD          -> "S_MOD"
  | S_PLUSA2       -> "S_PLUSA2"
  | OBJSWAP        -> "OBJSWAP"
  | S_ERASE        -> "S_ERASE"
  | SR_REF2        -> "SR_REF2"
  | S_ERASE2       -> "S_ERASE2"
  | S_PUSHBACK2    -> "S_PUSHBACK2"
  | S_POPBACK2     -> "S_POPBACK2"
  | ITOLI          -> "ITOLI"
  | LI_ADD         -> "LI_ADD"
  | LI_SUB         -> "LI_SUB"
  | LI_MUL         -> "LI_MUL"
  | LI_DIV         -> "LI_DIV"
  | LI_MOD         -> "LI_MOD"
  | LI_ASSIGN      -> "LI_ASSIGN"
  | LI_PLUSA       -> "LI_PLUSA"
  | LI_MINUSA      -> "LI_MINUSA"
  | LI_MULA        -> "LI_MULA"
  | LI_DIVA        -> "LI_DIVA"
  | LI_MODA        -> "LI_MODA"
  | LI_ANDA        -> "LI_ANDA"
  | LI_ORA         -> "LI_ORA"
  | LI_XORA        -> "LI_XORA"
  | LI_LSHIFTA     -> "LI_LSHIFTA"
  | LI_RSHIFTA     -> "LI_RSHIFTA"
  | LI_INC         -> "LI_INC"
  | LI_DEC         -> "LI_DEC"
  | A_FIND         -> "A_FIND"
  | A_REVERSE      -> "A_REVERSE"

  | SH_SR_ASSIGN                      -> "SH_SR_ASSIGN"
  | SH_MEM_ASSIGN_LOCAL               -> "SH_MEM_ASSIGN_LOCAL"
  | A_NUMOF_GLOB_1                    -> "A_NUMOF_GLOB_1"
  | A_NUMOF_STRUCT_1                  -> "A_NUMOF_STRUCT_1"
  | SH_MEM_ASSIGN_IMM                 -> "SH_MEM_ASSIGN_IMM"
  | SH_LOCALREFREF                    -> "SH_LOCALREFREF"
  | SH_LOCALASSIGN_SUB_IMM            -> "SH_LOCALASSIGN_SUB_IMM"
  | SH_IF_LOC_LT_IMM                  -> "SH_IF_LOC_LT_IMM"
  | SH_IF_LOC_GE_IMM                  -> "SH_IF_LOC_GE_IMM"
  | SH_LOCREF_ASSIGN_MEM              -> "SH_LOCREF_ASSIGN_MEM"
  | PAGE_REF                          -> "PAGE_REF"
  | SH_GLOBAL_ASSIGN_LOCAL            -> "SH_GLOBAL_ASSIGN_LOCAL"
  | SH_STRUCTREF_GT_IMM               -> "SH_STRUCTREF_GT_IMM"
  | SH_STRUCT_ASSIGN_LOCALREF_ITOB    -> "SH_STRUCT_ASSIGN_LOCALREF_ITOB"
  | SH_LOCAL_ASSIGN_STRUCTREF         -> "SH_LOCAL_ASSIGN_STRUCTREF"
  | SH_IF_STRUCTREF_NE_LOCALREF       -> "SH_IF_STRUCTREF_NE_LOCALREF"
  | SH_IF_STRUCTREF_GT_IMM            -> "SH_IF_STRUCTREF_GT_IMM"
  | SH_STRUCTREF_CALLMETHOD_NO_PARAM  -> "SH_STRUCTREF_CALLMETHOD_NO_PARAM"
  | SH_STRUCTREF2                     -> "SH_STRUCTREF2"
  | SH_REF_STRUCTREF2                 -> "SH_REF_STRUCTREF2"
  | SH_STRUCTREF3                     -> "SH_STRUCTREF3"
  | SH_STRUCTREF2_CALLMETHOD_NO_PARAM -> "SH_STRUCTREF2_CALLMETHOD_NO_PARAM"
  | SH_IF_STRUCTREF_Z                 -> "SH_IF_STRUCTREF_Z"
  | SH_IF_STRUCT_A_NOT_EMPTY          -> "SH_IF_STRUCT_A_NOT_EMPTY"
  | SH_IF_LOC_GT_IMM                  -> "SH_IF_LOC_GT_IMM"
  | SH_IF_STRUCTREF_NE_IMM            -> "SH_IF_STRUCTREF_NE_IMM"
  | THISCALLMETHOD_NOPARAM            -> "THISCALLMETHOD_NOPARAM"
  | SH_IF_LOC_NE_IMM                  -> "SH_IF_LOC_NE_IMM"
  | SH_IF_STRUCTREF_EQ_IMM            -> "SH_IF_STRUCTREF_EQ_IMM"
  | SH_GLOBAL_ASSIGN_IMM              -> "SH_GLOBAL_ASSIGN_IMM"
  | SH_LOCALSTRUCT_ASSIGN_IMM         -> "SH_LOCALSTRUCT_ASSIGN_IMM"
  | SH_STRUCT_A_PUSHBACK_LOCAL_STRUCT -> "SH_STRUCT_A_PUSHBACK_LOCAL_STRUCT"
  | SH_GLOBAL_A_PUSHBACK_LOCAL_STRUCT -> "SH_GLOBAL_A_PUSHBACK_LOCAL_STRUCT"
  | SH_LOCAL_A_PUSHBACK_LOCAL_STRUCT  -> "SH_LOCAL_A_PUSHBACK_LOCAL_STRUCT"
  | SH_IF_SREF_NE_STR0                -> "SH_IF_SREF_NE_STR0"
  | SH_S_ASSIGN_REF                   -> "SH_S_ASSIGN_REF"
  | SH_A_FIND_SREF                    -> "SH_A_FIND_SREF"
  | SH_SREF_EMPTY                     -> "SH_SREF_EMPTY"
  | SH_STRUCTSREF_EQ_LOCALSREF        -> "SH_STRUCTSREF_EQ_LOCALSREF"
  | SH_LOCALSREF_EQ_STR0              -> "SH_LOCALSREF_EQ_STR0"
  | SH_STRUCTSREF_NE_LOCALSREF        -> "SH_STRUCTSREF_NE_LOCALSREF"
  | SH_LOCALSREF_NE_STR0              -> "SH_LOCALSREF_NE_STR0"
  | SH_STRUCT_SR_REF                  -> "SH_STRUCT_SR_REF"
  | SH_STRUCT_S_REF                   -> "SH_STRUCT_S_REF"
  | S_REF2                            -> "S_REF2"
  | SH_REF_LOCAL_ASSIGN_STRUCTREF2    -> "SH_REF_LOCAL_ASSIGN_STRUCTREF2"
  | SH_GLOBAL_S_REF                   -> "SH_GLOBAL_S_REF"
  | SH_LOCAL_S_REF                    -> "SH_LOCAL_S_REF"
  | SH_LOCALREF_SASSIGN_LOCALSREF     -> "SH_LOCALREF_SASSIGN_LOCALSREF"
  | SH_LOCAL_APUSHBACK_LOCALSREF      -> "SH_LOCAL_APUSHBACK_LOCALSREF"
  | SH_S_ASSIGN_CALLSYS19             -> "SH_S_ASSIGN_CALLSYS19"
  | SH_S_ASSIGN_STR0                  -> "SH_S_ASSIGN_STR0"
  | SH_SASSIGN_LOCALSREF              -> "SH_SASSIGN_LOCALSREF"
  | SH_STRUCTREF_SASSIGN_LOCALSREF    -> "SH_STRUCTREF_SASSIGN_LOCALSREF"
  | SH_LOCALSREF_EMPTY                -> "SH_LOCALSREF_EMPTY"
  | SH_GLOBAL_APUSHBACK_LOCALSREF     -> "SH_GLOBAL_APUSHBACK_LOCALSREF"
  | SH_STRUCT_APUSHBACK_LOCALSREF     -> "SH_STRUCT_APUSHBACK_LOCALSREF"
  | SH_STRUCTSREF_EMPTY               -> "SH_STRUCTSREF_EMPTY"
  | SH_GLOBALSREF_EMPTY               -> "SH_GLOBALSREF_EMPTY"
  | SH_SASSIGN_STRUCTSREF             -> "SH_SASSIGN_STRUCTSREF"
  | SH_SASSIGN_GLOBALSREF             -> "SH_SASSIGN_GLOBALSREF"
  | SH_STRUCTSREF_NE_STR0             -> "SH_STRUCTSREF_NE_STR0"
  | SH_GLOBALSREF_NE_STR0             -> "SH_GLOBALSREF_NE_STR0"
  | SH_LOC_LT_IMM_OR_LOC_GE_IMM       -> "SH_LOC_LT_IMM_OR_LOC_GE_IMM"

  | A_SORT_MEM         -> "A_SORT_MEM"
  | DG_ADD             -> "DG_ADD"
  | DG_SET             -> "DG_SET"
  | DG_CALL            -> "DG_CALL"
  | DG_NUMOF           -> "DG_NUMOF"
  | DG_EXIST           -> "DG_EXIST"
  | DG_ERASE           -> "DG_ERASE"
  | DG_CLEAR           -> "DG_CLEAR"
  | DG_COPY            -> "DG_COPY"
  | DG_ASSIGN          -> "DG_ASSIGN"
  | DG_PLUSA           -> "DG_PLUSA"
  | DG_POP             -> "DG_POP"
  | DG_NEW_FROM_METHOD -> "DG_NEW_FROM_METHOD"
  | DG_MINUSA          -> "DG_MINUSA"
  | DG_CALLBEGIN       -> "DG_CALLBEGIN"
  | DG_NEW             -> "DG_NEW"
  | DG_STR_TO_METHOD   -> "DG_STR_TO_METHOD"

  | OP_0X102 -> "OP_0X102"
  | X_GETENV -> "X_GETENV"
  | X_SET    -> "X_SET"
  | X_ICAST  -> "X_ICAST"
  | X_OP_SET -> "X_OP_SET"
  | OP_0X107 -> "OP_0X107"
  | OP_0X108 -> "OP_0X108"
  | OP_0X109 -> "OP_0X109"
  | X_DUP    -> "X_DUP"
  | X_MOV    -> "X_MOV"
  | X_REF    -> "X_REF"
  | X_ASSIGN -> "X_ASSIGN"
  | X_A_INIT -> "X_A_INIT"
  | X_A_SIZE -> "X_A_SIZE"
  | X_TO_STR -> "X_TO_STR"

type syscall =
  | Exit
  | GlobalSave
  | GlobalLoad
  | LockPeek
  | UnlockPeek
  | Reset
  | Output
  | MsgBox
  | ResumeSave
  | ResumeLoad
  | ExistsFile
  | OpenWeb
  | GetSaveFolderName
  | GetTime
  | GetGameName
  | Error
  | ExistsSaveFile
  | IsDebugMode
  | MsgBoxOkCancel
  | GetFuncStackName
  | Peek
  | Sleep
  | ResumeWriteComment
  | ResumeReadComment
  | GroupSave
  | GroupLoad
  | DeleteSaveFile
  | ExistFunc
  | CopySaveFile

let int_of_syscall = function
  | Exit               -> 0x00
  | GlobalSave         -> 0x01
  | GlobalLoad         -> 0x02
  | LockPeek           -> 0x03
  | UnlockPeek         -> 0x04
  | Reset              -> 0x05
  | Output             -> 0x06
  | MsgBox             -> 0x07
  | ResumeSave         -> 0x08
  | ResumeLoad         -> 0x09
  | ExistsFile         -> 0x0A
  | OpenWeb            -> 0x0B
  | GetSaveFolderName  -> 0x0C
  | GetTime            -> 0x0D
  | GetGameName        -> 0x0E
  | Error              -> 0x0F
  | ExistsSaveFile     -> 0x10
  | IsDebugMode        -> 0x11
  | MsgBoxOkCancel     -> 0x12
  | GetFuncStackName   -> 0x13
  | Peek               -> 0x14
  | Sleep              -> 0x15
  | ResumeWriteComment -> 0x16
  | ResumeReadComment  -> 0x17
  | GroupSave          -> 0x18
  | GroupLoad          -> 0x19
  | DeleteSaveFile     -> 0x1A
  | ExistFunc          -> 0x1B
  | CopySaveFile       -> 0x1C

let syscall_of_string = function
  | "Exit"               -> Some Exit
  | "GlobalSave"         -> Some GlobalSave
  | "GlobalLoad"         -> Some GlobalLoad
  | "LockPeek"           -> Some LockPeek
  | "UnlockPeek"         -> Some UnlockPeek
  | "Reset"              -> Some Reset
  | "Output"             -> Some Output
  | "MsgBox"             -> Some MsgBox
  | "ResumeSave"         -> Some ResumeSave
  | "ResumeLoad"         -> Some ResumeLoad
  | "ExistsFile"         -> Some ExistsFile
  | "OpenWeb"            -> Some OpenWeb
  | "GetSaveFolderName"  -> Some GetSaveFolderName
  | "GetTime"            -> Some GetTime
  | "GetGameName"        -> Some GetGameName
  | "Error"              -> Some Error
  | "ExistsSaveFile"     -> Some ExistsSaveFile
  | "IsDebugMode"        -> Some IsDebugMode
  | "MsgBoxOkCancel"     -> Some MsgBoxOkCancel
  | "GetFuncStackName"   -> Some GetFuncStackName
  | "Peek"               -> Some Peek
  | "Sleep"              -> Some Sleep
  | "ResumeWriteComment" -> Some ResumeWriteComment
  | "ResumeReadComment"  -> Some ResumeReadComment
  | "GroupSave"          -> Some GroupSave
  | "GroupLoad"          -> Some GroupLoad
  | "DeleteSaveFile"     -> Some DeleteSaveFile
  | "ExistFunc"          -> Some ExistFunc
  | "CopySaveFile"       -> Some CopySaveFile
  | _ -> None

let function_of_syscall sys =
  let t_void = Alice.Ain.Type.make Void in
  let t_int = Alice.Ain.Type.make Int in
  let t_string = Alice.Ain.Type.make String in
  let t_bool = Alice.Ain.Type.make Bool in
  let t_ref_int = Alice.Ain.Type.make ~is_ref:true Int in
  let t_ref_array_string = Alice.Ain.Type.make ~is_ref:true (Array t_string) in
  let make_vars (types:Alice.Ain.Type.t list) =
    let make_var t i =
      let (r:Alice.Ain.Variable.t) =
        { index = i;
          name = "";
          name2 = Some "";
          value_type = t;
          initval = None;
          group_index = 0;
          var_type = 0
        }
      in
      r
    in
    List.map2_exn types (List.init (List.length types) ~f:(~+)) ~f:make_var
  in
  let (default:Alice.Ain.Function.t) =
    { index       = -1;
      name        = "";
      address     = 0;
      nr_args     = 0;
      vars        = [];
      return_type = t_void;
      is_label    = false;
      is_lambda   = 0;
      crc         = 0;
      struct_type = -1;
      enum_type   = -1
    }
  in
  let make_function i return_type name arg_types =
    { default with
      index = i;
      name = name;
      nr_args = (List.length arg_types);
      vars = make_vars arg_types;
      return_type = return_type
    }
  in
  match sys with
  | Exit               -> make_function 0x00 t_void "Exit" [t_int]
  | GlobalSave         -> make_function 0x01 t_int "GlobalSave" [t_string; t_string]
  | GlobalLoad         -> make_function 0x02 t_int "GlobalLoad" [t_string; t_string]
  | LockPeek           -> make_function 0x03 t_int "LockPeek" []
  | UnlockPeek         -> make_function 0x04 t_int "UnlockPeek" []
  | Reset              -> make_function 0x05 t_void "Reset" []
  | Output             -> make_function 0x06 t_string "Output" [t_string]
  | MsgBox             -> make_function 0x07 t_string "MsgBox" [t_string]
  | ResumeSave         -> make_function 0x08 t_int "ResumeSave" [t_string; t_string; t_ref_int]
  | ResumeLoad         -> make_function 0x09 t_void "ResumeLoad" [t_string; t_string]
  | ExistsFile         -> make_function 0x0A t_int "ExistsFile" [t_string]
  | OpenWeb            -> make_function 0x0B t_void "OpenWeb" [t_string]
  | GetSaveFolderName  -> make_function 0x0C t_string "GetSaveFolderName" []
  | GetTime            -> make_function 0x0D t_int "GetTime" []
  | GetGameName        -> make_function 0x0E t_string "GetGameName" []
  | Error              -> make_function 0x0F t_string "Error" [t_string]
  | ExistsSaveFile     -> make_function 0x10 t_int "ExistsSaveFile" [t_string]
  | IsDebugMode        -> make_function 0x11 t_int "IsDebugMode" []
  | MsgBoxOkCancel     -> make_function 0x12 t_int "MsgBoxOkCancel" [t_string]
  | GetFuncStackName   -> make_function 0x13 t_string "GetFuncStackName" [t_int]
  | Peek               -> make_function 0x14 t_void "Peek" []
  | Sleep              -> make_function 0x15 t_void "Sleep" [t_int]
  | ResumeWriteComment -> make_function 0x16 t_bool "ResumeWriteComment" [t_string; t_string; t_ref_array_string]
  | ResumeReadComment  -> make_function 0x17 t_bool "ResumeReadComment" [t_string; t_string; t_ref_array_string]
  | GroupSave          -> make_function 0x18 t_int "GroupSave" [t_string; t_string; t_string; t_ref_int]
  | GroupLoad          -> make_function 0x19 t_int "GroupLoad" [t_string; t_string; t_string; t_ref_int]
  | DeleteSaveFile     -> make_function 0x1A t_int "DeleteSaveFile" [t_string]
  | ExistFunc          -> make_function 0x1B t_bool "ExistFunc" [t_string]
  | CopySaveFile       -> make_function 0x1C t_int "CopySaveFile" [t_string; t_string]

type builtin =
  | IntString
  | FloatString
  | StringInt
  | StringLength
  | StringLengthByte
  | StringEmpty
  | StringFind
  | StringGetPart
  | StringPushBack
  | StringPopBack
  | StringErase
  | ArrayAlloc
  | ArrayRealloc
  | ArrayFree
  | ArrayNumof
  | ArrayCopy
  | ArrayFill
  | ArrayPushBack
  | ArrayPopBack
  | ArrayEmpty
  | ArrayErase
  | ArrayInsert
  | ArraySort

let int_builtin_of_string = function
  | "String" -> Some IntString
  | _ -> None

let float_builtin_of_string = function
  | "String" -> Some FloatString
  | _ -> None

let string_builtin_of_string = function
  | "Int" -> Some StringInt
  | "Length" -> Some StringLength
  | "LengthByte" -> Some StringLengthByte
  | "Empty" -> Some StringEmpty
  | "Find" -> Some StringFind
  | "GetPart" -> Some StringGetPart
  | "PushBack" -> Some StringPushBack
  | "PopBack" -> Some StringPopBack
  | "Erase" -> Some StringErase
  | _ -> None

let array_builtin_of_string = function
  | "Alloc" -> Some ArrayAlloc
  | "Realloc" -> Some ArrayRealloc
  | "Free" -> Some ArrayFree
  | "Numof" -> Some ArrayNumof
  | "Copy" -> Some ArrayCopy
  | "Fill" -> Some ArrayFill
  | "PushBack" -> Some ArrayPushBack
  | "PopBack" -> Some ArrayPopBack
  | "Empty" -> Some ArrayEmpty
  | "Erase" -> Some ArrayErase
  | "Insert" -> Some ArrayInsert
  | "Sort" -> Some ArraySort
  | _ -> None

let builtin_of_string (t:Alice.Ain.Type.data) name =
  match t with
  | Int -> int_builtin_of_string name
  | Float -> float_builtin_of_string name
  | String -> string_builtin_of_string name
  | Array _ -> array_builtin_of_string name
  | _ -> None

let function_of_builtin builtin =
  let t_void = Alice.Ain.Type.make Void in
  let t_int = Alice.Ain.Type.make Int in
  let t_string = Alice.Ain.Type.make String in
  let t_ref_array = Alice.Ain.Type.make ~is_ref:true (Array t_void) in
  let t_func = Alice.Ain.Type.make ~is_ref:true (Function 0) in
  let (default:Alice.Ain.Function.t) =
    { index       = -1;
      name        = "";
      address     = 0;
      nr_args     = 0;
      vars        = [];
      return_type = t_void;
      is_label    = false;
      is_lambda   = 0;
      crc         = 0;
      struct_type = -1;
      enum_type   = -1
    }
  in
  let make_function return_type name (arg_types:Alice.Ain.Type.t list) =
    let make_var (t:Alice.Ain.Type.t) i =
      let (r:Alice.Ain.Variable.t) =
        { index = i;
          name = "";
          name2 = Some "";
          value_type = t;
          initval = None;
          group_index = 0;
          var_type = 0
        }
      in
      r
    in
    { default with
      name = name;
      nr_args = List.length arg_types;
      vars = List.map2_exn arg_types (List.init (List.length arg_types) ~f:(~+)) ~f:make_var;
      return_type = return_type
    }
  in
  match builtin with
  | IntString        -> make_function t_string "String" []
  | FloatString      -> make_function t_string "String" []
  | StringInt        -> make_function t_int "Int" []
  | StringLength     -> make_function t_int "Length" []
  | StringLengthByte -> make_function t_int "LengthByte" []
  | StringEmpty      -> make_function t_int "Empty" []
  | StringFind       -> make_function t_int "Find" [t_string]
  | StringGetPart    -> make_function t_string "GetPart" [t_int; t_int]
  | StringPushBack   -> make_function t_void "PushBack" [t_int]
  | StringPopBack    -> make_function t_void "PopBack" []
  | StringErase      -> make_function t_void "Erase" [t_int]
  | ArrayAlloc       -> make_function t_void "Alloc" [t_int]
  | ArrayRealloc     -> make_function t_void "Realloc" [t_int]
  | ArrayFree        -> make_function t_void "Free" []
  | ArrayNumof       -> make_function t_int "Numof" []
  | ArrayCopy        -> make_function t_int "Copy" [t_int; t_ref_array; t_int; t_int]
  | ArrayFill        -> make_function t_int "Fill" [t_int; t_int; t_void]
  | ArrayPushBack    -> make_function t_void "PushBack" [t_void]
  | ArrayPopBack     -> make_function t_void "PopBack" []
  | ArrayEmpty       -> make_function t_int "Empty" []
  | ArrayErase       -> make_function t_int "Erase" [t_int]
  | ArrayInsert      -> make_function t_void "Insert" [t_int; t_void]
  | ArraySort        -> make_function t_void "Sort" [t_func]

type argtype =
  | Int
  | Float
  | Address
  | Function
  | String
  | Message
  | Local
  | Global
  | Struct
  | Syscall
  | Library
  | LibraryFunction
  | File
  | Delegate
  | Switch

let argtype_of_int = function
  | 0  -> Int
  | 1  -> Float
  | 2  -> Address
  | 3  -> Function
  | 4  -> String
  | 5  -> Message
  | 6  -> Local
  | 7  -> Global
  | 8  -> Struct
  | 9  -> Syscall
  | 10 -> Library
  | 11 -> LibraryFunction
  | 12 -> File
  | 13 -> Delegate
  | 14 -> Switch
  | _ -> failwith "invalid argument type"
