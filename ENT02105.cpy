      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 2      *
      *----------------------------------------------------------------*
      * NOME DO BOOK: ENT02105                                         *
      * DESCRIÇÃO   : ARQUIVO DE DEPOSITOS EM CONTA CORRENTE           *
      * TAMANHO     : 30 BYTES                                         *
      * AUTORA      : JULIANA SOARES                                   *
      * DATA        : 17/01/2023                                       *
      *----------------------------------------------------------------*
      *                       DADOS DE ENTRADA                         *
      *----------------------------------------------------------------*
      * COD-CPF     : CPF DO CORRENTISTA                               *
      * DIG-CPF     : DIGITO VERIFICADOR DO CPF                        *
      * DAT-DEPOS   : DATA DO DEPOSITO (DD-MM-AA)                      *
      * VAL-DEPOS   : VALOR DO DEPOSITO                                *
      *----------------------------------------------------------------*
      
       01 ARQENT01-REGISTRO.
          03 ARQENT01-CPF.
             05 ARQENT01-COD-CPF        PIC 9(09) VALUE ZEROS.
             05 ARQENT01-DIG-CPF        PIC 9(02) VALUE ZEROS.
          03 ARQENT01-DAT-DEPOS         PIC X(10) VALUE SPACES.
          03 ARQENT01-VAL-DEPOS         PIC S9(15)V99 COMP-3 VALUE +0.