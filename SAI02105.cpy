      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 2      *
      *----------------------------------------------------------------*
      * NOME DO BOOK: SAI02105                                         *
      * DESCRIÇÃO   : ARQUIVO DE DEPOSITOS EM CONTA CORRENTE           *
      * TAMANHO     : 61 BYTES                                         *
      * AUTORA      : JULIANA SOARES                                   *
      * DATA        : 17/01/2023                                       *
      *----------------------------------------------------------------*
      *                       DADOS DE SAIDA                           *
      *----------------------------------------------------------------*
      * MASK-CPF : MÁSCARA DO CPF
      * MASK-DAT : MÁSCARA DA DATA
      * MASK-VAL : MÁSCARA DO VALOR
      *----------------------------------------------------------------*

       01 ARQSAI01-REGISTRO.
          03 ARQSAI01-MASK-CPF                      PIC 999.999.999.99.
          03 FILLER                                 PIC X VALUE ';'.
          03 ARQSAI01-MASK-DAT                      PIC 99.99.9999.
          03 FILLER                                 PIC X VALUE ';'.          
          03 ARQSAI01-MASK-VAL                      PIC Z.ZZZ.ZZ9,99.