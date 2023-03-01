      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 3      *
      *----------------------------------------------------------------*
      * NOME DO BOOK: SAI03205                                         *
      * DESCRIÇÃO   : ARQUIVO DE DEPOSITOS EM CONTA CORRENTE           *
      * TAMANHO     : 06 BYTES                                         *
      * AUTORA      : JULIANA SOARES                                   *
      * DATA        : 19/01/2023                                       *
      *----------------------------------------------------------------*
      *                       DADOS DE SAIDA                           *
      *----------------------------------------------------------------*
      * COD-AGENCIA: CÓDIGO DA AGENCIA                                 *
      * NUM-CONTA  : NUMERO DA CONTA                                   *
      *----------------------------------------------------------------*

       01 ARQSAI02-REGISTRO.                                      
          03 ARQSAI02-COD-AGENCIA          PIC 9(03) VALUE ZEROS. 
          03 ARQSAI02-NUM-CONTA            PIC 9(03) VALUE ZEROS.     