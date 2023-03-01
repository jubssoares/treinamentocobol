      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 3      *
      *----------------------------------------------------------------*
      * NOME DO BOOK: ENT03205                                         *
      * DESCRIÇÃO   : ARQUIVO DE DEPOSITOS EM CONTA CORRENTE           *
      * TAMANHO     : 14 BYTES                                         *
      * AUTORA      : JULIANA SOARES                                   *
      * DATA        : 19/01/2023                                       *
      *----------------------------------------------------------------*
      *                       DADOS DE ENTRADA                         *
      *----------------------------------------------------------------*
      * COD-AGENCIA: CÓDIGO DA AGENCIA                                 *
      * NUM-CONTA  : NUMERO DA CONTA                                   *
      * DAT-PAGTO  : DATA DE PAGAMENTO                                 *
      *----------------------------------------------------------------*

       01 ARQENT02-REGISTRO.                                          
          03 ARQENT02-COD-AGENCIA          PIC S9(03) COMP-3 VALUE +0.
          03 ARQENT02-NUM-CONTA            PIC S9(03) COMP-3 VALUE +0.
          03 ARQENT02-DAT-PAGTO            PIC  9(08)        VALUE 0.