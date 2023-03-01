      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 3      *
      *----------------------------------------------------------------*
      * NOME DO BOOK: SAI03105                                         *
      * DESCRIÇÃO   : ARQUIVO DE DEPOSITOS EM CONTA CORRENTE           *
      * TAMANHO     : 16 BYTES                                         *
      * AUTORA      : JULIANA SOARES                                   *
      * DATA        : 19/01/2023                                       *
      *----------------------------------------------------------------*
      *                       DADOS DE SAIDA                           *
      *----------------------------------------------------------------*
      * COD-AGENCIA: CÓDIGO DA AGENCIA                                 *
      * NUM-CONTA  : NUMERO DA CONTA                                   *
      * DAT-PAGTO  : DATA DE PAGAMENTO                                 *
      *----------------------------------------------------------------*

       01 ARQSAI01-REGISTRO.                                      
          03 ARQSAI01-COD-AGENCIA          PIC 9(03) VALUE ZEROS. 
          03 ARQSAI01-NUM-CONTA            PIC 9(03) VALUE ZEROS. 
          03 ARQSAI01-DAT-PAGTO            PIC X(10) VALUE SPACES.